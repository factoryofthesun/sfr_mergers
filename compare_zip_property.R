# Compare property and zip DHHI trends directly

rm(list=ls())

# Setting ===============================================
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(gridExtra)
library(zoo)
library(ggplot2)
library(openxlsx)
library(readxl)
library(rlist)
library(bit64)
library(argparse)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"

t0 <- Sys.time()
dt <- fread(paste0(data_path, "panel_hhi_rent.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")))
dt_zip <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character")
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# ============= (0) Preprocessing ===================================================
# Zip code characteristics 
acs_zip <- fread("/project/humphries/jeh232/rent_project/data/census/acs_zip_demographics.csv", colClasses = c(zip = "integer", year="integer"))

# Drop 3-digit ZCTAs from 2000
acs_zip[, zip := as.integer(zip)] 
acs_zip <- acs_zip[!is.na(zip)]

acs_zip[,shift_year := fcase(year == 2011, 2007,
                             year == 2012, 2009,
                             year == 2018, 2017,
                             year == 2000, 2000,
                             default = NA)
        ] # Re-code data to so ACS5 covers start-yr to midpoints (e.g. 2007-2009 for 2007-2011 ACS)
acs_zip[is.na(shift_year), shift_year := year-2]
acs_zip <- rbindlist(list(acs_zip, copy(acs_zip)[year %in% c(2011, 2012, 2017, 2018)][, shift_year := shift_year + 1]))

dt <- merge(dt, acs_zip, by.x = c("Zip5", "year"), by.y = c("zip", "shift_year"), all.x = T)

# Zip is factor
dt[,Zip5 := factor(Zip5)]

# Drop low-property count zips 
dt_n <- dt[,.N,.(Zip5,year)]
bad_zips <- dt_n[,.(N = mean_na(N)), .(Zip5)][N < 10, Zip5]
print(paste0("Dropping ", nrow(dt[Zip5 %in% bad_zips]), " rows of zips with under 10 average property observations per year out of ", 
             nrow(dt), " total rows."))
dt <- dt[!(Zip5 %in% bad_zips)]

# Drop outlier rents 
n_bad_rents <- nrow(dt[RentPrice >= 500 & RentPrice <= 5000])
print(paste0("Dropping ", n_bad_rents, " rows with rent higher than 100k and lower than 100."))
dt <- dt[RentPrice < 5000 & RentPrice > 500]

# Define delta hhi 
dt[, zip_firm := paste0(Zip5, Merger_Owner_Name)]
dt[, treated := as.integer(treated)]

dt[,merge_firm_owned := as.integer(!is.na(Merger_Owner_Name) & Merger_Owner_Name != "")]
dt <- dt[year >= 2009 & year <= 2018]

treated_names <- setdiff(grep("^treated_", names(dt), value = T), "treated_overlap")
dt[,treated := 0]
treated_zips <- unique(dt[treated_overlap >= 1, Zip5])
dt[Zip5 %in% treated_zips,treated := 1]

post_names <- grep("^post_", names(dt), value = T)
dt[,post := 0]
dt[,post := as.integer(rowSums(.SD) >= 1), .SDcols = post_names]

label_names <- grep("^merge_label_", names(dt), value = T)
dt[,merge_label := as.character(NA)]
for (label in label_names){
  dt[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt), value = T)
dt[,delta_hhi := 0]
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  treated_var <- treated_names[i]
  hhi_var <- hhi_names[i]
  treated_zips <- unique(dt[get(treated_var) == 1, Zip5])
  dt[get(post_var) == 1 & Zip5 %in% treated_zips & delta_hhi > 0, delta_hhi := delta_hhi + get(hhi_var)]
  dt[Zip5 %in% treated_zips & delta_hhi == 0, delta_hhi := get(hhi_var)]
}

# Clean zip code data -------------------------
# Year between 2000 and 2020
dt_zip <- dt_zip[year >= 2000 & year <= 2020]

# Drop low property count zips 
bad_zips <- dt_zip[,.(N = mean_na(N)), .(Zip5)][N < 50, Zip5]
print(paste0("Dropping ", nrow(dt_zip[Zip5 %in% bad_zips]), " rows of zips with under 50 average property observations per year out of ", 
             nrow(dt_zip), " total rows."))
dt_zip <- dt_zip[!(Zip5 %in% bad_zips)]

# Define new vars 
dt_zip[,log_zori := log(ZORI)]
dt_zip[!is.na(month) & !is.na(year),monthyear := parse_date_time(paste0(year, "-", month), "ym")]
treated_names <- grep("^treated_", names(dt_zip), value=T)
dt_zip[,treated_overlap := rowSums(.SD, na.rm = T), .SDcols = treated_names]

# Document multi-merge zips 
dt_zip[treated_overlap > 1, multi_merge := 1] # Why is multi-merge not being recorded properly?

# Create pooled variables
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
start_date <- min(mergers$DateAnnounced) - years(5)
dt_zip <- dt_zip[monthyear >= start_date] 

label_names <- grep("^merge_label_", names(dt_zip), value = T)
dt_zip[,merge_label := as.character(NA)]
for (label in label_names){
  dt_zip[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt_zip), value = T)
post_names <- grep("^post_", names(dt_zip), value = T)
dt_zip[,delta_hhi := 0]
dt_zip[,treated := 0]
dt_zip[,post := 0]
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  hhi_var <- hhi_names[i]
  treated_var <- treated_names[i]
  dt_zip[get(post_var) == 1 & get(treated_var) == 1 & delta_hhi > 0, delta_hhi := delta_hhi + get(hhi_var)]
  dt_zip[get(treated_var) == 1 & delta_hhi == 0, delta_hhi := get(hhi_var)]
  dt_zip[get(post_var) == 1 & get(treated_var) == 1, treated := 1]
  dt_zip[post == 0, post := get(post_var)]
}

dt_zip <- merge(dt_zip, acs_zip, by.x = c("Zip5", "year"), by.y = c("zip", "shift_year"), all.x = T)

# ======================= (0.5) HHI and ZORI =========================
# Median zip prices for DHHI quartiles
dt_zip[delta_hhi > 0, dhhi_quart := cut(delta_hhi, breaks=c(quantile(delta_hhi,probs=seq(0,1,by=0.25),na.rm=T)),
                                              labels=paste0("Q", 1:4), include.lowest = T)]

dt_month_zori <- dt_zip[,.(med_zori = median_na(ZORI)),.(dhhi_quart, monthyear)]
dt_month_zori[is.na(dhhi_quart), dhhi_quart := "Unmerged Zips"]
ggplot(dt_month_zori, aes(x = monthyear, y = med_zori, color = dhhi_quart)) + geom_line() + 
  labs(y = "Median ZORI", color = "\u0394 HHI Quartile", title = "HHI Trend By Quartile of Simulated HHI Increase") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zori_dhhi_q.pdf"))

# HHI Trends by DHHI Quartiles
dt_hhi <- dt_zip[, .(mean_hhi = mean_na(hhi)), .(dhhi_quart, year)]
dt_hhi[is.na(dhhi_quart), dhhi_quart := "Unmerged Zips"]
ggplot(dt_hhi, aes(x = year, y = mean_hhi, color = dhhi_quart)) + geom_line() + 
  labs(y = "Avg HHI", color = "\u0394 HHI Quartile", title = "HHI Trend By Quartile of Simulated HHI Increase") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/hhi_dhhi_q.pdf"))

# Simple HHI plot for zip codes affected by merger
dt_zip[, ever_treated := as.integer(treated_overlap > 0)]
dt_hhi <- dt_zip[,.(mean_hhi = mean_na(hhi), median_hhi = median_na(hhi)), .(year, ever_treated)]
ggplot(dt_hhi, aes(x = year, y = median_hhi, color = ever_treated)) + geom_line() +
  labs(y = "Med HHI", title = "Med HHI Over Sample Period") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/med_hhi_treated.pdf"))

ggplot(dt_hhi, aes(x = year, y = mean_hhi, color = ever_treated)) + geom_line() +
  labs(y = "Mean HHI", title = "Mean HHI Over Sample Period") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/mean_hhi_treated.pdf"))

# DHHI and 1-5 Year Post-Merger HHI 
setorder(dt_zip, Zip5, year)
dt_zip[post > 0, post_yr := year[1], Zip5]
for (i in 0:4){
  dt_tmp <- dt_zip[year == post_yr + 1]
  ggplot(dt_tmp, aes(x = delta_hhi, y = hhi)) + geom_point() + 
    labs(x = "\u0394 Sim HHI", y = paste0("Year ", i, " Post-Merger"), title = paste0("\u0394 Sim HHI and Post-Merger Year")) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/dhhi_post_", i, ".pdf"))
}

# ============= Residuals of Zip Code Time Trends on HHI/log_rent ==============
library(lfe)

# HHI on Zip Time Trends ------------
dt_hhi <- dt_zip[delta_hhi != 0, .(hhi = median_na(hhi), delta_hhi = max_na(delta_hhi), treated = min_na(treated),
                                   prop_unemp = median_na(prop_unemp), 
                                   median_household_income = median_na(median_household_income)), .(Zip5, year)]
dt_hhi[, year_trend := year - min_na(year)]
dt_hhi[, sq_year_trend := year_trend^2]
dt_hhi[, tert_year_trend := year_trend^3]

hhi_reg <- felm(hhi ~ 0|factor(year) + factor(Zip5) + factor(Zip5):year_trend|0|Zip5, data = dt_hhi)
hhi_resid <- hhi_reg$residuals
dt_hhi[, resid := hhi_resid]

ggplot(dt_hhi, aes(x = year, y = resid)) + geom_point() + 
  stat_smooth(aes(y = resid, x = year), method = "lm", formula = y ~ x + x^2 + x^3) +
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_resids_point.pdf"))


ggplot(dt_hhi[,.(resid = median(resid)), year], aes(x = year, y = resid)) + geom_line() + 
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_resids_line.pdf"))

# Spot check: residuals against delta HHI
ggplot(dt_hhi, aes(x = delta_hhi, y = resid)) + geom_point() + facet_wrap(~ treated) + 
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_resids_dhhi_line.pdf"))

# Mean of residuals
ggplot(dt_hhi[, .(resid = mean(resid)), delta_hhi], aes(x = delta_hhi, y = resid)) + geom_point() + 
  labs(title = "Avg Residual from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_avg_resids.png"))

# Slopes of residuals by Zip
dt_hhi[, event_yr := year - min(year) + 1]
resid_slope <- dt_hhi[, .(slope = coef(lm(resid ~ event_yr - 1))['event_yr'], delta_hhi = max(delta_hhi)), Zip5]
ggplot(resid_slope, aes(x = delta_hhi, y = slope)) + geom_point() + geom_smooth(method = "lm", color = "red", se = F) + 
  theme_bw() + 
  labs(x = "sim\u0394 HHI", y = "Slope of Residual Trend", title = "Residual Trend of Zip Time Trend Regression on HHI")

# Run it again for quadratic and third order 
hhi_reg <- felm(hhi ~ 0|factor(year) + factor(Zip5) + factor(Zip5):year_trend + factor(Zip5):sq_year_trend|0|Zip5, data = dt_hhi)
hhi_resid <- hhi_reg$residuals
dt_hhi[, resid := hhi_resid]

ggplot(dt_hhi, aes(x = year, y = resid)) + geom_point() + 
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_sq_resids_point.pdf"))


ggplot(dt_hhi[,.(resid = median(resid)), year], aes(x = year, y = resid)) + geom_line() + 
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_sq_resids_line.pdf"))

# Run it again for quadratic and third order 
hhi_reg <- felm(hhi ~ 0|factor(year) + factor(Zip5) + factor(Zip5):year_trend + factor(Zip5):sq_year_trend + 
                  factor(Zip5):tert_year_trend|0|Zip5, data = dt_hhi)
hhi_resid <- hhi_reg$residuals
dt_hhi[, resid := hhi_resid]

ggplot(dt_hhi, aes(x = year, y = resid)) + geom_point() + 
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_tert_resids_point.pdf"))


ggplot(dt_hhi[,.(resid = median(resid)), year], aes(x = year, y = resid)) + geom_line() + 
  labs(title = "Residuals from Zip Time Trend Regression on HHI") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/zip_hhi_tert_resids_line.pdf"))

# Prepost HHI slope trends
setorder(dt_zip, Zip5, monthyear)
dt_zip[, event_yr := year - min(year) + 1]
dt_zip[, max_dhhi := max(delta_hhi), Zip5]
hhi_slope <- dt_zip[max_dhhi > 0, .(slope = coef(lm(hhi ~ event_yr))["event_yr"], delta_hhi = max_dhhi[1]), .(Zip5, treated)]
#hhi_slope <- dt_zip[delta_hhi > 0, .(slope = coef(lm(hhi ~ event_yr - 1))["event_yr"], delta_hhi = delta_hhi[1]), .(Zip5, treated)]
hhi_slope[treated == 0, status := "Pre"]
hhi_slope[treated == 1, status := "Post"]
hhi_slope[, status := factor(status, levels=c("Pre", "Post"))]
ggplot(hhi_slope, aes(x = delta_hhi, y = slope)) + geom_point() + geom_smooth(method = "lm", color = "red", se = F) + 
  facet_wrap(~ status) + theme_bw() + 
  labs(x = "sim\u0394 HHI", y = "Slope of HHI Trend", title = "Slope of HHI Trend by Zip Code")

hhi_slope[, slope_change := slope - shift(slope), Zip5]
ggplot(hhi_slope, aes(x = delta_hhi, y = slope_change)) + geom_point() + geom_smooth(method = "lm", color = "red", se = F) + 
  theme_bw() + labs(x = "sim\u0394 HHI", y = "\u0394 Slope of HHI Trend", title = "\u0394 Slope of HHI Trend by Zip Code") +
  ggsave(paste0(mergers_path, "figs/for_paper/delta_zip_hhi_slope.png"))


# log rent on Zip Time Trends ------------
merge_labels <- c("Beazer Pre-Owned - American Homes 4 Rent", "Beazer Pre-Owned - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
for (merge_id in c(1,2,4)){
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  merge_announce_year <- unique(mergers[MergeID_1 == merge_id, year_announced])
  merge_eff_year <- unique(mergers[MergeID_1 == merge_id, year_effective])
  merge_target <- unique(mergers[MergeID_1 == merge_id, TargetName])
  merge_acquiror <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
  
  # Relevant column names based on merge id
  treated_var <- paste0("treated_", merge_id)
  post_var <- paste0("post_", merge_id)
  delta_hhi_var <- paste0("delta_hhi_", merge_id)
  dt[, treated := 0]
  dt[get(treated_var) == 1 & get(post_var) == 1, treated := 1]
  dt[,delta_hhi := get(delta_hhi_var)]
  
  sample1_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year )
               & get(sample1_var) == 1]
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  reg <- felm(log_rent ~ 0|factor(year) + factor(Zip5) + factor(Zip5):time_trend|0|Zip5, data = dt_tmp)
  
  resids <- reg$residuals
  dt_tmp[, resid := resids]
  
  ggplot(dt_tmp, aes(x = year, y = resid)) + geom_point(size = 1) + 
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_resids_", merge_id, "_point.pdf"))
  
  ggplot(dt_tmp[,.(resid = median(resid)), year], aes(x = year, y = resid)) + geom_line() +
  labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_resids_", merge_id, "_line.pdf"))
  
  # Again on squared 
  reg <- felm(log_rent ~ 0|factor(year) + factor(Zip5) + factor(Zip5):time_trend + factor(Zip5):sq_time_trend|0|Zip5, data = dt_tmp)
  
  resids <- reg$residuals
  dt_tmp[, resid := resids]
  
  ggplot(dt_tmp, aes(x = year, y = resid)) + geom_point(size = 1) + 
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_sq_resids_", merge_id, "_point.pdf"))
  
  ggplot(dt_tmp[,.(resid = median(resid)), year], aes(x = year, y = resid)) + geom_line() +
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_sq_resids_", merge_id, "_line.pdf"))
}

# for (merge_id in c(1,2,4)){
#   # Delta prepost log zori
#   merge_announce_date <- min(mergers[MergeID_1 == merge_id, DateAnnounced])
#   merge_eff_date <- max(mergers[MergeID_1 == merge_id, DateEffective])
#   sample_start <- merge_announce_date - years(2)
#   sample_end <- merge_eff_date + years(2)
#   
#   dt_tmp_plot <- merge(dt_tmp[treated == 1,.(log_zori = median_na(log_zori), treated = 1), .(monthyear)],
#                        dt_tmp[treated == 0,.(log_zori = median_na(log_zori), treated = 0), .(monthyear)],
#                        by = "monthyear")
#   dt_tmp_plot <- dt_tmp_plot[monthyear >= sample_start & monthyear <= sample_end]
#   dt_tmp_plot[, delta_log_zori := ]
#   ggplot(dt_tmp_plot, aes(x = monthyear, y = ZORI, color = factor(treated))) + geom_line() + 
#     labs(x = "Date", y = "Median Zip Rent", color = "Treated", title = merge_label) + 
#     ggsave(paste0(mergers_path, "figs/for_paper/delta_zori_", merge_id, ".png"))
#   
#}

# TODO: delta prepost log zori trends, prepost HHI slope trends

# 
# # ======================= (1) Comparison of Trends =========================
# # Zip code coverage 
# prop_zips <- unique(dt[!is.na(RentPrice), Zip5])
# zip_zips <- unique(dt_zip[!is.na(ZORI), Zip5])
# length(setdiff(prop_zips, zip_zips))
# length(setdiff(zip_zips, prop_zips))
# 
# # DHHI Distributions (larger zips actually have somewhat lower DHHI)
# quantile(dt[delta_hhi > 0, delta_hhi])
# quantile(dt_zip[!is.na(ZORI) & delta_hhi > 0, delta_hhi])
# 
# # Plot DHHI distribution for each merger
# unique_hhi <- unique(dt_zip[,.(Zip5, delta_hhi_1, delta_hhi_2, delta_hhi_4, delta_hhi_5)])
# unique_hhi_long <- melt(unique_hhi, id.vars = c("Zip5"))
# ggplot(unique_hhi_long, aes(x = variable, y = value)) + geom_point() + 
#   labs(x = "Merger", y = "Delta HHI", title = "DHHI Distribution by Merger") + 
#   ggsave(paste0(mergers_path, "figs/diagnostics/dhhi_distr.png"))
# 
# # Plot trends against quartiles of DHHI 
# dt[,monthyear := as.POSIXct(ymd(year, truncated = 2))]
# setnames(dt_zip, "log_zori", "log_rent")
# dt[,rent_type := "MLS"]
# dt_zip[,rent_type := "ZORI"]
# dt[delta_hhi > 0,dhhi_quart := cut(delta_hhi, breaks=c(quantile(delta_hhi,probs=seq(0,1,by=0.5),na.rm=T)),
#                            labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
# dt_zip[delta_hhi > 0,dhhi_quart := cut(delta_hhi, breaks=c(quantile(delta_hhi,probs=seq(0,1,by=0.5),na.rm=T)),
#                            labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
# 
# dt_tot <- rbindlist(list(dt[!is.na(dhhi_quart)], dt_zip[!is.na(dhhi_quart)]), fill = T)
# dt_tot <- dt_tot[,.(med_rent = median_na(log_rent)), .(monthyear, dhhi_quart, rent_type)]
# ggplot(dt_tot[!is.na(dhhi_quart)], aes(x = monthyear, y = med_rent, color = dhhi_quart, linetype = rent_type)) + 
#   geom_line() + geom_line(stat="smooth", method="lm", alpha=0.7) +
#   labs(x = "date", y = "median log rent", title = "DHHI Rent Trends: All", color = "DHHI Quantile", 
#                      linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_trends.png"))
# 
# # Plot differences in trends 
# dt_tot_wide <- dcast(dt_tot, rent_type + monthyear ~ dhhi_quart, value.var="med_rent")
# dt_tot_wide[, trend_diff := `Upper 50%` - `Lower 50%`]
# ggplot(dt_tot_wide, aes(x = monthyear, y = trend_diff, linetype = rent_type)) + geom_line() + 
#   geom_smooth(method = "lm", color = "grey", alpha = 0.7, se = F) + 
#   labs(x = "date", y = "Upper-Lower 50% Log DHHI Trend Difference", title = "DHHI Rent Trends: All",
#        linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_delta_trends.png"))
# 
# # Is the discontinuity at 2018 caused by multi-merge zips?
# setorder(dt_zip, Zip5, monthyear)
# dt_zip[,dhhi_change := delta_hhi != shift(delta_hhi), .(Zip5)]
# dt_zip[,.(N = sum_na(dhhi_change)), .(monthyear)] # It appears so 
# 
# # Separate trend plots for each merger
# for (i in c(1,2,4,5)){
#   sample_var <- paste0("sample_", i)
#   sample_prop_var <- paste0("sample_", i, "_c1")
#   merge_label_var <- paste0("merge_label_", i)
#   dhhi_var <- paste0("delta_hhi_", i)
#   merge_label <- unique(dt[get(sample_prop_var) == 1 & get(merge_label_var) != "", get(merge_label_var)])
#   dt[get(dhhi_var) > 0 & get(sample_prop_var) == 1,dhhi_quart := cut(get(dhhi_var), breaks=c(quantile(get(dhhi_var),probs=seq(0,1,by=0.5),na.rm=T)),
#                                           labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
#   dt_zip[get(dhhi_var) > 0 & get(sample_var) == 1,dhhi_quart := cut(get(dhhi_var), breaks=c(quantile(get(dhhi_var),probs=seq(0,1,by=0.5),na.rm=T)),
#                                          labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
#   
#   dt_tot <- rbindlist(list(dt[!is.na(dhhi_quart)], dt_zip[!is.na(dhhi_quart)]), fill = T)
#   dt_tot <- dt_tot[,.(med_rent = median_na(log_rent)), .(monthyear, dhhi_quart, rent_type)]
#   ggplot(dt_tot[!is.na(dhhi_quart)], aes(x = monthyear, y = med_rent, color = dhhi_quart, linetype = rent_type)) + 
#     geom_line() + geom_line(stat="smooth", method="lm", alpha=0.7) +
#     labs(x = "date", y = "median log rent", title = paste0("DHHI Rent Trends: ", merge_label), color = "DHHI Quantile", 
#          linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_trends_", i, ".png"))
#   
#   dt_tot_wide <- dcast(dt_tot, rent_type + monthyear ~ dhhi_quart, value.var="med_rent")
#   dt_tot_wide[, trend_diff := `Upper 50%` - `Lower 50%`]
#   ggplot(dt_tot_wide, aes(x = monthyear, y = trend_diff, linetype = rent_type)) + geom_line() + 
#     geom_smooth(method = "lm", color = "grey", alpha = 0.7, se = F) + 
#     labs(x = "date", y = "Upper-Lower 50% Log DHHI Trend Difference", title = paste0("DHHI Rent Trends: ", merge_label),
#          linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_", i, "_delta_trends.png"))
# }
