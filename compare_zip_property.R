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
library(fixest)
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

# Total population not actually sum of the race categories
acs_zip[,total_pop := rowSums(.SD, na.rm = T), .SDcols = c("white_pop", "black_pop", "hispanic_latino_pop","asian_pop", "american_indian_pop", "two_or_more_races_pop")]
races <- c("black", "white", "hispanic_latino", "asian", "american_indian", "two_or_more_races")
for (race in races){
  pop <- paste0(race, "_pop")
  prop <- paste0("prop_", race)
  acs_zip[,(prop) := get(pop)/total_pop]
}
acs_zip[,prop_min := prop_black + prop_hispanic_latino]

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

# Zip is integer
dt[,Zip5 := as.integer(Zip5)]

# Drop low-property count zips 
dt_n <- dt[,.N,.(Zip5,year)]
bad_zips <- dt_n[,.(N = mean_na(N)), .(Zip5)][N < 50, Zip5]
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

# ============= Residuals of Zip Code Time Trends on HHI/log_rent ==============
library(lfe)

# log rent on Zip Time Trends ------------
merge_labels <- c("Beazer Pre-Owned - American Homes 4 Rent", "Beazer Pre-Owned - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
lin_resids <- list()
sq_resids <- list()
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
  
  reg <- feols(log_rent ~ 0|factor(year) + Zip5[time_trend], data = dt_tmp)
  
  resids <- reg$residuals
  dt_tmp[, resid := resids]
  
  # Just take 10% of points for each year
  sampled_resids <- dt_tmp[,.SD[sample(.N, 0.1*.N)], year]
  ggplot(sampled_resids, aes(x = year, y = resid)) + geom_point(size = 1) + 
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_resids_", merge_id, "_point.pdf"))
  
  med_resids <- dt_tmp[,.(resid = median(resid)), year]
  med_resids[, merger := merge_label]
  ggplot(med_resids, aes(x = year, y = resid)) + geom_line() +
  labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_resids_", merge_id, "_line.pdf"))
  
  # Save median residuals in list
  lin_resids[[merge_id]] <- med_resids
  
  # Check if fixest is faster -- yeah it is!
  reg <- feols(log_rent ~ 0 | factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp)

  resids <- reg$residuals
  dt_tmp[, resid := resids]
  
  ggplot(dt_tmp, aes(x = year, y = resid)) + geom_point(size = 1) + 
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_sq_resids_", merge_id, "_point.pdf"))
  
  med_resids <- dt_tmp[,.(resid = median(resid)), year]
  med_resids[, merger := merge_label]
  sq_resids[[merge_id]] <- med_resids
  ggplot(med_resids, aes(x = year, y = resid)) + geom_line() +
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/diagnostics/zip_rent_sq_resids_", merge_id, "_line.pdf"))
}

# Median resid curves all on one thing 
lin_resid_dt <- rbindlist(lin_resids)
ggplot(lin_resid_dt, aes(x = year, y = resid, color = merger)) + geom_line() +
  labs(color = "Merger", title = "Residuals of 2-Way FE Regression on Log Rent and Zip Time Trends", y = "Median Residual") + 
  ggsave(paste0(mergers_path, "figs/for_paper/zip_rent_resids_line.pdf"))

sq_resid_dt <- rbindlist(sq_resids)
ggplot(sq_resid_dt, aes(x = year, y = resid, color = merger)) + geom_line() +
  labs(color = "Merger", title = "Residuals of 2-Way FE Regression on Log Rent and Zip Quadratic Time Trends", y = "Median Residual") + 
  ggsave(paste0(mergers_path, "figs/for_paper/zip_sq_rent_resids_line.pdf"))

# ================ Parallel Plots ====================
# Zip comparison of different characteristics
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
  
  dt_tmp[delta_hhi == 0, dhhi_quart := "Control"]
  dt_tmp[delta_hhi > 0, dhhi_quart := cut(delta_hhi, breaks=c(quantile(delta_hhi,probs=seq(0,1,by=0.25),na.rm=T)),
                                      labels=paste0("Q",1:4), include.lowest = T)]
  
  acs_to_name <- c("prop_min" = "% Minority Population", "prop_hs_grad_plus" = "% HS Graduate Population", 
                   "prop_unemp" = "% Unemployed Population", "median_household_income" = "Median Household Income",
                   "gini_inequality_index" = "Gini Index", "total_pop" = "Total Population", "tot_unit_val" = "Property Tax Value",
                   "sqft" = "Square Footage")
  for (var in c("prop_min", "prop_hs_grad_plus", "prop_unemp", "median_household_income", "gini_inequality_index", "tot_unit_val",
                "sqft")){
    dt_tmp_plot <- dt_tmp[, .(tmp = median_na(get(var))), .(dhhi_quart, year)]
    y_label <- acs_to_name[var]
    first_year <- min(dt_tmp[!is.na(tmp)])
    ggplot(dt_tmp_plot[year <= 2018 & year >= first_year], aes(x = year, y = tmp, color = dhhi_quart)) + geom_line() +
      geom_vline(aes(linetype = "Merge Effective", xintercept = merge_eff_year)) + 
      geom_vline(aes(linetype="Merge Announced", xintercept = merge_announce_year)) + 
      scale_linetype_manual(values = c("solid", "dashed"), breaks = c("Merge Effective", "Merge Announced"), name = "Timing") + 
      labs(x = "Date", y = y_label, color = "sim\u0394 HHI Quartile", title = merge_label) +
      ggsave(paste0(mergers_path, "figs/for_paper/", var, "_", merge_id, ".png"))
  }
}

# ======================= Beazer Reconciliation =======================
beazer_zips <- unique(dt[sample_1_c3 == 1, Zip5])
dt_prop_zip <- dt[Zip5 %in% beazer_zips, .(med_log_rent = median_na(log_rent)), .(Zip5, year)]
beazer_dt <- merge(dt_prop_zip, dt_zip[!is.na(log_zori) & sample_1 == 1], by = c("Zip5", "year"), all = T) # 1:M
beazer_dt[, rent_diff := med_log_rent - log_zori]
beazer_dt[, med_annual_log_rent := median_na(med_log_rent), year]
beazer_dt[, med_annual_zori := median_na(log_zori), year]
ggplot(beazer_dt, aes(x = year, y = rent_diff)) + geom_point() 

beazer_diff <- beazer_dt[,.(mean_diff = mean_na(rent_diff), median_diff = mean_na(rent_diff), sd_diff = sd(rent_diff, na.rm=T), 
                            diff_of_median = median_na(med_annual_log_rent - med_annual_zori)), year]
beazer_diff[,`:=`(upper_se = mean_diff + sd_diff, lower_se = mean_diff - sd_diff)]
ggplot(beazer_diff[!is.na(mean_diff)], aes(x = year, y = mean_diff)) + geom_line() + geom_point(shape=16, size = 2,color="red") +
  scale_x_continuous(breaks = unique(beazer_diff$year)) + 
  geom_vline(xintercept = 2014) + 
    labs(y = "Log(Rent/ZORI)", title = "Beazer Pre-Owned - American Homes 4 Rent: MLS and ZORI Rent Difference", x = "Year") +
    ggsave(paste0(mergers_path, "figs/for_paper/beazer_rent_recon.png"))


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
