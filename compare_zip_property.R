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
n_bad_rents <- nrow(dt[RentPrice >= 1e5 & RentPrice <= 100])
print(paste0("Dropping ", n_bad_rents, " rows with rent higher than 100k and lower than 100."))
dt <- dt[RentPrice < 1e5 & RentPrice > 100]

# Define delta hhi 
dt[, zip_firm := paste0(Zip5, Merger_Owner_Name)]
dt[, treated := as.integer(treated)]

dt[,merge_firm_owned := as.integer(!is.na(Merger_Owner_Name) & Merger_Owner_Name != "")]
dt_prop <- dt[year >= 2009 & year <= 2018]

treated_names <- setdiff(grep("^treated_", names(dt_prop), value = T), "treated_overlap")
dt_prop[,treated := 0]
treated_zips <- unique(dt_prop[treated_overlap >= 1, Zip5])
dt_prop[Zip5 %in% treated_zips,treated := 1]

post_names <- grep("^post_", names(dt_prop), value = T)
dt_prop[,post := 0]
dt_prop[,post := as.integer(rowSums(.SD) >= 1), .SDcols = post_names]

label_names <- grep("^merge_label_", names(dt_prop), value = T)
dt_prop[,merge_label := as.character(NA)]
for (label in label_names){
  dt_prop[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt_prop), value = T)
dt_prop[,delta_hhi := 0]
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  treated_var <- treated_names[i]
  hhi_var <- hhi_names[i]
  treated_zips <- unique(dt_prop[get(treated_var) == 1, Zip5])
  dt_prop[get(post_var) == 1 & Zip5 %in% treated_zips & delta_hhi > 0, delta_hhi := delta_hhi + get(hhi_var)]
  dt_prop[Zip5 %in% treated_zips & delta_hhi == 0, delta_hhi := get(hhi_var)]
}

# Clean zip code data -------------------------
# Year between 2000 and 2020
dt_zip <- dt_zip[year >= 2000 & year <= 2020]

# Drop low property count zips 
bad_zips <- dt_zip[,.(N = mean_na(N)), .(Zip5)][N < 10, Zip5]
print(paste0("Dropping ", nrow(dt_zip[Zip5 %in% bad_zips]), " rows of zips with under 10 average property observations per year out of ", 
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
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  hhi_var <- hhi_names[i]
  treated_var <- treated_names[i]
  dt_zip[get(post_var) == 1 & get(treated_var) == 1 & delta_hhi > 0, delta_hhi := delta_hhi + get(hhi_var)]
  dt_zip[get(treated_var) == 1 & delta_hhi == 0, delta_hhi := get(hhi_var)]
  dt_zip[get(post_var) == 1 & get(treated_var) == 1, treated := 1]
}
# ======================= (1) Comparison of Trends =========================
# Zip code coverage 
prop_zips <- unique(dt_prop[!is.na(RentPrice), Zip5])
zip_zips <- unique(dt_zip[!is.na(ZORI), Zip5])
length(setdiff(prop_zips, zip_zips))
length(setdiff(zip_zips, prop_zips))

# DHHI Distributions (larger zips actually have somewhat lower DHHI)
quantile(dt_prop[delta_hhi > 0, delta_hhi])
quantile(dt_zip[!is.na(ZORI) & delta_hhi > 0, delta_hhi])

# Plot DHHI distribution for each merger
unique_hhi <- unique(dt_zip[,.(Zip5, delta_hhi_1, delta_hhi_2, delta_hhi_4, delta_hhi_5)])
unique_hhi_long <- melt(unique_hhi, id.vars = c("Zip5"))
ggplot(unique_hhi_long, aes(x = variable, y = value)) + geom_point() + 
  labs(x = "Merger", y = "Delta HHI", title = "DHHI Distribution by Merger") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/dhhi_distr.png"))

# Plot trends against quartiles of DHHI 
dt_prop[,monthyear := as.POSIXct(ymd(year, truncated = 2))]
setnames(dt_zip, "log_zori", "log_rent")
dt_prop[,rent_type := "MLS"]
dt_zip[,rent_type := "ZORI"]
dt_prop[delta_hhi > 0,dhhi_quart := cut(delta_hhi, breaks=c(quantile(delta_hhi,probs=seq(0,1,by=0.5),na.rm=T)),
                           labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
dt_zip[delta_hhi > 0,dhhi_quart := cut(delta_hhi, breaks=c(quantile(delta_hhi,probs=seq(0,1,by=0.5),na.rm=T)),
                           labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]

dt_tot <- rbindlist(list(dt_prop[!is.na(dhhi_quart)], dt_zip[!is.na(dhhi_quart)]), fill = T)
dt_tot <- dt_tot[,.(med_rent = median_na(log_rent)), .(monthyear, dhhi_quart, rent_type)]
ggplot(dt_tot[!is.na(dhhi_quart)], aes(x = monthyear, y = med_rent, color = dhhi_quart, linetype = rent_type)) + 
  geom_line() + geom_line(stat="smooth", method="lm", alpha=0.7) +
  labs(x = "date", y = "median log rent", title = "DHHI Rent Trends: All", color = "DHHI Quantile", 
                     linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_trends.png"))

# Plot differences in trends 
dt_tot_wide <- dcast(dt_tot, rent_type + monthyear ~ dhhi_quart, value.var="med_rent")
dt_tot_wide[, trend_diff := `Upper 50%` - `Lower 50%`]
ggplot(dt_tot_wide, aes(x = monthyear, y = trend_diff, linetype = rent_type)) + geom_line() + 
  geom_smooth(method = "lm", color = "grey", alpha = 0.7, se = F) + 
  labs(x = "date", y = "Upper-Lower 50% Log DHHI Trend Difference", title = "DHHI Rent Trends: All",
       linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_delta_trends.png"))

# Is the discontinuity at 2018 caused by multi-merge zips?
setorder(dt_zip, Zip5, monthyear)
dt_zip[,dhhi_change := delta_hhi != shift(delta_hhi), .(Zip5)]
dt_zip[,.(N = sum_na(dhhi_change)), .(monthyear)] # It appears so 

# Separate trend plots for each merger
for (i in c(1,2,4,5)){
  sample_var <- paste0("sample_", i)
  sample_prop_var <- paste0("sample_", i, "_c1")
  merge_label_var <- paste0("merge_label_", i)
  dhhi_var <- paste0("delta_hhi_", i)
  merge_label <- unique(dt_prop[get(sample_prop_var) == 1 & get(merge_label_var) != "", get(merge_label_var)])
  dt_prop[get(dhhi_var) > 0 & get(sample_prop_var) == 1,dhhi_quart := cut(get(dhhi_var), breaks=c(quantile(get(dhhi_var),probs=seq(0,1,by=0.5),na.rm=T)),
                                          labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
  dt_zip[get(dhhi_var) > 0 & get(sample_var) == 1,dhhi_quart := cut(get(dhhi_var), breaks=c(quantile(get(dhhi_var),probs=seq(0,1,by=0.5),na.rm=T)),
                                         labels=c("Upper 50%", "Lower 50%"), include.lowest = T)]
  
  dt_tot <- rbindlist(list(dt_prop[!is.na(dhhi_quart)], dt_zip[!is.na(dhhi_quart)]), fill = T)
  dt_tot <- dt_tot[,.(med_rent = median_na(log_rent)), .(monthyear, dhhi_quart, rent_type)]
  ggplot(dt_tot[!is.na(dhhi_quart)], aes(x = monthyear, y = med_rent, color = dhhi_quart, linetype = rent_type)) + 
    geom_line() + geom_line(stat="smooth", method="lm", alpha=0.7) +
    labs(x = "date", y = "median log rent", title = paste0("DHHI Rent Trends: ", merge_label), color = "DHHI Quantile", 
         linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_trends_", i, ".png"))
  
  dt_tot_wide <- dcast(dt_tot, rent_type + monthyear ~ dhhi_quart, value.var="med_rent")
  dt_tot_wide[, trend_diff := `Upper 50%` - `Lower 50%`]
  ggplot(dt_tot_wide, aes(x = monthyear, y = trend_diff, linetype = rent_type)) + geom_line() + 
    geom_smooth(method = "lm", color = "grey", alpha = 0.7, se = F) + 
    labs(x = "date", y = "Upper-Lower 50% Log DHHI Trend Difference", title = paste0("DHHI Rent Trends: ", merge_label),
         linetype = "Rent Source") + ggsave(paste0(mergers_path, "figs/diagnostics/", "dhhi_", i, "_delta_trends.png"))
}
