# ===================================================================
# Mergers Analysis: Property Regressions
# ===================================================================
# Property level regressions 
#

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
library(sandwich)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"

t0 <- Sys.time()
dt <- fread(paste0(data_path, "panel_hhi_rent.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")))
# dt <- fread(paste0(data_path, "panel_hhi_rent.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")), nrows=5e6)
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

# Document and remove multi-merge zips
n_multi_drop <- nrow(dt[multi_merge == 1])
treated_zips <- unique(dt[treated_overlap >= 1, Zip5])
n_affected <- nrow(dt[Zip5 %in% treated_zips])
print(paste0("There are ", n_multi_drop, " observations that are multi-merge affected out of ", 
             n_affected, " rows..."))

treated_zips <- unique(dt[treated_overlap >= 1, Zip5])
dt_multi_check <- dt[year >= 2007 & Zip5 %in% treated_zips,.(median_rent = median_na0(RentPrice)), .(multi_merge, year)]
ggplot(dt_multi_check, aes(x = year, y = median_rent, color = factor(multi_merge))) + geom_line() + 
  labs(title = "Median Rent for Treated Zips") + ggsave(paste0(mergers_path, "figs/diagnostics/multi_merge_property_rent.png"))

# Drop low-property count zips 
dt_n <- dt[,.N,.(Zip5,year)]
bad_zips <- dt_n[,.(N = mean_na(N)), .(Zip5)][N < 50, Zip5]
print(paste0("Dropping ", nrow(dt[Zip5 %in% bad_zips]), " rows of zips with under 50 average property observations per year out of ", 
             nrow(dt), " total rows."))
dt <- dt[!(Zip5 %in% bad_zips)]

# Drop outlier rents 
n_bad_rents <- nrow(dt[RentPrice >= 5000 | RentPrice <= 500])
print(paste0("Dropping ", n_bad_rents, " rows with rent higher than 100k and lower than 100"))
dt <- dt[RentPrice < 5000 & RentPrice > 500]

# observation cutoff at 2018
dt <- dt[year <= 2018]

# Drop bad mergers 
bad_ids <- c(3,5)
bad_cols <- c(paste0("post_", bad_ids), paste0("merge_label_", bad_ids), paste0("delta_hhi_", bad_ids), 
              paste0("treated_", bad_ids))
dt[, (bad_cols) := NULL]

# Owner indicators
# Flags for whether target or acquiror
acquirors <- c("American Homes 4 Rent", "Invitation Homes Inc", "Starwood Waypoint Residential")
targets <- c("Beazer Pre-Owned Rental Homes", "Ellington Housing", "Colony American Homes Inc")
dt[Merger_Owner_Fill %in% acquirors, merge_position := "acquiror"]
dt[Merger_Owner_Fill %in% targets, merge_position := "target"]
dt[is.na(merge_position), merge_position := ""]
dt[,merge_position := factor(merge_position, levels = c("", "target", "acquiror"))]

# Starwood Waypoint is a special case: it will be target post merger 4 in treated group 4 
dt[Merger_Owner_Fill == "Starwood Waypoint Residential" & post_4 == 1 & treated_4 == 1, merge_position := "target"]

# Define aggregate variables
dt[, zip_firm := paste0(Zip5, Merger_Owner_Fill)]
dt[, treated := as.integer(treated)]
dt[,merge_firm_owned := as.integer(!is.na(Merger_Owner_Fill) & Merger_Owner_Fill != "")]

treated_names <- setdiff(grep("^treated_", names(dt), value = T), "treated_overlap")
post_names <- grep("^post_", names(dt), value = T)
label_names <- grep("^merge_label_", names(dt), value = T)
dt[,merge_label := as.character(NA)]
for (label in label_names){
  dt[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt), value = T)

# Need to set delta hhi for all in zip 
for (hhi_name in hhi_names){
  dt[, (hhi_name) := median_na0(get(hhi_name)), Zip5]
  dt[is.na(get(hhi_name)), (hhi_name) := 0]
}

dt[,delta_hhi_alt := 0]
dt[,delta_hhi := 0]
dt[,treated := 0]
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  treated_var <- treated_names[i]
  hhi_var <- hhi_names[i]
  treated_zips <- unique(dt[get(treated_var) == 1, Zip5])
  
  #TODO: Check if multi-merge zips should be constant DHHI with last post var! 
  dt[Zip5 %in% treated_zips & get(post_var) == 1, treated := 1]
  
  dt[get(post_var) == 1 & Zip5 %in% treated_zips & delta_hhi > 0, delta_hhi := delta_hhi + get(hhi_var)]
  dt[Zip5 %in% treated_zips & delta_hhi == 0, delta_hhi := get(hhi_var)]
  
  # Define alternative DHHI
  dt[Zip5 %in% treated_zips, delta_hhi_alt := delta_hhi_alt + get(hhi_var)]
}

# Define alternative treated var 
dt[, treated_alt := treated]
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  treated_var <- treated_names[i]
  treated_zips <- unique(dt[get(treated_var) == 1, Zip5])
  dt[post_var == 0 & Zip5 %in% treated_zips, treated_alt := 0]
}

# ============= Diagnostics ===================================================
dt_zip <- dt[year >= 2009, .(med_rent = median_na(RentPrice), N = .N), .(Zip5, delta_hhi, year)]

# Zip rent growth rates
setorder(dt_zip, Zip5, year)
dt_zip[,zip_rent_growth := (last(med_rent) - first(med_rent))/first(med_rent), Zip5]

# Scatter of DHHI and zip sizes/rent growth 
dt_scatter <- dt_zip[,.(avg_N = mean_na(N), zip_rent_growth = median_na(zip_rent_growth)), .(Zip5, delta_hhi)]

ggplot(dt_scatter, aes(x = log(delta_hhi), y = avg_N)) + geom_point() +
 labs(x = "Log DHHI", y = "Mean Annual Zip N", title = "DHHI by Zip Size") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/dhhi_N_scatter.png"))

ggplot(dt_scatter, aes(x = log(delta_hhi), y = zip_rent_growth)) + geom_point() +
  labs(x = "Log DHHI", y = "Zip Rent Growth", title = "DHHI by Zip Rent Growth") + 
  ggsave(paste0(mergers_path, "figs/diagnostics/dhhi_rent_scatter.png"))

# ============= Property Level Regressions ===================================================
estimate_path <- paste0(mergers_path, "estimates/")
library(lfe)
library(stargazer)

# Want to cluster by zip-firm (treatment HHI on zip-level and sampling differs by firm in terms of level of capture)
dt_tmp <- dt[year >= 2009]

# HHI on Delta HHI ------------
dt_hhi <- dt_tmp[delta_hhi != 0, .(hhi = median_na(hhi), delta_hhi = median_na(delta_hhi), treated = min_na(treated),
                                   prop_unemp = median_na(prop_unemp), 
                                   median_household_income = median_na(median_household_income)), .(Zip5, year)]
head(dt_hhi[,.N,.(Zip5, year)][order(-N)])

# Include zip time trend
dt_hhi[, year_trend := year - min_na(year)]
dt_hhi[, sq_year_trend := year_trend^2]
dt_hhi[, dhhi_treated := delta_hhi*treated]
reg0 <- feols(hhi ~ dhhi_treated|factor(year) + factor(Zip5), data = dt_hhi)
reg1 <- feols(hhi ~ dhhi_treated + prop_unemp|factor(year) + factor(Zip5), data = dt_hhi)
reg2 <- feols(hhi ~ dhhi_treated + prop_unemp + median_household_income|factor(year) + factor(Zip5), data = dt_hhi)
reg3 <- feols(hhi ~ dhhi_treated + prop_unemp + median_household_income|factor(year) + Zip5[year_trend], data = dt_hhi)
reg4 <- feols(hhi ~ dhhi_treated + prop_unemp + median_household_income|factor(year) + Zip5[year_trend, sq_year_trend], data = dt_hhi)
reg5 <- feols(hhi ~ dhhi_treated + prop_unemp + median_household_income + delta_hhi:year_trend |factor(year) + factor(Zip5), data = dt_hhi)
reg6 <- feols(hhi ~ dhhi_treated + prop_unemp + median_household_income + delta_hhi:year_trend + delta_hhi:sq_year_trend|factor(year) + factor(Zip5), data = dt_hhi)

covariates_line <- c("Zip Controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
zip_trend_line <- c("Zip Time Trend", "No", "No","No", "Yes", "Sq", "No", "No")
hhi_trend_line <- c("$\\Delta \\text{sim\\_HHI}$ Time Trend", "No", "No", "No","No", "No", "Yes", "Sq")
lines <- list(covariates_line, zip_trend_line, hhi_trend_line)

out <- capture.output(esttex(reg0,reg1, reg2, reg3,reg4,reg5,reg6, cluster="Zip5",
                                extraline=lines, se="cluster", order = c("treated", "prop_unemp", "median_household_income", "year_trend"),
                                dict = c("dhhi_treated" = "$\\Delta \\text{sim\\_HHI} \\times \\text{Post}$"),
                                label = paste0("Post-Merger Concentration on Simulated Concentration")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("htbp", "H", out, fixed = T)

cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=F)

# 2-way FE with delta HHI and firm-owned indicator
# Save averages
avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)

# Time trend
dt_tmp[,time_trend := year - min_na(year)]
dt_tmp[,sq_time_trend := time_trend^2]
dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(log_sqft) & !is.na(tot_unit_val) & 
                                     !is.na(prop_unemp) & !is.na(median_household_income), time_trend]), Zip5]
dt_tmp[, dhhi_treated := delta_hhi * treated]
# OG spec, DHHI ONLY ---------

reg0 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5, data = dt_tmp) # OG treated variable to compare 
reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5, data = dt_tmp)
reg2 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5, data = dt_tmp)
reg3 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
reg4 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|factor(year) + Zip5, data = dt_tmp)
reg5 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
               delta_hhi:time_trend + delta_hhi:sq_time_trend|factor(year) + Zip5, data = dt_tmp)

zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No")
hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes", "Yes")
lines <- list(zip_trend_line, hhi_trend_line)
out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,cluster="Zip5",
                                extraline = lines,se="cluster",
                                label = "2-Way FE with delta HHI and firm owned indicator, All Treated Zips, OG"))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("htbp", "H", out, fixed = T)

# Replace notes
note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                     ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                     ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                     "}} \\\\")
note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
out[grepl("Signif Codes",out)] <- note.latex
cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)

# Event-Time Regs 
merge_labels <- c("Beazer Pre-Owned - American Homes 4 Rent", "Beazer Pre-Owned - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
mergers$label <- merge_labels
# Each mergers: event time coefficients
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
  dt[,delta_hhi := get(delta_hhi_var)]
  
  # Set treated and sample
  sample1_var <- paste0("sample_", merge_id, "_c1")
  dt[get(sample1_var) == 1 & get(post_var) == 1, treated := 1]
  
  sample_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  dt_tmp[, event_yr := factor(year - merge_eff_year)]
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  
  event_reg <- felm(log_rent ~ delta_hhi:event_yr + log_sqft + tot_unit_val + 
                      prop_unemp + median_household_income|factor(year) + factor(Zip5)|0|Zip5, 
                    data = dt_tmp)
  
  # Plot event time coefficients with 1 SE shading 
  varnames <- rownames(event_reg$coefficients)[-1:-4]
  event_years <- as.integer(gsub("delta_hhi:event_yr", "", varnames, fixed = T))
  years <- sort(unique(dt_tmp$year))
  
  # Enforce reference level at event month 0
  coefs <- event_reg$coefficients[-1:-4] 
  coefs[is.na(coefs)] <- 0
  ref_coef <- coefs[which(event_years == 0)]
  coefs <- coefs - ref_coef
  
  coefs <- coefs * avg_delta_hhi
  se <- se(event_reg, cluster="Zip5")[-1:-4] * 1.96
  se[is.na(se)] <- se[which(event_years == 0)]
  
  years <- years[1:length(coefs)]
  event_month_dt <- data.table(coefs = coefs, event_years = event_years, se_ub = coefs + se, se_lb = coefs - se, date = years)
  ggplot(event_month_dt, aes(x = date, y = coefs)) + geom_line() + geom_point(shape=16, size=2, color="red") + 
    geom_ribbon(aes(ymin = se_lb, ymax = se_ub), alpha=0.5) + geom_vline(aes(linetype = "Merge Effective", xintercept = merge_eff_year)) + 
    geom_vline(aes(linetype="Merge Announced", xintercept = merge_announce_year)) + 
    scale_linetype_manual(values = c("solid", "dashed"), breaks = c("Merge Effective", "Merge Announced"), name = "Timing") + 
    labs(x = "Date", y = "Estimate Scaled by Average Simulated \u0394 HHI", title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/regs/year_coefs_", merge_id, ".png"), width = 20)
  
  # Same thing but also interact with merger owner
  dt_tmp[,merge_position := ""]
  dt_tmp[Merger_Owner_Fill %in% merge_target, merge_position := "target"]
  dt_tmp[Merger_Owner_Fill %in% merge_acquiror, merge_position := "acquiror"]
  
  event_reg <- felm(log_rent ~ delta_hhi:event_yr:merge_position + log_sqft + tot_unit_val + 
                      prop_unemp + median_household_income|factor(year) + factor(Zip5)|0|Zip5, 
                    data = dt_tmp)
  
  # Plot event time coefficients with 1 SE shading 
  varnames <- grep("merge_positiontarget", rownames(event_reg$coefficients), value = T)
  event_years <- as.integer(gsub(":merge_positiontarget", "", gsub("delta_hhi:event_yr", "", varnames, fixed = T)))
  years <- sort(unique(dt_tmp$year))
  
  # Enforce reference level at event month 0 for each merge position
  tot_coefs <- c()
  tot_se <- c()
  positions <- rep(c("Outside", "Target", "Acquiror"), length(event_years))
  for (i in 0:2){
    coefs <- event_reg$coefficients[(5+i*length(event_years)):(4 + (i+1)*length(event_years))] 
    coefs[is.na(coefs)] <- 0
    ref_coef <- coefs[which(event_years == 0)]
    coefs <- coefs - ref_coef
    tot_coefs <- c(tot_coefs, coefs)
    
    se <- event_reg$se[(5+i*length(event_years)):(4 + (i+1)*length(event_years))] * 1.96
    se[is.na(se)] <- se[which(event_years == 0)]
    tot_se <- c(tot_se, se)
  }
    
  tot_years <- rep(years[1:length(event_years)], 3)
  event_month_dt <- data.table(coefs = tot_coefs, event_years = rep(event_years, 3), 
                               se_ub = tot_coefs + tot_se, se_lb = tot_coefs - tot_se, date = tot_years,
                               positions = positions)
  ggplot(event_month_dt, aes(x = date, y = coefs, color = positions)) + geom_line() + geom_point(shape=16, size=2, color="red") + 
    geom_vline(aes(linetype = "Merge Effective", xintercept = merge_eff_year)) + 
    geom_vline(aes(linetype="Merge Announced", xintercept = merge_announce_year)) + 
    geom_ribbon(aes(ymin = se_lb, ymax = se_ub), alpha=0.5) +
    scale_linetype_manual(values = c("solid", "dashed"), breaks = c("Merge Effective", "Merge Announced"), name = "Timing") + 
    labs(x = "Date", y = "Estimate Scaled by Average Simulated \u0394 HHI", title = merge_label, color="Market Position") + 
    ggsave(paste0(mergers_path, "figs/regs/year_prop_coefs_", merge_id, ".png"), width = 10)
}

# Each merger: 2-Way FE without DHHI interaction 
merge_labels <- c("Beazer Pre-Owned - American Homes 4 Rent", "Beazer Pre-Owned - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
mergers$label <- merge_labels
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id %in% c(3,5)){
    next
  }
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
  
  # Fix target and acquiror labels
  dt[,merge_position := ""]
  dt[Merger_Owner_Fill %in% merge_target, merge_position := "target"]
  dt[Merger_Owner_Fill %in% merge_acquiror, merge_position := "acquiror"]
  
  # 1) Control = merging-zip non-affected 
  sample1_var <- paste0("sample_", merge_id, "_c1")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year )
               & get(sample1_var) == 1]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we observe for under 3 periods
  dt_tmp[, n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg6 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg7 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg8 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
                 delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No", "Yes", "No", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "No", "Yes", "Sq")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6,reg7,reg8, cluster="Zip5",
                                  extraline = lines,se="cluster",
                                  label = paste0("2-Way FE, No DHHI, Merger: ", merge_label,", Control: Within-Zip Non-Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 2) Control = merging firms in unmerged zips; include all c1 properties as treated now
  dt[get(post_var) == 1, treated := get(sample1_var)]
  sample_var <- paste0("sample_", merge_id, "_c2")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg6 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg7 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg8 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
                 delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No", "Yes", "No", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "No", "Yes", "Sq")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6,reg7,reg8, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, No DHHI, Merger: ", merge_label,", Control: Outside Zip Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  sample_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]

  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg6 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg7 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg8 <- feols(log_rent ~ treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
                 delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No", "Yes", "No", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "No", "Yes", "Sq")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6,reg7,reg8, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, No DHHI, Merger: ", merge_label,", Control: Single Firm Zips")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
}

# Each merger: 2-Way FE with DHHI interaction 
merge_labels <- c("Beazer Pre-Owned - American Homes 4 Rent", "Beazer Pre-Owned - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
mergers$label <- merge_labels
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id %in% c(3,5)){
    next
  }
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
  
  # Fix target and acquiror labels
  dt[,merge_position := ""]
  dt[Merger_Owner_Fill %in% merge_target, merge_position := "target"]
  dt[Merger_Owner_Fill %in% merge_acquiror, merge_position := "acquiror"]
  
  # 1) Control = merging-zip non-affected 
  sample1_var <- paste0("sample_", merge_id, "_c1")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year )
               & get(sample1_var) == 1]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we observe for under 3 periods
  dt_tmp[, n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg6 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg7 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6,reg7, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, Merger: ", merge_label,", Control: Within-Zip Non-Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 2) Control = merging firms in unmerged zips; include all c1 properties as treated now
  dt[, treated := 0]
  dt[get(post_var) == 1, treated := get(sample1_var)]
  sample_var <- paste0("sample_", merge_id, "_c2")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg6 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg7 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)

  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6,reg7, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, Merger: ", merge_label,", Control: Outside Zip Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  sample_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)

  reg0 <- feols(log_rent ~ dhhi_treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg6 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg7 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6,reg7, cluster="Zip5",
                               extraline = lines, se="cluster",
                               label = paste0("2-Way FE, Merger: ", merge_label,", Control: Single Zip Firms")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # Plot sample of reg6, reg7 residuals
  dt_tmp[, resid := reg6$residuals]
  sampled_resids <- dt_tmp[,.SD[sample(.N, 0.1*.N)], year]
  ggplot(sampled_resids, aes(x = year, y = resid)) + geom_point(size = 1) + 
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/regs/prop_zip_resids_", merge_id, "_point.pdf"))
  
  dt_tmp[, resid := reg7$residuals]
  sampled_resids <- dt_tmp[,.SD[sample(.N, 0.1*.N)], year]
  ggplot(sampled_resids, aes(x = year, y = resid)) + geom_point(size = 1) + 
    labs(title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/regs/prop_dhhi_resids_", merge_id, "_point.pdf"))
}

# Same regs with squared time trends
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id %in% c(3,5)){
    next
  }
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
  
  # Fix target and acquiror labels
  dt[,merge_position := ""]
  dt[Merger_Owner_Fill %in% merge_target, merge_position := "target"]
  dt[Merger_Owner_Fill %in% merge_acquiror, merge_position := "acquiror"]
  
  # 1) Control = merging-zip non-affected 
  sample1_var <- paste0("sample_", merge_id, "_c1")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year )
               & get(sample1_var) == 1]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we observe for under 3 periods
  dt_tmp[, n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg3 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Sq Linear Trend", "Yes", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Sq Linear Trend", "No", "Yes", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, Sq Merger: ", merge_label,", Control: Within Zip Non-Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 2) Control = merging firms in unmerged zips; include all c1 properties as treated now
  dt[, treated := 0]
  dt[get(post_var) == 1, treated := get(sample1_var)]
  sample_var <- paste0("sample_", merge_id, "_c2")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg3 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Sq Linear Trend", "Yes", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Sq Linear Trend", "No", "Yes", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, Sq Merger: ", merge_label,", Control: Outsize Zip Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  sample_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1)]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg3 <- feols(log_rent ~ dhhi_treated:merge_position + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Sq Linear Trend", "Yes", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Sq Linear Trend", "No", "Yes", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE, Sq Merger: ", merge_label,", Control: Single Firm Zips")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
}


# Same regressions restricting on zips common to Zillow
dt_zip <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character")
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id %in% c(3,5)){
    next
  }
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
  
  zillow_zips <- unique(dt_zip[get(treated_var) == 1 & !is.na(ZORI), Zip5])
    
  # Fix target and acquiror labels
  dt[,merge_position := ""]
  dt[Merger_Owner_Fill %in% merge_target, merge_position := "target"]
  dt[Merger_Owner_Fill %in% merge_acquiror, merge_position := "acquiror"]
  
  # 1) Control = merging-zip non-affected 
  sample1_var <- paste0("sample_", merge_id, "_c1")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year )
               & get(sample1_var) == 1 & Zip5 %in% zillow_zips]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # We will need to remove the zips that we observe for under 3 periods
  dt_tmp[, n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|
                 factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg6 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
                 delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "Sq", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "Sq")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6, cluster="Zip5",
                                  extraline = lines,se="cluster",
                                  label = paste0("2-Way FE ZILLOW, Merger: ", merge_label,", Control: Within-Zip Non-Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 2) Control = merging firms in unmerged zips; include all c1 properties as treated now
  dt[get(post_var) == 1, treated := get(sample1_var)]
  sample_var <- paste0("sample_", merge_id, "_c2")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1) & Zip5 %in% zillow_zips]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|
                 factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg6 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
                 delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "Sq", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "Sq")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE ZILLOW, Merger: ", merge_label,", Control: Outside Zip Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  sample_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[((year >= merge_announce_year-5 & year <= merge_announce_year) | year >=merge_eff_year) & 
                 (get(sample_var) == 1 | get(sample1_var) == 1) & Zip5 %in% zillow_zips]
  
  # Add linear time trend to regressions 
  dt_tmp[, min_year := min_na(year)]
  dt_tmp[,time_trend := year - min_year]
  dt_tmp[,sq_time_trend := time_trend^2]
  dt_tmp[, dhhi_treated := delta_hhi*treated]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[,n_zip_trend := uniqueN(.SD[!is.na(sqft) & !is.na(tot_unit_val), time_trend]), Zip5]
  
  # Save averages
  avg_rent <- round(mean_na(dt_tmp$RentPrice), digits=2)
  avg_log_rent <- round(mean_na(dt_tmp$log_rent), digits=2)
  avg_hhi <- round(mean_na0(dt_tmp[,.(hhi = median_na(hhi)), .(Zip5)][,hhi]), digits=2)
  sd_hhi <- round(sd(dt_tmp[hhi != 0,.(hhi = median_na(hhi)), .(Zip5)][,hhi], na.rm=T), digits=2)
  
  reg0 <- feols(log_rent ~ dhhi_treated|Zip5 + factor(year), data = dt_tmp)
  reg1 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val|Zip5 + factor(year), data = dt_tmp)
  reg2 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|Zip5 + factor(year), data = dt_tmp)
  reg3 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|factor(year) + Zip5[time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg4 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + delta_hhi:time_trend|Zip5 + factor(year), data = dt_tmp)
  reg5 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income|
                 factor(year) + Zip5[time_trend, sq_time_trend], data = dt_tmp[n_zip_trend >= 3])
  reg6 <- feols(log_rent ~ dhhi_treated + log_sqft + tot_unit_val + prop_unemp + median_household_income + 
                 delta_hhi:time_trend + delta_hhi:sq_time_trend|Zip5 + factor(year), data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No", "Sq", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes","No", "Sq")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(esttex(reg0,reg1, reg2, reg3, reg4, reg5,reg6, cluster="Zip5",
                               extraline = lines,se="cluster",
                               label = paste0("2-Way FE ZILLOW, Merger: ", merge_label,", Control: Single Firm Zips")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is property level, with year and zip FE. 
                     Sample average rent: ", avg_rent, ". Sample average log rent: ", avg_log_rent, 
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
                       "}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Signif Codes",out)] <- note.latex
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # Plot event time coefficients with 1 SE shading -------------------------
  dt_tmp[, event_yr := factor(year - merge_eff_year)]
  avg_delta_hhi <- round(mean_na0(dt_tmp[delta_hhi != 0,.(delta_hhi = median_na(delta_hhi)), .(Zip5)][,delta_hhi]), digits=2)
  
  event_reg <- felm(log_rent ~ delta_hhi:event_yr + log_sqft + tot_unit_val + 
                      prop_unemp + median_household_income|factor(year) + factor(Zip5), 
                    data = dt_tmp)
  
  varnames <- rownames(event_reg$coefficients)[-1:-4]
  event_years <- as.integer(gsub("delta_hhi:event_yr", "", varnames, fixed = T))
  years <- sort(unique(dt_tmp$year))
  
  # Enforce reference level at event month 0
  coefs <- event_reg$coefficients[-1:-4] 
  coefs[is.na(coefs)] <- 0
  ref_coef <- coefs[which(event_years == 0)]
  coefs <- coefs - ref_coef
  
  coefs <- coefs * avg_delta_hhi
  se <- event_reg$se[-1:-4] * 1.96
  se[is.na(se)] <- se[which(event_years == 0)]
  
  years <- years[1:length(coefs)]
  event_month_dt <- data.table(coefs = coefs, event_years = event_years, se_ub = coefs + se, se_lb = coefs - se, date = years)
  ggplot(event_month_dt, aes(x = date, y = coefs)) + geom_line() + geom_point(shape=16, size=2, color="red") + 
    geom_ribbon(aes(ymin = se_lb, ymax = se_ub), alpha=0.5) + geom_vline(aes(linetype = "Merge Effective", xintercept = merge_eff_year)) + 
    geom_vline(aes(linetype="Merge Announced", xintercept = merge_announce_year)) + 
    scale_linetype_manual(values = c("solid", "dashed"), breaks = c("Merge Effective", "Merge Announced"), name = "Timing") + 
    labs(x = "Date", y = "Estimate Scaled by Average Simulated \u0394 HHI", title = paste0(merge_label, " Zillow Zips")) + 
    ggsave(paste0(mergers_path, "figs/regs/year_zillow_coefs_", merge_id, ".png"), width = 20)
  
}




