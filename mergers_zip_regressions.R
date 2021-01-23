# ===================================================================
# Mergers Analysis: Zip Regressions
# ===================================================================
# Zip level regressions 
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
library(lfe)
library(stargazer)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"
estimate_path <- paste0(mergers_path, "estimates/")

t0 <- Sys.time()
dt <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character")
# dt <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")), nrows=1e6)
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# Read in mergers data 
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
merge_labels <- c("Beazer/Ellington - American Homes 4 Rent", "Beazer/Ellington - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
colors <- c("coral1","coral1", "dodgerblue", "darkorange", "forestgreen", "plum")
mergers$label <- merge_labels
mergers$colors <- colors
# ============= (0) Preprocessing ===================================================
# Year between 2000 and 2020
dt <- dt[year >= 2000 & year <= 2020]

# Drop low property count zips 
bad_zips <- dt[,.(N = mean_na(N)), .(Zip5)][N < 50, Zip5]
print(paste0("Dropping ", nrow(dt[Zip5 %in% bad_zips]), " rows of zips with under 50 average property observations per year out of ", 
             nrow(dt), " total rows."))
dt <- dt[!(Zip5 %in% bad_zips)]

# Define new vars 
dt[,log_zori := log(ZORI)]
dt[!is.na(month) & !is.na(year),monthyear := parse_date_time(paste0(year, "-", month), "ym")]
treated_names <- grep("^treated_", names(dt), value=T)
dt[,treated_overlap := rowSums(.SD, na.rm = T), .SDcols = treated_names]

# Aggregate vars
label_names <- grep("^merge_label_", names(dt), value = T)
dt[,merge_label := as.character(NA)]
for (label in label_names){
  dt[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt), value = T)
post_names <- grep("^post_", names(dt), value = T)
dt[,delta_hhi := 0]
dt[,delta_hhi_alt := 0]
dt[,treated := 0]
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  hhi_var <- hhi_names[i]
  treated_var <- treated_names[i]
  dt[get(post_var) == 1 & get(treated_var) == 1, treated := 1]
  dt[get(post_var) == 1 & get(treated_var) == 1 & delta_hhi > 0, delta_hhi := delta_hhi + get(hhi_var)]
  dt[get(treated_var) == 1 & delta_hhi == 0, delta_hhi := get(hhi_var)]
  dt[get(treated_var) == 1, delta_hhi_alt := delta_hhi + get(hhi_var)]
}

# Define alternative treated var 
dt[, treated_alt := treated]
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  treated_var <- treated_names[i]
  dt[post_var == 0 & get(treated_var) == 1, treated_alt := 0]
}

# Document multi-merge zips 
dt[, multi_merge := 0]
dt[treated_overlap > 1, multi_merge := 1] # Why is multi-merge not being recorded properly?
n_multi_zips <- uniqueN(dt[multi_merge == 1, Zip5])
print(paste0(n_multi_zips, " zips out of ", uniqueN(dt$Zip5), " with multi-merge."))
treated_zips <- unique(dt[treated_overlap >= 1, Zip5])
dt_multi_check <- dt[Zip5 %in% treated_zips,.(median_zori = as.numeric(median_na0(ZORI))), .(multi_merge, monthyear)]

ggplot(dt_multi_check, aes(x = monthyear, y = median_zori, color = factor(multi_merge))) + geom_line() + 
  labs(title = "Median Monthly ZORI for Treated Zips") + ggsave(paste0(mergers_path, "figs/diagnostics/multi_merge_zip_rent.png"))

# Let's explore HHI
dt_yr <- dt[,.(hhi = median_na(hhi)), .(Zip5, year)]
ggplot(dt_yr, aes(x = log(hhi))) + geom_histogram() + labs(x = "Log HHI", title = "Log HHI Frequency") + 
  ggsave(paste0(mergers_path, "hhi_hist.png"))

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

# ============= (0.5) Summary Figures ===================================================

# ============= (1) Zip Level Regressions ===================================================
# Restrict to 5 years before first merger
start_date <- min(mergers$DateAnnounced) - years(5)
dt_tmp <- dt[monthyear >= start_date] 

# HHI on Delta HHI ------------
dt_hhi <- dt_tmp[delta_hhi != 0, .(hhi = median_na(hhi), delta_hhi = max_na(delta_hhi), treated = max_na(treated),
                                   prop_unemp = median_na(prop_unemp), 
                                   median_household_income = median_na(median_household_income)), .(Zip5, year)]
head(dt_hhi[,.N,.(Zip5, year)][order(-N)])

# Include zip time trend
dt_hhi[, year_trend := year - min_na(year)]

reg0 <- felm(hhi ~ delta_hhi:treated|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg1 <- felm(hhi ~ delta_hhi:treated + prop_unemp|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg2 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg3 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income|factor(year) + factor(Zip5) + factor(Zip5):year_trend|0|Zip5, data = dt_hhi)
reg4 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income + delta_hhi:year_trend|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)

covariates_line <- c("Zip Controls", "No", "Yes", "Yes", "Yes", "Yes")
zip_trend_line <- c("Zip Time Trend", "No", "No","No", "Yes", "No")
hhi_trend_line <- c("$\\Delta \\text{sim\\_HHI}$ Time Trend", "No", "No", "No","No", "Yes")
# n_acs_zips <- uniqueN(dt_hhi[!is.na(prop_unemp) & !is.na(median_household_income), Zip5])
# n_zips <- c("# Zips Codes", uniqueN(dt_hhi$Zip5), n_acs_zips, n_acs_zips, n_acs_zips, n_acs_zips)
# hhi_labels <- c("HHI Statistics", "Mean", "SD", "10 Pctl", "50 Pctl", "99 Pctl")
# hhi <- c("HHI", mean_na(dt_hhi$hhi), sd(dt_hhi$hhi, na.rm=T), quantile(dt_hhi$hhi, 0.1), quantile(dt_hhi$hhi, 0.5), quantile(dt_hhi$hhi, 0.99))
# delta_hhi <- c("Delta HHI", mean_na(dt_hhi$delta_hhi), sd(dt_hhi$delta_hhi, na.rm=T), quantile(dt_hhi$delta_hhi, 0.1), quantile(dt_hhi$delta_hhi, 0.5), quantile(dt_hhi$delta_hhi, 0.99))

lines <- list(covariates_line, zip_trend_line, hhi_trend_line)

out <- capture.output(stargazer(reg0,reg1, reg2, reg3,reg4,
                                add.lines=lines, dep.var.labels = c("HHI"), order = c("treated", "prop_unemp", "median_household_income", "year_trend"),
                                covariate.labels = c("$\\Delta \\text{sim\\_HHI} \times \text{Post}$"),
                                title = paste0("Post-Merger Concentration on Simulated Concentration")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=F)

# Same thing but keep control HHI ------------
dt_hhi <- dt_tmp[, .(hhi = median_na(hhi), delta_hhi = max_na(delta_hhi), treated = max_na(treated),
                                   prop_unemp = median_na(prop_unemp), 
                                   median_household_income = median_na(median_household_income)), .(Zip5, year)]
max(dt_hhi[,.N,.(Zip5, year)])

# Include zip time trend
dt_hhi[, year_trend := year - min_na(year)]

reg0 <- felm(hhi ~ delta_hhi:treated|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg1 <- felm(hhi ~ delta_hhi:treated + prop_unemp|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg2 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg3 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income|factor(year) + factor(Zip5) + factor(Zip5):year_trend|0|Zip5, data = dt_hhi)
reg4 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income + delta_hhi:year_trend|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)

covariates_line <- c("Zip Controls", "No", "Yes", "Yes", "Yes", "Yes")
zip_trend_line <- c("Zip Time Trend", "No", "No","No", "Yes", "No")
hhi_trend_line <- c("$\\Delta \\text{sim\\_HHI}$ Time Trend", "No", "No", "No","No", "Yes")

lines <- list(covariates_line, zip_trend_line, hhi_trend_line)

out <- capture.output(stargazer(reg0,reg1, reg2, reg3,reg4,
                                add.lines=lines, dep.var.labels = c("HHI"), order = c("treated", "prop_unemp", "median_household_income", "year_trend"),
                                covariate.labels = c("$\\Delta \\text{sim\\_HHI} \times \text{Post}$"),
                                title = paste0("Post-Merger Concentration on Simulated Concentration, With Control HHI")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)


# Same thing but excluding multi-merge zips ----------------------------------
dt_hhi <- dt_tmp[delta_hhi != 0 & multi_merge == 0, .(hhi = median_na(hhi), delta_hhi = max_na(delta_hhi), treated = max_na(treated),
                                   prop_unemp = median_na(prop_unemp), 
                                   median_household_income = median_na(median_household_income)), .(Zip5, year)]
# Include zip time trend
dt_hhi[, year_trend := year - min_na(year)]

reg0 <- felm(hhi ~ delta_hhi:treated|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg1 <- felm(hhi ~ delta_hhi:treated + prop_unemp|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg2 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)
reg3 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income|factor(year) + factor(Zip5) + factor(Zip5):year_trend|0|Zip5, data = dt_hhi)
reg4 <- felm(hhi ~ delta_hhi:treated + prop_unemp + median_household_income + delta_hhi:year_trend|factor(year) + factor(Zip5)|0|Zip5, data = dt_hhi)

zip_trend_line <- c("Zip*Linear Trend", "No", "No","No", "Yes", "No")
hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No","No", "Yes")
n_acs_zips <- uniqueN(dt_hhi[!is.na(prop_unemp) & !is.na(median_household_income), Zip5])
n_zips <- c("# Zips Codes", uniqueN(dt_hhi$Zip5), n_acs_zips, n_acs_zips, n_acs_zips, n_acs_zips)
hhi_labels <- c("HHI Statistics", "Mean", "SD", "10 Pctl", "50 Pctl", "99 Pctl")
hhi <- c("HHI", mean_na(dt_hhi$hhi), sd(dt_hhi$hhi, na.rm=T), quantile(dt_hhi$hhi, 0.1), quantile(dt_hhi$hhi, 0.5), quantile(dt_hhi$hhi, 0.99))
delta_hhi <- c("Delta HHI", mean_na(dt_hhi$delta_hhi), sd(dt_hhi$delta_hhi, na.rm=T), quantile(dt_hhi$delta_hhi, 0.1), quantile(dt_hhi$delta_hhi, 0.5), quantile(dt_hhi$delta_hhi, 0.99))

lines <- list(zip_trend_line, hhi_trend_line, n_zips, hhi_labels, hhi, delta_hhi)

out <- capture.output(stargazer(reg0,reg1, reg2, reg3,reg4,
                                add.lines=lines,
                                title = paste0("2-Way FE HHI, No Multi-Merge")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)

# 2-way FE + delta HHI, all mergers ---------
# Remove all monthyears between announcement and effective 
dt_fe <- copy(dt_tmp)
# for (merge_id in unique(mergers$MergeID_1)){
#   # Skip American Residential Ppty (no props found)
#   if (merge_id == 3){
#     next 
#   }
#   sample_var <- paste0("sample_", merge_id)
#   merge_announce_date <- min(mergers[MergeID_1 == merge_id, DateAnnounced])
#   merge_eff_date <- max(mergers[MergeID_1 == merge_id, DateEffective])
#   dt_fe[(monthyear <= merge_announce_date | monthyear >= merge_eff_date) & get(sample_var) == 1, keep := 1]
# }
# dt_fe <- dt_fe[keep == 1]

avg_zori <- round(mean_na0(dt_fe$ZORI), 2)
avg_log_zori <- round(mean_na0(dt_fe$log_zori), 2)
avg_log_rent <- round(mean_na0(dt_fe$median_log_rent), 2)
avg_hhi <- round(mean_na0(dt_fe$hhi), 2)
avg_delta_hhi <- round(mean_na0(dt_fe$delta_hhi), 2)
sd_hhi <- round(sd(dt_fe[hhi!=0, hhi]), 2)
sd_delta_hhi <- round(sd(dt_fe[delta_hhi!=0, delta_hhi]), 2)

# Include zip time trend
dt_fe[,min_monthyear := min_na(monthyear)]
dt_fe[, month_trend := round(12 * (as.yearmon(monthyear) - as.yearmon(min_monthyear)))]

# We will need to remove the zips that we only observe for 1 period
dt_fe[!is.na(ZORI) & !is.na(median_log_sqft) & !is.na(median_tot_unit_val),n_zip_trend := uniqueN(month_trend), Zip5]

reg0 <- felm(ZORI ~ delta_hhi:treated + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg1 <- felm(ZORI ~ delta_hhi:treated + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5) + factor(Zip5):month_trend|0|Zip5, data = dt_fe[n_zip_trend >= 3])
reg2 <- felm(ZORI ~ delta_hhi:treated + median_sqft + median_tot_unit_val + delta_hhi:month_trend|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)

reg3 <- felm(log_zori ~ delta_hhi:treated|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg4 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg5 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg6 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg7 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5) + factor(Zip5):month_trend|0|Zip5, data = dt_fe[n_zip_trend >= 3])
reg8 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income + delta_hhi:month_trend|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)

zip_trend_line <- c("Zip*Linear Trend", "No", "Yes", "No", "No", "No", "No","No", "Yes", "No")
hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "Yes", "No", "No", "No", "No","No", "Yes")
lines <- list(zip_trend_line, hhi_trend_line)

# Notes
# note.latex <- paste0("Sample is zip level, with month-year and zip FE. Sample average ZORI: ", avg_zori, 
#                      ". Sample average log ZORI: ", avg_log_zori,
#                      ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi, 
#                      ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi, 
#                      ". ZORI sample covers years 2014-2020.")

out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,reg6,reg7,reg8,
                                column.labels = c("ZORI", "Log ZORI"), column.separate = c(3,6), 
                                add.lines=lines,
                                title = paste0("2-Way FE Zip, All Mergers")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

# Replace notes
note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE.
                     Sample average ZORI: ", avg_zori, ". Sample average log ZORI: ", avg_log_zori,
                     ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi,
                     ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi,
                     ". ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
out[grepl("Note",out)] <- note.latex

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)

# 2-way FE + delta HHI, exclude multi-merge zips ---------
# Remove all monthyears between announcement and effective 
dt_fe <- dt_fe[multi_merge == 0]

avg_zori <- round(mean_na0(dt_fe$ZORI), 2)
avg_log_zori <- round(mean_na0(dt_fe$log_zori), 2)
avg_log_rent <- round(mean_na0(dt_fe$median_log_rent), 2)
avg_hhi <- round(mean_na0(dt_fe$hhi), 2)
avg_delta_hhi <- round(mean_na0(dt_fe$delta_hhi), 2)
sd_hhi <- round(sd(dt_fe[hhi!=0, hhi]), 2)
sd_delta_hhi <- round(sd(dt_fe[delta_hhi!=0, delta_hhi]), 2)

# Include zip time trend
dt_fe[,min_monthyear := min_na(monthyear)]
dt_fe[, month_trend := round(12 * (as.yearmon(monthyear) - as.yearmon(min_monthyear)))]

# We will need to remove the zips that we only observe for 1 period
dt_fe[!is.na(ZORI) & !is.na(median_log_sqft) & !is.na(median_tot_unit_val),n_zip_trend := uniqueN(month_trend), Zip5]

reg0 <- felm(ZORI ~ delta_hhi:treated + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg1 <- felm(ZORI ~ delta_hhi:treated + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5) + factor(Zip5):month_trend|0|Zip5, data = dt_fe[n_zip_trend >= 3])
reg2 <- felm(ZORI ~ delta_hhi:treated + median_sqft + median_tot_unit_val + delta_hhi:month_trend|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)

reg3 <- felm(log_zori ~ delta_hhi:treated|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg4 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg5 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg6 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)
reg7 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5) + factor(Zip5):month_trend|0|Zip5, data = dt_fe[n_zip_trend >= 3])
reg8 <- felm(log_zori ~ delta_hhi:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income + delta_hhi:month_trend|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_fe)

zip_trend_line <- c("Zip*Linear Trend", "No", "Yes", "No", "No", "No", "No","No", "Yes", "No")
hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "Yes", "No", "No", "No", "No","No", "Yes")
lines <- list(zip_trend_line, hhi_trend_line)

out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,reg6,reg7,reg8,
                                column.labels = c("ZORI", "Log ZORI"), column.separate = c(3,6), 
                                add.lines=lines,
                                title = paste0("2-Way FE Zip, All Mergers, No Multi-Merge")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

# Replace notes
note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE.
                     Sample average ZORI: ", avg_zori, ". Sample average log ZORI: ", avg_log_zori,
                     ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi,
                     ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi,
                     ". ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
out[grepl("Note",out)] <- note.latex

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)

# 2-way FE, each merger, zip clustered SE
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  # if (merge_id == 3){
  #   next 
  # }
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  merge_announce_date <- min(mergers[MergeID_1 == merge_id, DateAnnounced])
  merge_eff_date <- max(mergers[MergeID_1 == merge_id, DateEffective])
  sample_start <- merge_announce_date - years(5)
  
  # Relevant column names based on merge id
  treated_var <- paste0("treated_", merge_id)
  post_var <- paste0("post_", merge_id)
  sample_var <- paste0("sample_", merge_id)
  hhi_var <- paste0("delta_hhi_", merge_id)
  
  #dt_tmp <- dt[((monthyear >= sample_start & monthyear < merge_announce_date) | monthyear >= merge_eff_date) & get(sample_var) == 1]
  dt_tmp <- dt[monthyear >= sample_start & get(sample_var) == 1]
  dt_tmp[, treated := get(treated_var)]
  dt_tmp[, post := get(post_var)]
  dt_tmp[,delta_hhi := get(hhi_var)]
  
  avg_zori <- mean_na0(dt_tmp$ZORI)
  avg_log_zori <- mean_na0(dt_tmp$log_zori)
  avg_log_rent <- mean_na0(dt_tmp$median_log_rent)
  avg_hhi <- round(mean_na0(dt_tmp$hhi), 2)
  avg_delta_hhi <- round(mean_na0(dt_tmp$delta_hhi), 2)
  sd_hhi <- round(sd(dt_tmp[hhi!=0, hhi]), 2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi!=0, delta_hhi]), 2)
  
  # Add linear time trend 
  dt_tmp[, min_monthyear := min_na(monthyear)]
  dt_tmp[, month_trend := round(12 * (as.yearmon(monthyear) - as.yearmon(min_monthyear)))]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[!is.na(ZORI) & !is.na(median_log_sqft) & !is.na(median_tot_unit_val),n_zip_trend := uniqueN(month_trend), Zip5]
  
  reg0 <- felm(log_zori ~ post:treated|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  reg1 <- felm(log_zori ~ post:treated + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  reg2 <- felm(log_zori ~ post:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  reg3 <- felm(log_zori ~ post:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5) + factor(Zip5):month_trend|0|Zip5, data = dt_tmp[n_zip_trend >= 3])
  reg4 <- felm(log_zori ~ post:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income + delta_hhi:month_trend|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,
                                  column.labels = c("Log Zori"),
                                  add.lines=lines,
                                  title = paste0("2-Way FE Zip, No DHHI, Merger: ", merge_label)))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{6}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE.
                       Sample average log ZORI: ", avg_log_zori, ". Sample average ZORI: ", avg_zori,
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi,
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi,
                       "ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Note",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)
}

# 2-way FE + delta HHI, each merger, zip clustered SE
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  # if (merge_id == 3){
  #   next 
  # }
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  merge_announce_date <- min(mergers[MergeID_1 == merge_id, DateAnnounced])
  merge_eff_date <- max(mergers[MergeID_1 == merge_id, DateEffective])
  sample_start <- merge_announce_date - years(5)
  
  # Relevant column names based on merge id
  treated_var <- paste0("treated_", merge_id)
  post_var <- paste0("post_", merge_id)
  sample_var <- paste0("sample_", merge_id)
  hhi_var <- paste0("delta_hhi_", merge_id)
  
  #dt_tmp <- dt[((monthyear >= sample_start & monthyear < merge_announce_date) | monthyear >= merge_eff_date) & get(sample_var) == 1]
  dt_tmp <- dt[monthyear >= sample_start & get(sample_var) == 1]
  dt_tmp[, treated := get(treated_var)]
  dt_tmp[, post := get(post_var)]
  dt_tmp[,delta_hhi := get(hhi_var)]
  
  avg_zori <- mean_na0(dt_tmp$ZORI)
  avg_log_zori <- mean_na0(dt_tmp$log_zori)
  avg_log_rent <- mean_na0(dt_tmp$median_log_rent)
  avg_hhi <- round(mean_na0(dt_tmp$hhi), 2)
  avg_delta_hhi <- round(mean_na0(dt_tmp$delta_hhi), 2)
  sd_hhi <- round(sd(dt_tmp[hhi!=0, hhi]), 2)
  sd_delta_hhi <- round(sd(dt_tmp[delta_hhi!=0, delta_hhi]), 2)
  
  # Add linear time trend 
  dt_tmp[, min_monthyear := min_na(monthyear)]
  dt_tmp[, month_trend := round(12 * (as.yearmon(monthyear) - as.yearmon(min_monthyear)))]
  
  # We will need to remove the zips that we only observe for 1 period
  dt_tmp[!is.na(ZORI) & !is.na(median_log_sqft) & !is.na(median_tot_unit_val),n_zip_trend := uniqueN(month_trend), Zip5]
  
  reg0 <- felm(log_zori ~ delta_hhi:post:treated|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  reg1 <- felm(log_zori ~ delta_hhi:post:treated + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  reg2 <- felm(log_zori ~ delta_hhi:post:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  reg3 <- felm(log_zori ~ delta_hhi:post:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income|factor(monthyear) + factor(Zip5) + factor(Zip5):month_trend|0|Zip5, data = dt_tmp[n_zip_trend >= 3])
  reg4 <- felm(log_zori ~ delta_hhi:post:treated + median_log_sqft + median_tot_unit_val + prop_unemp + median_household_income + delta_hhi:month_trend|factor(monthyear) + factor(Zip5)|0|Zip5, data = dt_tmp)
  
  zip_trend_line <- c("Zip*Linear Trend", "No", "No", "No", "Yes", "No")
  hhi_trend_line <- c("Delta HHI*Linear Trend", "No", "No", "No", "No", "Yes")
  lines <- list(zip_trend_line, hhi_trend_line)

  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,
                                  column.labels = c("Log Zori"),
                                  add.lines=lines,
                                  title = paste0("2-Way FE Zip, Merger: ", merge_label)))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{6}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE.
                       Sample average log ZORI: ", avg_log_zori, ". Sample average ZORI: ", avg_zori,
                       ". Sample average HHI: ", avg_hhi, ". Sample average delta HHI: ", avg_delta_hhi,
                       ". Sample SD HHI: ", sd_hhi, ". Sample SD delta HHI: ", sd_delta_hhi,
                       "ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Note",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)
  
  # Event time coefficient plots -- keep ALL dates
  dt_tmp <- dt[get(sample_var) == 1]
  dt_tmp[, treated := get(treated_var)]
  dt_tmp[, post := get(post_var)]
  dt_tmp[,delta_hhi := get(hhi_var)]
  
  post_date <- last(dt_tmp[post == 0 & !is.na(monthyear), monthyear])
  dt_tmp[, event_month := NA]
  dt_tmp[, event_month := round(12 * (as.yearmon(monthyear) - as.yearmon(merge_announce_date)))]
  dt_tmp <- dt_tmp[event_month >= -60]
  dt_tmp[, event_month := factor(event_month)]
  
  # Figure: Log ZORI for Treated/Control
  dt_tmp_plot <- rbindlist(list(dt_tmp[treated == 1,.(ZORI = median_na(ZORI), treated = 1), .(monthyear)],
                                dt_tmp[treated == 0,.(ZORI = median_na(ZORI), treated = 0), .(monthyear)]))
  ggplot(dt_tmp_plot, aes(x = monthyear, y = ZORI, color = factor(treated))) + geom_line() + 
    labs(x = "Date", y = "Median Zip Rent", color = "Treated", title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/prepost/zip/zori_", merge_id, ".png"))

  # Figure: Delta log ZORI between treated and control zips with SE bars
  annual_delta_mean <- c()
  annual_delta_sd <- c()
  months <- unique(dt_tmp$monthyear)
  event_months <- as.integer(levels(unique(dt_tmp$event_month)))
  for (month_tmp in months){
    comb <- CJ(dt_tmp[monthyear == month_tmp & treated == 1 & !is.na(ZORI), log(ZORI)], 
               dt_tmp[monthyear == month_tmp & treated == 0 & !is.na(ZORI), log(ZORI)])
    comb[, diff := V1 - V2]
    annual_delta_mean <- c(annual_delta_mean, mean_na(comb$diff))
    annual_delta_sd <- c(annual_delta_sd, sd(comb$diff, na.rm=T))
  }
  delta_dt <- data.table(delta_log_zori = annual_delta_mean, sd_log_zori = annual_delta_sd, date = months, event_month = event_months)
  delta_dt[, se_lb := delta_log_zori - sd_log_zori]
  delta_dt[, se_ub := delta_log_zori + sd_log_zori]
  
  ggplot(delta_dt[event_months >= -30 & event_months <= 30], aes(x = date, y = delta_log_zori)) + geom_line() + geom_point(shape=16, size=2, color="red") + 
    geom_ribbon(aes(ymin = se_lb, ymax = se_ub), color="grey", alpha=0.5) + geom_vline(aes(linetype = "Merge Effective", xintercept = merge_eff_date)) + 
    geom_vline(aes(linetype="Merge Announced", xintercept = merge_announce_date)) + 
    scale_linetype_manual(values = c("solid", "dashed"), breaks = c("Merge Effective", "Merge Announced"), name = "Timing") + 
    labs(x = "Date", y = "\u0394 Log(ZORI)", title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/prepost/zip/delta_zori_", merge_id, ".png"), width = 10)
  
  event_reg <- felm(log_zori ~ delta_hhi:event_month + median_log_sqft + median_tot_unit_val + 
                      prop_unemp + median_household_income|factor(monthyear) + factor(Zip5)|0|Zip5, 
                    data = dt_tmp)
  
  # Plot event time coefficients with 1 SE shading 
  varnames <- rownames(event_reg$coefficients)[-1:-4]
  event_months <- as.integer(gsub("delta_hhi:event_month", "", varnames, fixed = T))
  months <- sort(unique(dt_tmp$monthyear))
  
  # Enforce reference level at event month 0
  coefs <- event_reg$coefficients[-1:-4] 
  coefs[is.na(coefs)] <- 0
  ref_coef <- coefs[which(event_months == 0)]
  coefs <- coefs - ref_coef
  
  coefs <- coefs * avg_delta_hhi
  se <- event_reg$se[-1:-4] * 1.96
  se[is.na(se)] <- se[which(event_months == 0)]
  
  # TODO: WHY IS REGRESSION RANK DEFICIENT + DROPPING SOME MONTHS????
  months <- months[1:length(coefs)]
  event_month_dt <- data.table(coefs = coefs, event_months = event_months, se_ub = coefs + se, se_lb = coefs - se, date = months)
  ggplot(event_month_dt[event_months >= -30 & event_months <= 40], aes(x = date, y = coefs)) + geom_line() + geom_point(shape=16, size=1) + 
    geom_ribbon(aes(ymin = se_lb, ymax = se_ub), color="grey", alpha=0.5) + geom_vline(aes(linetype = "Merge Effective", xintercept = merge_eff_date)) + 
    geom_vline(aes(linetype="Merge Announced", xintercept = merge_announce_date)) + 
    scale_linetype_manual(values = c("solid", "dashed"), breaks = c("Merge Effective", "Merge Announced"), name = "Timing") + 
    labs(x = "Date", y = "Estimate Scaled by Average Delta HHI", title = merge_label) + 
    ggsave(paste0(mergers_path, "figs/regs/month_coefs_", merge_id, ".png"), width = 20)
}
