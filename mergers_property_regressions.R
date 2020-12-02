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
# Document multi-merge zips


# ============= Property Level Regressions ===================================================
estimate_path <- paste0(mergers_path, "estimates/")
library(lfe)
library(stargazer)

# Basic DiD (2-way FE with just the treated props and different time periods)
reg0 <- felm(RentPrice ~ treated|factor(year) + factor(id)|0|merge_label, data = dt[!is.na(merge_label) & year >= 2000])
reg1 <- felm(RentPrice ~ treated + sqft + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt[!is.na(merge_label) & year >= 2000])
reg2 <- felm(log_rent ~ treated|factor(year) + factor(id)|0|merge_label, data = dt[!is.na(merge_label) & year >= 2000])
reg3 <- felm(log_rent ~ treated + sqft + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt[!is.na(merge_label) & year >= 2000])
reg4 <- felm(rent_sqft ~ treated|factor(year) + factor(id)|0|merge_label, data = dt[!is.na(merge_label) & year >= 2000])
reg5 <- felm(rent_sqft ~ treated + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt[!is.na(merge_label) & year >= 2000])

out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), title = "2-Way FE"))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)
cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=F)


mergers <- fread(paste0(mergers_path, "mergers_cleaned.csv"))

# Each merger: basic DiD -- pre-post flags 
for (merge_id in unique(mergers$MergeID_1)){
  # Merger vars 
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
  acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
  norm_year <- unique(mergers[MergeID_1 == merge_id, Year])
  
  # Adjust panel depending on merger
  target_zips <- unique(dt[year==norm_year & Merger_Owner_Name %in% target_name, Zip5])
  acquiror_zips <- unique(dt[year==norm_year & Merger_Owner_Name == acquiror_name, Zip5])
  treated_zips <- intersect(target_zips, acquiror_zips)
  dt[,event_yr := as.integer(year - norm_year)]
  dt[Zip5 %in% treated_zips & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), treated_tmp := 1]
  dt[, post := as.integer(event_yr > 0)]
  
  # 1) Control = merging-zip non-affected 
  dt[is.na(treated_tmp) & Zip5 %in% treated_zips, treated_tmp := 0]
  reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post + tot_unit_val, data = dt[year >= 2000])
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost Basic, Merger: ", merge_label,", Control: Within-Zip Non-Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 2) Control = merging firms in unmerged zips
  dt[treated_tmp == 0, treated_tmp := NA]
  dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), treated_tmp := 0]
  reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost Basic, Merger: ", merge_label,", Control: Unmerged Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  single_firm_zips <- unique(dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), Zip5])
  dt[treated_tmp == 0, treated_tmp := NA]
  dt[Zip5 %in% single_firm_zips, treated_tmp := 0]
  reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
  reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost Basic, Merger: ", merge_label,", Control: Single Firm Zips")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
}

# Each merger: pre-post with reduced-form delta HHI and separate merge-owned indicator
for (merge_id in unique(mergers$MergeID_1)){
  # Merger vars 
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
  acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
  norm_year <- unique(mergers[MergeID_1 == merge_id, Year])
  
  # Adjust panel depending on merger
  target_zips <- unique(dt[year==norm_year & Merger_Owner_Name %in% target_name, Zip5])
  acquiror_zips <- unique(dt[year==norm_year & Merger_Owner_Name == acquiror_name, Zip5])
  treated_zips <- intersect(target_zips, acquiror_zips)
  dt[,event_yr := as.integer(year - norm_year)]
  dt[Zip5 %in% treated_zips, treated_tmp := 1]
  dt[Zip5 %in% treated_zips & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), merge_owned := 1]
  dt[, post := as.integer(event_yr > 0)]
  
  # 2) Control = merging firms in unmerged zips
  dt[treated_tmp == 0, treated_tmp := NA]
  dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), treated_tmp := 0]
  reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned, data = dt[year >= 2000])
  reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned + sqft + tot_unit_val, data = dt[year >= 2000])
  reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned, data = dt[year >= 2000])
  reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned + sqft + tot_unit_val, data = dt[year >= 2000])
  reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned, data = dt[year >= 2000])
  reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned + tot_unit_val, data = dt[year >= 2000])
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost HHI Reduced Form, Merger: ", merge_label,", Control: Unmerged Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  single_firm_zips <- unique(dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), Zip5])
  dt[treated_tmp == 0, treated_tmp := NA]
  dt[Zip5 %in% single_firm_zips, treated_tmp := 0]
  reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned, data = dt[year >= 2000])
  reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned + sqft + tot_unit_val, data = dt[year >= 2000])
  reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned, data = dt[year >= 2000])
  reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned + sqft + tot_unit_val, data = dt[year >= 2000])
  reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned, data = dt[year >= 2000])
  reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp*post*delta_hhi + treated_tmp*post*delta_hhi*merge_owned + tot_unit_val, data = dt[year >= 2000])
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost HHI Reduced Form, Merger: ", merge_label,", Control: Single Firm Zips")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
}

