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
# Document and remove multi-merge zips
n_multi_drop <- nrow(dt[multi_merge == 1])
print(paste0("There are ", n_multi_drop, " observations that are multi-merge affected out of ", nrow(dt), " rows..."))

treated_zips <- unique(dt[treated == 1, Zip5])
dt_multi_check <- dt[year >= 2007 & Zip5 %in% treated_zips,.(median_rent = median_na0(RentPrice)), .(multi_merge, year)]
ggplot(dt_multi_check, aes(x = year, y = median_rent, color = factor(multi_merge))) + geom_line() + 
  labs(title = "Median Rent for Treated Zips") + ggsave(paste0(mergers_path, "figs/diagnostics/multi_merge_property_rent.png"))

# ============= Property Level Regressions ===================================================
estimate_path <- paste0(mergers_path, "estimates/")
library(lfe)
library(stargazer)

# 2-way FE with delta HHI and firm-owned indicator and excluding multi-merge zips
treated_zips <- unique(dt[treated_overlap == 1, Zip5])
dt_tmp <- dt[year >= 2000 & multi_merge != 1 & Zip5 %in% treated_zips]
dt_tmp[,merge_firm_owned := as.integer(treated_overlap == 1)] 

post_names <- grep("^post_", names(dt), value = T)
dt_tmp[,post := 0]
dt_tmp[,post := as.integer(rowSums(.SD) == 1), .SDcols = post_names]

label_names <- grep("^merge_label_", names(dt), value = T)
dt_tmp[,merge_label := as.character(NA)]
for (label in label_names){
  dt_tmp[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt), value = T)
dt_tmp[,delta_hhi := 0]
for (hhi_col in hhi_names){
  dt_tmp[is.na(delta_hhi) | delta_hhi == 0, delta_hhi := get(hhi_col)]
}
reg0 <- felm(RentPrice ~ delta_hhi:post + delta_hhi:post:merge_firm_owned|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
reg1 <- felm(RentPrice ~ delta_hhi:post + delta_hhi:post:merge_firm_owned + sqft + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
reg2 <- felm(log_rent ~ delta_hhi:post + delta_hhi:post:merge_firm_owned|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
reg3 <- felm(log_rent ~ delta_hhi:post + delta_hhi:post:merge_firm_owned + sqft + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
reg4 <- felm(rent_sqft ~ delta_hhi:post + delta_hhi:post:merge_firm_owned|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
reg5 <- felm(rent_sqft ~ delta_hhi:post + delta_hhi:post:merge_firm_owned + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt_tmp)

out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                title = "2-Way FE with delta HHI and firm owned indicator, All Treated Zips"))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)
cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=F)

# 2-way FE with event-time dummies -- NOT FEASIBLE WHILE RENT COUNTS ARE STILL LOW
# reg0 <- felm(RentPrice ~ delta_hhi:post + delta_hhi:post:merge_firm_owned|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
# reg1 <- felm(RentPrice ~ delta_hhi:post + delta_hhi:post:merge_firm_owned + sqft + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
# reg2 <- felm(log_rent ~ delta_hhi:post + delta_hhi:post:merge_firm_owned|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
# reg3 <- felm(log_rent ~ delta_hhi:post + delta_hhi:post:merge_firm_owned + sqft + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
# reg4 <- felm(rent_sqft ~ delta_hhi:post + delta_hhi:post:merge_firm_owned|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
# reg5 <- felm(rent_sqft ~ delta_hhi:post + delta_hhi:post:merge_firm_owned + tot_unit_val|factor(year) + factor(id)|0|merge_label, data = dt_tmp)
# 
# out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
#                                 column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
#                                 title = "2-Way FE with delta HHI and firm owned indicator, All Treated Zips"))
# 
# # Wrap tabular environment in resizebox
# out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
# out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
# 
# # Set position
# out <- gsub("!htbp", "H", out, fixed = T)
# cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)


mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
# Each merger: basic DiD -- pre-post flags 
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id == 3){
    next 
  }
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
    
  # Relevant column names based on merge id
  treated_var <- paste0("treated_", merge_id)
  post_var <- paste0("post_", merge_id)
  dt[, treated := get(treated_var)]
  dt[, post := get(post_var)]
  
  # 1) Control = merging-zip non-affected 
  sample_var <- paste0("sample_", merge_id, "_c1")
  dt_tmp <- dt[year >= 2000 & get(sample_var) == 1]
  reg0 <- felm(RentPrice ~ treated + post + treated:post, data = dt_tmp)
  reg1 <- felm(RentPrice ~ treated + post + treated:post + sqft + tot_unit_val, data = dt_tmp)
  reg2 <- felm(log_rent ~ treated + post + treated:post, data = dt_tmp)
  reg3 <- felm(log_rent ~ treated + post + treated:post + sqft + tot_unit_val, data = dt_tmp)
  reg4 <- felm(rent_sqft ~ treated + post + treated:post, data = dt_tmp)
  reg5 <- felm(rent_sqft ~ treated + post + treated:post + tot_unit_val, data = dt_tmp)
  
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
  sample_var <- paste0("sample_", merge_id, "_c2")
  dt_tmp <- dt[year >= 2000 & get(sample_var) == 1]
  reg0 <- felm(RentPrice ~ treated + post + treated:post, data = dt_tmp)
  reg1 <- felm(RentPrice ~ treated + post + treated:post + sqft + tot_unit_val, data = dt_tmp)
  reg2 <- felm(log_rent ~ treated + post + treated:post, data = dt_tmp)
  reg3 <- felm(log_rent ~ treated + post + treated:post + sqft + tot_unit_val, data = dt_tmp)
  reg4 <- felm(rent_sqft ~ treated + post + treated:post, data = dt_tmp)
  reg5 <- felm(rent_sqft ~ treated + post + treated:post + tot_unit_val, data = dt_tmp)
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost Basic, Merger: ", merge_label,", Control: Outside-Zip Firm")))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
  
  # 3) Control = all properties unmerged zips with 1 firm
  sample_var <- paste0("sample_", merge_id, "_c3")
  dt_tmp <- dt[year >= 2000 & get(sample_var) == 1]
  reg0 <- felm(RentPrice ~ treated + post + treated:post, data = dt_tmp)
  reg1 <- felm(RentPrice ~ treated + post + treated:post + sqft + tot_unit_val, data = dt_tmp)
  reg2 <- felm(log_rent ~ treated + post + treated:post, data = dt_tmp)
  reg3 <- felm(log_rent ~ treated + post + treated:post + sqft + tot_unit_val, data = dt_tmp)
  reg4 <- felm(rent_sqft ~ treated + post + treated:post, data = dt_tmp)
  reg5 <- felm(rent_sqft ~ treated + post + treated:post + tot_unit_val, data = dt_tmp)
  
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

# Each merger: pre-post with reduced-form delta HHI and controls for merging firm owned and single-firm zips
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id == 3){
    next 
  }
  # Quadruple differences: exploit variation in cross-merge zip HHI changes, merging firm ownership, single-firm zips, and time
  treated_var <- paste0("treated_", merge_id)
  post_var <- paste0("post_", merge_id)
  hhi_var <- paste0("delta_hhi_", merge_id)
  sample_var1 <- paste0("sample_", merge_id, "_c1")
  sample_var2 <- paste0("sample_", merge_id, "_c3")
  treated_zips <- unique(dt[get(treated_var) == 1, Zip5])
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  
  dt[,treated_zip := 0]
  dt[Zip5 %in% treated_zips, treated_zip := 1]
  dt[, treated := get(treated_var)]
  dt[, post := get(post_var)]
  dt[, delta_hhi := get(hhi_var)]
  dt_tmp <- dt[year >= 2000 & (get(sample_var1) == 1 | get(sample_var2) == 1)]
  reg0 <- felm(RentPrice ~ treated_zip + post + delta_hhi + treated + post:treated_zip + post:treated +
                 post:treated_zip:delta_hhi + post:treated_zip:delta_hhi:treated, data = dt_tmp)
  reg1 <- felm(RentPrice ~ treated_zip + post + delta_hhi + treated + post:treated_zip + post:treated +
                 post:treated_zip:delta_hhi + post:treated_zip:delta_hhi:treated +
                 sqft + tot_unit_val, data = dt_tmp)
  reg2 <- felm(log_rent ~ treated_zip + post + delta_hhi + treated + post:treated_zip + post:treated +
                 post:treated_zip:delta_hhi + post:treated_zip:delta_hhi:treated, data = dt_tmp)
  reg3 <- felm(log_rent ~ treated_zip + post + delta_hhi + treated + post:treated_zip + post:treated +
                 post:treated_zip:delta_hhi + post:treated_zip:delta_hhi:treated +
                 sqft + tot_unit_val, data = dt_tmp)
  reg4 <- felm(rent_sqft ~ treated_zip + post + delta_hhi + treated + post:treated_zip + post:treated +
                 post:treated_zip:delta_hhi + post:treated_zip:delta_hhi:treated, data = dt_tmp)
  reg5 <- felm(rent_sqft ~ treated_zip + post + delta_hhi + treated + post:treated_zip + post:treated +
                 post:treated_zip:delta_hhi + post:treated_zip:delta_hhi:treated +
                 tot_unit_val, data = dt_tmp)
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                  column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                  title = paste0("Prepost Quad Differences, Merger: ", merge_label)))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "property_did.tex"), append=T)
}

