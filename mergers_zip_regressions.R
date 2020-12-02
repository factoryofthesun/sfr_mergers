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
dt <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")))
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
# Define new vars 
dt[,log_zori := log(ZORI)]
dt[,zori_sqft := ZORI/median_tot_unit_val]
dt[,monthyear := paste0(year, "-", month)]

#TODO: Figure out better way to deal with multi-merge zips 
dt[is.na(multi_merge), multi_merge := 0]

# Document dropped multi-merge zips 
n_multi_zips <- uniqueN(dt[multi_merge == 1, Zip5])
print(paste0("Dropping ", n_multi_zips, " zips with multi-merge."))

dt_multi_check <- dt[,.(median_median_rent = median_na0(median_rent)), .(multi_merge, year, month)]


# Remove multi-merge zips 
dt <- dt[multi_merge != 1]

# ============= (1) Zip Level Regressions ===================================================
# 2-way FE + delta HHI, each merger
for (merge_label in unique(dt$merge_label)){
  # Merger vars 
  month <- max(unique(mergers[label == merge_label, month_effective]))
  year <- unique(mergers[label == merge_label, year_effective])
  
  # Adjust panel depending on merger
  dt_tmp <- dt[affected == 1 & merge_label == merge_label & year >= 2000]
  dt_tmp[, post := 0]
  dt_tmp[year == year & month >= month, post := 1]
  dt_tmp[year > year, post := 1]

  reg0 <- felm(log_zori ~ delta_hhi*post|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  reg1 <- felm(log_zori ~ delta_hhi*post + sqft + tot_unit_val|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  reg2 <- felm(zori_sqft ~ delta_hhi*post|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  reg3 <- felm(zori_sqft ~ delta_hhi*post + tot_unit_val|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  

  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,
                        column.labels = c("Log Zori", "Zori/Sqft"), column.separate = c(2,2), title = "2-Way FE ZORI"))

  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "initial_zip_did.tex"), append=F)
}

# 2-way FE + delta HHI * year-month dummy
for (merge_label in unique(dt$merge_label)){
  # Merger vars 
  month <- max(unique(mergers[label == merge_label, month_effective]))
  year <- unique(mergers[label == merge_label, year_effective])
  
  # Adjust panel depending on merger
  dt_tmp <- dt[affected == 1 & merge_label == merge_label & year >= 2000]
  dt_tmp[, post := 0]
  dt_tmp[year == year & month >= month, post := 1]
  dt_tmp[year > year, post := 1]
  
  reg0 <- felm(log_zori ~ delta_hhi*post*factor(monthyear)|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  reg1 <- felm(log_zori ~ delta_hhi*post*factor(monthyear) + sqft + tot_unit_val|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  reg2 <- felm(zori_sqft ~ delta_hhi*post*factor(monthyear)|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  reg3 <- felm(zori_sqft ~ delta_hhi*post*factor(monthyear) + tot_unit_val|factor(monthyear) + factor(Zip5)|0|merge_label, data = dt_tmp)
  
  
  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,
                                  column.labels = c("Log Zori", "Zori/Sqft"), column.separate = c(2,2), title = "2-Way FE ZORI, Time-varying effects"))
  
  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "initial_zip_did.tex"), append=T)
}

