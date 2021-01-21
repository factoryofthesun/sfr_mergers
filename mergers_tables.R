# ===================================================================
# Mergers Analysis: Tables and Figures
# ===================================================================
# Tables and figures for final merger paper

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
library(stringi)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")
Sys.umask(000)

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"

t0 <- Sys.time()
dt <- fread(paste0(data_path, "panel_hhi_rent.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")))
dt_zip <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character")
t1 <- Sys.time() 

mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))

for (id in 1:5){
  target_name <- unique(mergers[MergeID_1 == id, TargetName])
  post_col <- paste0("post_", id)
  print(target_name)
  print(paste0("Pre counts: ", uniqueN(dt[Merger_Owner_Fill %in% target_name & get(post_col) == 0, id])))
  print(paste0("Post counts: ", uniqueN(dt[Merger_Owner_Fill %in% target_name & get(post_col) == 1, id])))
  
}

# Drop outlier rents 
n_bad_rents <- nrow(dt[RentPrice >= 1e5 | RentPrice <= 100])
print(paste0("Dropping ", n_bad_rents, " rows with rent higher than 100k and lower than 100"))
dt <- dt[RentPrice < 1e5 & RentPrice > 100]

# Table 1 + 2 : Mergers Summary and Concentration 
# Get means for each merger
for (id in 1:5){
  # Need to do Beazer/Ellington separately for first merger 
  if (id == 1){
      acquiror_name <- mergers[1, AcquirorName]
      target_name1 <- mergers[1, TargetName]
      target_name2 <- mergers[2, TargetName]
      target_name_concat <- paste0(target_name1, "-", target_name2)
      target_name <- c(target_name1, target_name2)
      print(paste0("Summary for targets: ", target_name_concat, "----------------------"))
      
      # Number of properties
      n_target1 <- uniqueN(dt[Merger_Owner_Fill == target_name1, id])
      n_target2 <- uniqueN(dt[Merger_Owner_Fill == target_name2, id])
      n_acquiror <- uniqueN(dt[Merger_Owner_Fill == acquiror_name, id])
      print(paste0("# Beazer: ", n_target1))
      print(paste0("# Ellington: ", n_target2))
      print(paste0("# acquiror: ", n_acquiror))
    } else{
    i <- id + 1
    acquiror_name <- mergers[i, AcquirorName]
    target_name <- mergers[i, TargetName]
    print(paste0("Summary for target: ", target_name, "----------------------"))
    
    # Number of properties
    n_target <- uniqueN(dt[Merger_Owner_Fill == target_name, id])
    n_acquiror <- uniqueN(dt[Merger_Owner_Fill == acquiror_name, id])
    print(paste0("# target: ", n_target))
    print(paste0("# acquiror: ", n_acquiror))
  }
  # Prepost firm mean rent 
  post_col <- paste0("post_", id)
  target_pre_rent <- mean_na(dt[Merger_Owner_Fill %in% target_name & get(post_col) == 0, RentPrice])
  target_post_rent <- mean_na(dt[Merger_Owner_Fill %in% target_name & get(post_col) == 1, RentPrice])
  target_pre_rent_sd <- sd(dt[Merger_Owner_Fill %in% target_name & get(post_col) == 0, RentPrice], na.rm=T)
  target_post_rent_sd <- sd(dt[Merger_Owner_Fill %in% target_name & get(post_col) == 1, RentPrice], na.rm=T)
  
  print(paste0("Target pre-rent: ", target_pre_rent))
  print(paste0("Target pre-rent SD: ", target_pre_rent_sd))
  print(paste0("Target post-rent: ", target_post_rent))
  print(paste0("Target post-rent SD: ", target_post_rent_sd))
  
  acq_pre_rent <- mean_na(dt[Merger_Owner_Fill == acquiror_name & get(post_col) == 0, RentPrice])
  acq_post_rent <- mean_na(dt[Merger_Owner_Fill == acquiror_name & get(post_col) == 1, RentPrice])
  acq_pre_rent_sd <- sd(dt[Merger_Owner_Fill == acquiror_name & get(post_col) == 0, RentPrice], na.rm=T)
  acq_post_rent_sd <- sd(dt[Merger_Owner_Fill == acquiror_name & get(post_col) == 1, RentPrice], na.rm=T)
  
  print(paste0("Acquiror pre-rent: ", acq_pre_rent))
  print(paste0("Acquiror pre-rent SD: ", acq_pre_rent_sd))
  
  print(paste0("Acquiror post-rent: ", acq_post_rent))
  print(paste0("Acquiror post-rent SD: ", acq_post_rent_sd))
  
  # Prepost merger ZORI
  treated_col <- paste0("treated_", id)
  treated_pre_rent <- mean_na(dt_zip[get(treated_col) == 1 & get(post_col) == 0, ZORI])
  treated_pre_sd <- sd(dt_zip[get(treated_col) == 1 & get(post_col) == 0, ZORI], na.rm=T)
  
  treated_post_rent <- mean_na(dt_zip[get(treated_col) == 1 & get(post_col) == 1, ZORI])
  treated_post_sd <- sd(dt_zip[get(treated_col) == 1 & get(post_col) == 1, ZORI], na.rm=T)
  
  print(paste0("Treated pre-ZORI: ", treated_pre_rent))
  print(paste0("Treated pre-sd: ", treated_pre_sd))
  print(paste0("Treated post-ZORI: ", treated_post_rent))
  print(paste0("Treated post-sd: ", treated_post_sd))
  
  # Acquiror/target shares
  acq_col <- paste0("acquiror_share_", id)
  target_col <- paste0("target_share_", id)
  
  target_share <- mean_na(dt[get(target_col) != 0 & !is.na(get(target_col)), .(share = median_na(get(target_col))), Zip5][,share])
  acquiror_share <- mean_na(dt[get(acq_col) != 0 & !is.na(get(acq_col)), .(share = median_na(get(acq_col))), Zip5][,share])
  
  target_share_sd <- sd(dt[get(target_col) != 0 & !is.na(get(target_col)), .(share = median_na(get(target_col))), Zip5][,share], na.rm=T)
  acquiror_share_sd <- sd(dt[get(acq_col) != 0 & !is.na(get(acq_col)), .(share = median_na(get(acq_col))), Zip5][,share], na.rm=T)
  
  print(paste0("Target share: ", target_share))
  print(paste0("Target share SD: ", target_share_sd))
  print(paste0("Acquiror share: ", acquiror_share))
  print(paste0("Acquiror share SD: ", acquiror_share_sd))
  
  # Prepost HHI 
  treated_pre_hhi <- mean_na(dt_zip[get(treated_col) == 1 & get(post_col) == 0, hhi])
  treated_pre_sd <- sd(dt_zip[get(treated_col) == 1 & get(post_col) == 0, hhi], na.rm=T)
  
  treated_post_hhi <- mean_na(dt_zip[get(treated_col) == 1 & get(post_col) == 1, hhi])
  treated_post_sd <- sd(dt_zip[get(treated_col) == 1 & get(post_col) == 1, hhi], na.rm=T)
  
  print(paste0("Treated pre-HHI: ", treated_pre_hhi))
  print(paste0("Treated pre-sd: ", treated_pre_sd))
  print(paste0("Treated post-HHI: ", treated_post_hhi))
  print(paste0("Treated post-sd: ", treated_post_sd))
  
  # Control zips HHI 
  sample_col <- paste0("sample_", id)
  control_pre_hhi <- mean_na(dt_zip[get(sample_col) == 1 & get(treated_col) == 0 & get(post_col) == 0, hhi])
  control_pre_sd <- sd(dt_zip[get(sample_col) == 1 & get(treated_col) == 0 & get(post_col) == 0, hhi], na.rm=T)
  control_post_hhi <- mean_na(dt_zip[get(sample_col) == 1 & get(treated_col) == 0 & get(post_col) == 1, hhi])
  control_post_sd <- sd(dt_zip[get(sample_col) == 1 & get(treated_col) == 0 & get(post_col) == 1, hhi], na.rm=T)
  
  print(paste0("Control pre-HHI: ", control_pre_hhi))
  print(paste0("Control pre-sd: ", control_pre_sd))
  print(paste0("Control post-HHI: ", control_post_hhi))
  print(paste0("Control post-sd: ", control_post_sd))
}

# All outside zip HHI
treated_names <- grep("^treated_", names(dt_zip), value=T)
dt_zip[,treated_overlap := rowSums(.SD, na.rm = T), .SDcols = treated_names]

all_hhi <- mean_na(dt_zip[treated_overlap == 0, hhi])
all_sd <- sd(dt_zip[treated_overlap == 0, hhi], na.rm=T)
print(paste0("All outside HHI: ", all_hhi))
print(paste0("All outside SD: ", all_sd))
