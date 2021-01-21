# ===================================================================
# Mergers Try Different Imputations
# ===================================================================
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
library(stringi)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")
Sys.umask(000)

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"

# Read and concat merger identified panel data
files <- paste0(data_path, grep("_mergers\\.csv$", list.files(data_path), value = T))
size <- sum_na(file.info(files)$size)/1e9
print(paste("Total size of merger state files:", size, "GB"))

t0 <- Sys.time()
micro_list <- lapply(files, function(x) fread(x, integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted"))))
# micro_list <- lapply(files, function(x) fread(x, integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")),nrows=1e5))
dt <- rbindlist(micro_list, fill = T)
rm(micro_list)
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# ============= (0) Preprocessing ===================================================
print(paste0("Pre-Impute rows with NA Zip5: ", nrow(dt[is.na(Zip5)])))
# Impute zips (not sure if necessary)
dt[, Zip5 := argmax_na(Zip5), id]
dt[, Zip5 := as.integer(Zip5)]
print(paste0("Post-Impute rows with NA Zip5: ", nrow(dt[is.na(Zip5)])))

# Drop missing Zip5 and out of bounds years
dt <- dt[!is.na(Zip5) & year <= 2020 & year >= 2000]

# Redefine id
dt[, id := .GRP, .(fips, apn_unformatted, apn_num)]

# Restrict to only SFR properties
sfr_ids <- unique(dt[prop_type_clean %in% c(10, 11), id])
dt <- dt[id %in% sfr_ids]

# Drop low property count zips 
bad_zips <- dt[,.(N = uniqueN(id)), .(Zip5)][N < 50, Zip5]
print(paste0("Dropping ", nrow(dt[Zip5 %in% bad_zips]), " rows of zips with under 50 residential properties out of ", 
             nrow(dt), " total rows."))
dt <- dt[!(Zip5 %in% bad_zips)]

# Clean variables -------------------------------------------
# Data types
dt[,year := as.integer(year)]
dt[,Zip5 := as.integer(Zip5)]

# Check and clean Beazer properties
beazer_search <- c("rental", "pre owned", "preowned")
print(paste0("Cleaning Beazer property identifiation. Pre-counts: ", uniqueN(dt[Merger_Owner_Impute == "Beazer Pre-Owned Rental Homes", id])))
dt[Merger_Owner_Impute == "Beazer Pre-Owned Rental Homes" & 
     grepl(paste0(beazer_search, collapse = "|"), owner_name_clean, ignore.case = T), good_beazer := 1]
dt[Merger_Owner_Name == "Beazer Pre-Owned Rental Homes", Merger_Owner_Name := NA]
dt[good_beazer == 1, Merger_Owner_Name := "Beazer Pre-Owned Rental Homes"]
print(paste0("Post-counts: ", uniqueN(dt[good_beazer == 1, id])))

# Impute owner names using smty addresses 
dt[is.na(Merger_Owner_Name), Merger_Owner_Name := ""]
for (name in unique(dt[Merger_Owner_Name != "", Merger_Owner_Name])){
  print(paste0("Pre-Impute Top owners for: ", name))
  print(dt[Merger_Owner_Name == name,.N, owner_name_clean][order(-N)][1:10])
}

# Imputation procedures for address ------------------------------------------------------------------------
print("Imputation address procedures -------------------------------------------------")
# Don't touch addresses associated with multiple firms 
dt[Merger_Owner_Name != "" & owner_smty_addr != "", n_merge_owned := uniqueN(Merger_Owner_Name), owner_smty_addr]
single_owner_addr <- unique(dt[n_merge_owned == 1, owner_smty_addr])
print(paste0("% merger properties associated with multi-address: ", 
             nrow(dt[Merger_Owner_Name != "" & !(owner_smty_addr %in% single_owner_addr)])/nrow(dt[Merger_Owner_Name != ""])))

print("Addr imputation 1: only single owner addresses --------------------------------------")
dt[,Merger_Owner_Impute := Merger_Owner_Name]
dt[(owner_smty_addr %in% single_owner_addr) & owner_smty_addr != "", 
   Merger_Owner_Impute := Merger_Owner_Name[Merger_Owner_Name != ""][1], owner_smty_addr]

dt[is.na(Merger_Owner_Impute), Merger_Owner_Impute := ""]
for (name in unique(dt[Merger_Owner_Impute != "", Merger_Owner_Impute])){
  print(paste0("Post-Impute Top owners for: ", name))
  print(dt[Merger_Owner_Impute == name,.N, owner_name_clean][order(-N)][1:10])
}
print("Post-Impute Properties with Rent")
print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Impute])

print("Addr imputation 2: only single owner addresses, exclude institution addr --------------------------------------")
bad_addr <- unique(dt[(owner_name_clean_bank == 1 | owner_name_clean_gov == 1 | owner_builder == 1 | owner_law == 1), owner_smty_addr])

dt[,Merger_Owner_Impute2 := Merger_Owner_Name]
dt[(owner_smty_addr %in% single_owner_addr) & owner_smty_addr != "" & !(owner_smty_addr %in% bad_addr), 
   Merger_Owner_Impute2 := Merger_Owner_Name[Merger_Owner_Name != ""][1], owner_smty_addr]

dt[is.na(Merger_Owner_Impute2), Merger_Owner_Impute2 := ""]
for (name in unique(dt[Merger_Owner_Impute2 != "", Merger_Owner_Impute2])){
  print(paste0("Post-Impute Top owners for: ", name))
  print(dt[Merger_Owner_Impute2 == name,.N, owner_name_clean][order(-N)][1:10])
}
print("Post-Impute Properties with Rent")
print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Impute2])

print("Addr imputation 3: allow multi-owner address, as long as NOT involved in same merger --------------------------------------")
# Don't touch addresses associated with multiple, same-merger firms 
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
bad_addr <- c() 
for (i in 1:nrow(mergers)){
  target_name <- mergers[i, TargetName]
  acquiror_name <- mergers[i, AcquirorName]
  target_addr <- unique(dt[Merger_Owner_Name == target_name, owner_smty_addr])
  acquiror_addr <- unique(dt[Merger_Owner_Name == acquiror_name, owner_smty_addr])
  bad_addr <- unique(c(bad_addr, intersect(target_addr, acquiror_addr)))
}

print(paste0("% merger properties associated with same-merge multi-address: ", 
             nrow(dt[Merger_Owner_Name != "" & (owner_smty_addr %in% bad_addr)])/nrow(dt[Merger_Owner_Name != ""])))

dt[,Merger_Owner_Impute3 := Merger_Owner_Name]
dt[!(owner_smty_addr %in% bad_addr) & owner_smty_addr != "", 
   Merger_Owner_Impute3 := Merger_Owner_Name[Merger_Owner_Name != ""][1], owner_smty_addr]

dt[is.na(Merger_Owner_Impute3), Merger_Owner_Impute3 := ""]
for (name in unique(dt[Merger_Owner_Impute3 != "", Merger_Owner_Impute3])){
  print(paste0("Post-Impute Top owners for: ", name))
  print(dt[Merger_Owner_Impute3 == name,.N, owner_name_clean][order(-N)][1:10])
}
print("Post-Impute Properties with Rent")
print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Impute3])

# Front-fill imputation procedures ---------------------------------------------------------
print("ID frontfill procedures -------------------------------------------------")

for (col in c("Merger_Owner_Impute", "Merger_Owner_Impute2", "Merger_Owner_Impute3")){
  print(paste0("Front fill options for: ", col))
  print("Fronfill 1: Merger Name never changes after first appearance ------------")
  # Merger owner name shouldn't change after first appearance
  setorder(dt, id, year)
  id_change <- dt[,c(TRUE, id[-1] != id[-.N])]
  dt[, Merger_Owner_Fill := get(col)]
  dt[,merge_own_cum := cumsum(get(col) != ""), id]
  dt[merge_own_cum > 0, Merger_Owner_Fill := Merger_Owner_Fill[1], id]
  print("Post-Fill Properties with Rent")
  print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Fill])
  
  print("Fronfill 2: Argmax Merger Name UNLESS target and acquiror both present ------------")
  bad_id <- c()
  for (i in 1:nrow(mergers)){
    target_name <- mergers[i, TargetName]
    acquiror_name <- mergers[i, AcquirorName]
    target_id <- unique(dt[get(col) == target_name, id])
    acquiror_id <- unique(dt[get(col) == acquiror_name, id])
    bad_id <- unique(c(bad_id, intersect(target_id, acquiror_id)))
  }
  # Frontfill bad ids
  setorder(dt, id, year)
  id_change <- dt[,c(TRUE, id[-1] != id[-.N])]
  dt[, Merger_Owner_Fill := get(col)]
  dt[,merge_own_cum := cumsum(get(col) != ""), id]
  dt[id %in% bad_id & merge_own_cum > 0, Merger_Owner_Fill := Merger_Owner_Fill[1], id]
  
  # Argmax rest after first appearance
  all_merge_id <- unique(dt[get(col) != "" & !is.na(get(col)), id])
  dt[(id %in% all_merge_id) & !(id %in% bad_id) & merge_own_cum > 0, 
     Merger_Owner_Fill := argmax_na(.SD[Merger_Owner_Fill != "", Merger_Owner_Fill]), id]
  
  print("Post-Fill Properties with Rent")
  print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Fill])
  print("------------------------------------------------------------------------------")
}