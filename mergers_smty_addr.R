# ===================================================================
# Mergers Project: Smarty streets address merging 
# ===================================================================
rm(list=ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(bit64)
library(argparse)
setwd("~/project")
Sys.umask(000)

mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
mergers_data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
smty_path <- "/gpfs/loomis/scratch60/humphries/rl874/address/owner_mail/state/"

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

# Command Line Inputs ---------------------------------------------------
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-s", "--state", type="character", default="IL", 
                    help="State file to build [default %(default)s]",
                    metavar = "abbrev")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()

# Set inputs
state <- args$state

# Read owner-expanded smty
smty <- fread(paste0(smty_path, "cl_owner_smty_expand_", state, ".csv"), colClasses=list(character = "apn_unformatted"))
smty[,year := year_impute]

# Merge into mergers full panel file ---------------------------------------------------
dt <- fread(paste0(mergers_data_path, state, "_full.csv"), colClasses=list(character = "apn_unformatted"))
dt <- merge(dt, smty, by = c("fips", "apn_unformatted", "apn_num", "year"), all.x=T)
print(paste0("Unmatched id-years for ", state, ": ", nrow(dt[is.na(id_apn_st)])))

# Concatenate address fields
smty_fields <- c("smty_owner_street1", "smty_owner_street2", "smty_owner_zip4", "smty_owner_city")
for (c in smty_fields){
  dt[,(c) := as.character(get(c))]
  dt[is.na(get(c)), (c) := ""]
}
dt[, bad_smty := 0]
dt[smty_owner_street1 == ""|smty_owner_zip4 == ""|smty_owner_city=="", bad_smty := 1]
dt[,owner_smty_addr := paste(smty_owner_street1, smty_owner_street2, smty_owner_zip4, smty_owner_city)]
dt[bad_smty == 1, owner_smty_addr := ""]

print(paste0("Rows with smarty address pre-impute: ", nrow(dt[owner_smty_addr != ""]), " out of ", nrow(dt), " rows."))

# Impute missing with CL owner address
dt[,owner_mail_zip5 := substr(owner_mail_zip, 1,5)]
cl_fields <- c("owner_mail_num1", "owner_mail_direc", "owner_mail_street", "owner_mail_mode", "owner_mail_unit", "owner_mail_zip5",
               "owner_mail_city")
for (c in cl_fields){
  dt[,(c) := as.character(get(c))]
  dt[is.na(get(c)), (c) := ""]
}
dt[, bad_cl := 0]
dt[owner_mail_num1 == ""|owner_mail_street == ""|owner_mail_zip == ""|owner_mail_city=="", bad_cl := 1]
dt[,owner_cl_addr := paste(owner_mail_num1, owner_mail_direc, owner_mail_street, owner_mail_mode, 
                           owner_mail_unit, owner_mail_zip5, owner_mail_city)]
dt[bad_cl == 1, owner_cl_addr := ""]
dt[bad_smty == 1 & bad_cl != 1, owner_smty_addr := owner_cl_addr]

addr_vars <- c("owner_smty_addr", "owner_cl_addr")
dt[, (addr_vars) := lapply(.SD, function(x) {
  gsub("\\s\\s+", " ", trimws(gsub("[[:punct:]]", "", toupper(x))))
}), .SDcols = addr_vars]

dt[is.na(owner_smty_addr), owner_smty_addr := ""]
dt[is.na(owner_cl_addr), owner_cl_addr := ""]

print(paste0("Rows with smarty address post-impute: ", nrow(dt[owner_smty_addr != ""]), " out of ", nrow(dt), " rows."))
print(paste0("Rows with owner name: ", nrow(dt[owner_name_clean != "" & !is.na(owner_name_clean)])))

# Identify merger owners using mailing addresses 
print("Pre-impute merger owner counts")
print(dt[,.N,Merger_Owner_Name])
dt[Merger_Owner_Name != "" & owner_smty_addr != "", n_merge_owned := uniqueN(Merger_Owner_Name), owner_smty_addr]
print("Addresses associated with multiple merge owned")
print(dt[n_merge_owned > 1, .N, .(owner_smty_addr, n_merge_owned)][order(-N)])

dt[owner_smty_addr != "", Merger_Owner_Impute := argmax_na(.SD[Merger_Owner_Name != "", Merger_Owner_Name]), owner_smty_addr]
dt[is.na(Merger_Owner_Impute), Merger_Owner_Impute := Merger_Owner_Name]
print("Post-impute merger owner counts")
print(dt[,.N,Merger_Owner_Impute])

# Overwrite mergers restricted panels 
dt[Merger_Owner_Impute != "", merge_affected := 1]
relevant_zips <- unique(dt[merge_affected == 1, Zip5])
fwrite(dt[Zip5 %in% relevant_zips], paste0(mergers_data_path, state, "_mergers.csv"))
