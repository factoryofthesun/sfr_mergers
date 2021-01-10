# ===================================================================
# MLS Pipeline Hack to Update Mergers Tables
# ===================================================================
# We will try merging MLS prices again into the mergers tables excluding the apn_num match 
rm(list=ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(bit64)
library(argparse)
setwd("~/project")
Sys.umask(000)

mergers_data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
scratch_path <- "/gpfs/loomis/scratch60/humphries/rl874/"

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

# ============= (0) User Input =================================================
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

# MLS Cleaning Inputs -----------------------------------------------
id_vars <- c("CMAS_FIPS_CODE", "CMAS_PARCEL_ID")
date_vars <- c("FA_ContractDate", "LeaseDate", "FA_CloseDate", "CloseDate", 
               "PurchaseContractDate", "ContingentDate", "FA_ListDate",
               "ListDate", "FA_OriginalListDate",  "OriginalListDate",
               "FA_PostDate", "FA_EntryDate")
p_vars <- c("FA_Rent_Sale_ind", "ListPrice", "ClosePrice", "RentPrice")

# Merge Inputs -------------------------------------------------------------
id0 <- c("fips", "apn_unformatted")  # id vars in existing data set
id1 <- id_vars

# ============= (1) Read CL and MLS Data =========================================
# We will restrict the script to state-level parsing for now
print("Initial memory:")
getAvailMem()

cl <- fread(paste0(mergers_data_path, state, "_mergers_impute.csv"), colClasses = list(character = c(id0)))

print("After reading CoreLogic:")
getAvailMem()

mls <- fread(paste0(scratch_path, "mls/quickhistory_state/mls_quickhistory_", state, ".csv"), 
             colClasses = list(character = c("CMAS_Zip5", id_vars)))
print("After reading MLS:")
getAvailMem()

# ============== (1) Clean CL owner names ====================================================
# Recreate owner impute variable
name_cols <- c("owner1_fn", "owner1_ln", "owner2_fn", "owner2_ln")
for (c in name_cols){
  cl[is.na(get(c)), (c) := ""]
}

cl[,owner_impute_fill := trimws(paste(owner1_fn, owner1_ln))]
cl[owner_impute_fill == "" | is.na(owner_impute_fill), owner_impute := trimws(paste(owner2_fn, owner2_ln))]

print(paste0("Old # owner names: ", uniqueN(cl$owner_name_clean)))

# Completely wipe all names from imputation
unique_impute_names <- unique(cl$owner_impute_fill)
cl[owner_name_clean %in% owner_impute_fill, owner_name_clean := NA]
cl[owner_name_clean == "", owner_name_clean := NA]

# Frontfill/backfill owner name 
setorder(cl, id, year)
id_change <- cl[,c(TRUE, id[-1] != id[-.N])]

cl[, owner_name_clean := owner_name_clean[cummax((!is.na(owner_name_clean) | id_change) * .I)]]
cl[order(id, -year), owner_name_clean := owner_name_clean[cummax((!is.na(owner_name_clean) | id_change) * .I)]]

print(paste0("New # owner names: ", uniqueN(cl$owner_name_clean)))

# ============== (2) Clean MLS ====================================================
mls_vars <- names(mls)

# date cleaning
ranked_fill(mls, "mls_date", list(date_vars))
mls[, mls_date := ymd_hms(mls_date)]
mls[, mls_year := year(mls_date)]

# zip cleaning
mls[CMAS_Zip5 == "", CMAS_Zip5 := NA]

# Tract cleaning 
mls[,CensusTract := as.numeric(CensusTract)]
mls[, st_tract := as.character(as.integer(CensusTract * 100))]
mls[is.na(st_tract)|st_tract=="", st_tract := CMAS_TRACT_NBR] # Supplement with CMAS Tract Number
mls[st_tract == "", st_tract := NA]
mls[, st_tract := substr(st_tract, 1, 6)]
mls[nchar(st_tract) > 6, st_tract := NA]

n_bad_tracts <- nrow(mls[is.na(st_tract)|st_tract==""])
print(paste0("There are ", n_bad_tracts, " rows with missing census tract values."))

# price cleaning
mls_p <- mls[!is.na(CMAS_FIPS_CODE) & CMAS_PARCEL_ID != "", .SD, .SDcols = c(id_vars, "mls_year", p_vars, "CMAS_Zip5", "st_tract")]
mls_p[FA_Rent_Sale_ind == "", FA_Rent_Sale_ind := "NA"]

# Enforce 1 zip code/census tract per property
mls_p[, temp_id := .GRP, by = c(id_vars, "FA_Rent_Sale_ind")]
mls_p[,CMAS_Zip5 := argmax_na(CMAS_Zip5), by = id_vars]
mls_p[,st_tract := argmax_na(st_tract), by = id_vars]

# dedup
mls_p <- dedup(mls_p, temp_id, mls_year, c("ListPrice", "ClosePrice", "RentPrice"),
               c(mean_na0, mean_na0, mean_na0), hpc = "foreach", cores = 5)

# price for sale or rent
mls_p <- dcast(mls_p, CMAS_FIPS_CODE+CMAS_PARCEL_ID+mls_year+CMAS_Zip5+st_tract~FA_Rent_Sale_ind,
               value.var = c("ListPrice", "ClosePrice", "RentPrice"))

# Sanity checks: quantiles of price variables
p_vars <- grep("Price", names(mls_p), fixed = T, value = T)
quart_dt <- data.table()
for (p in p_vars){
  tmp <- data.table(price = quantile(mls_p[,get(p)], na.rm=T))
  setnames(tmp, "price", p)
  quart_dt <- cbind(quart_dt, tmp)
}
print("Price quartiles:")
print(quart_dt)

# Clean price columns ---------------------------------------------------------------
mls_p[,`:=`(SalePrice = ClosePrice_S, RentPrice = ClosePrice_R)]
mls_p[is.na(SalePrice)|SalePrice == 0, SalePrice := ListPrice_S]
for (c in c("ListPrice_R", "RentPrice_R", "RentPrice_S")){
  mls_p[is.na(RentPrice)|RentPrice == 0, RentPrice := get(c)]
}
mls_p[SalePrice == 0, SalePrice := NA]
mls_p[RentPrice == 0, RentPrice := NA]

# Inflation adjust prices
p_vars_all <- grep("price|Price", names(mls_p), value=T)
price_adjust(mls_p, vars = p_vars_all, new_vars = p_vars_all, yr_var = "mls_year")

# Define profit margin
mls_p[,ProfitMargin := RentPrice/SalePrice]
mls_p[is.infinite(ProfitMargin), ProfitMargin := NA]

# ============== (3) Merge CL and MLS =============================================
t0 <- Sys.time()
# Print original RentPrice coverage and remove existing MLS columns
print(paste0("Original RentPrice count: ", nrow(cl[!is.na(RentPrice) & RentPrice != 0])))
mls_cols <- setdiff(names(mls_p), c(id0, "year"))
cl[,(mls_cols) := NULL]

mls_p[,mls_year := as.integer(mls_year)]
cl[,year := as.integer(year)]
cl <- merge(cl, mls_p, by.x = c(id0, "year"), by.y = c(id_vars, "mls_year"), all = T, 
            suffixes = c("_cl", "_mls"))
t1 <- Sys.time()
print("merge took:")
print(t1-t0)
print("Available Memory:")
getAvailMem()

# Supplement with CL sale_price
cl[is.na(SalePrice)|SalePrice == 0, SalePrice := sale_price]
cl[RentPrice == 0, RentPrice := NA]
cl[SalePrice == 0, SalePrice := NA]

# Redefine ProfitMargin 
cl[,ProfitMargin := RentPrice/SalePrice]
cl[is.infinite(ProfitMargin), ProfitMargin := NA]

# New rent counts
print(paste0("New RentPrice count: ", nrow(cl[!is.na(RentPrice) & RentPrice != 0])))

# Save
fwrite(cl[!is.na(id)], paste0(mergers_data_path, state, "_mergers_mls.csv"))