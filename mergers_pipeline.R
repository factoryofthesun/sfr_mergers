# ===================================================================
# Deduplication, Merge, and Collation Procedure for CL Deed and Tax Data
# ===================================================================
# When running from command line: 
#   "corelogic_pipeline.R [state abbrevation]
#
# Cleaning-specific parameters can be set in the corresponding sections.
#
# Output: merged CL panel data by APN * year 

rm(list=ls())

# Setting ===============================================
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(gridExtra)
library(zoo)
library(ggplot2); theme_set(theme_bw())
library(openxlsx)
library(readxl)
library(rlist)
library(bit64)
library(argparse)
setwd("~/project")

master_path <- "/gpfs/loomis/scratch60/humphries/rl874/"
zip_save_path <- "/project/humphries/jeh232/rent_project/data/corelogic/city_by_zip/tmp"
ct_save_path <- "/project/humphries/jeh232/rent_project/data/corelogic/city_by_ct/tmp"
micro_save_path <- "/project/humphries/jeh232/rent_project/data/corelogic/city/tmp"

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

tax_vars0 <- unlist(fread(paste0(master_path, "taxhist_var.csv"), select=1))
deed_vars0 <- unlist(fread(paste0(master_path, "deed_var.csv"), select=1))

tax_addr <- grep("^st_", as.vector(tax_vars0), value=T) # All relevant address fields
deed_addr <- grep("^st_", as.vector(deed_vars0), value=T) # All relevant address fields

tax_owner_addr <- grep("^owner_mail_", tax_vars0, value = T)
deed_owner_addr <- grep("^owner_mail_", deed_vars0, value = T)

# ============= (0) User Input =================================================
# Command Line Inputs ---------------------------------------------------
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-s", "--state", type="character", default="IL", 
                    help="State file to build [default %(default)s]",
                    metavar = "abbrev")
parser$add_argument("-c", "--cities", nargs="+", 
                    help = "City files to build for state [default \"%(default)s\"]")
parser$add_argument("-z", "--zip", action = "store_true", default=FALSE,
                    help="Whether to collapse output panel to zip codes [default %(default)s]")
parser$add_argument("-m", "--micro", action="store_true", default=FALSE,
                    help="Whether to save micro property data [default %(default)s]")
parser$add_argument("-t", "--tract", action = "store_true", default=FALSE,
                    help="Whether to collapse output panel to census tract [default %(default)s]")
parser$add_argument("-l", "--load", action = "store_true", default=FALSE,
                    help="Whether to load saved city-restricted data or read from raw files [default %(default)s]")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()

# Testing
#args <- parser$parse_args(c("-s", "IL", "-c", "CHICAGO", "PHOENIX", "GEORGIA", "--zip"))

# Set inputs
state <- args$state
cities <- gsub("_", " ", args$cities, fixed = T) # Needed to store multi-word city names with underscore
to_zip <- args$zip
to_ct <- args$tract
micro <- args$micro
l <- args$load

# Cores for parallelization
dedup_cores <- 10
if (state == "TX" | state == "FL"){
  collate_cores <- 1
  if (length(cities) == 0){
    collate_cores <- 1
  }
} else {
  dedup_cores <-10
  collate_cores <- 10
  if (length(cities) == 0){
    collate_cores <- 5
  }
}

# Reading Inputs -------------------------------------------------------------
tax_vars <- c("fips", "apn_unformatted", "apn_num", "tax_year",
              tax_addr, "ct", "block", tax_owner_addr,
              "condition", "prop_type", "year_built", "sale_price",
              "owner_corp", "abs_owner", "resid", "owner_rights", "doc_type",
              "tot_val_code", "tot_val", "assd_tot", "mkt_tot", "appr_tot", 
              "story_n", "tax_amt", "zoning", "land_use", "county_use1", "county_use2",
              "unit_n", "units_number", "univ_bldg_sqft", "bldg_sqft", "gross_sqft","adj_gross_sqft",
              "land_sqft", "acres", "trans_type", "owner1_ln", "owner1_fn", "owner2_ln", "owner2_fn", 
              "mtg_amt_1", "mtg_loan_type", "eff_year_built")
deed_vars <- c("fips", "apn_unformatted", "apn_num", "sale_dt", deed_owner_addr,
               deed_addr, "trans_type", "owner_rights","seller1_name", "mtg_deed_type",
               "resale_or_new", "foreclosure", "cash_sale", "const_loan", "prop_type",
               "owner_corp", "abs_owner", "owner_name", "owner1_ln", "owner1_fn", "owner2_ln", "owner2_fn",
               "resid", "deed_code", "doc_type", "prim_code", 
               "sale_price", "mtg_amt", "mtg_int_rate", "mtg_loan_type")

# Vars to cast to numeric before dedup
tax_num_vars <- c("tot_val", "assd_tot", "mkt_tot", "appr_tot",
                  "univ_bldg_sqft", "bldg_sqft", "gross_sqft", "adj_gross_sqft",
                  "land_sqft", "acres", 
                  "mtg_amt_1", "tax_amt")
deed_num_vars <- c("sale_price", "mtg_amt", "mtg_int_rate")

# Vars for maxchar dedup
tax_char_vars <- c("zoning", "land_use", "county_use1", "county_use2")

# Names of cleaned vars to keep track of
tax_to_clean <- c("owner_corp", "abs_owner", "resid", "year_built", 
                  "tot_val_code", "owner_rights", "condition",
                  "prop_type", "unit_n", "trans_type",
                  "units_number", "mtg_loan_type", "story_n", 
                  "doc_type")
tax_cleaned_cols <- paste0(tax_to_clean, "_clean")
deed_to_clean <- c("resale_or_new", "foreclosure", "cash_sale", "const_loan", 
                   "owner_corp", "abs_owner", "resid", 
                   "doc_type", "deed_code", "prop_type", 
                   "prim_code", "owner_rights",
                   "mtg_deed_type", "trans_type", "mtg_loan_type")
deed_cleaned_cols <- paste0(deed_to_clean, "_clean")

stopifnot(sum(duplicated(tax_vars)) == 0)
stopifnot(sum(duplicated(deed_vars)) == 0)

# Deduplication Inputs -------------------------------------------
id_vars <- c("fips", "apn_unformatted", "apn_num")
dedup_tax_vars <- c(tax_cleaned_cols, tax_addr, "st_ct", "st_map_block",
                    "eff_year_built", "owner1_ln", "owner1_fn", 
                    "owner2_ln", "owner2_fn", tax_num_vars, tax_char_vars)
dedup_tax_fns <- c(rep(c(argmax_na), length(dedup_tax_vars)-length(tax_num_vars)-length(tax_char_vars)), 
                   rep(c(mean_na), length(tax_num_vars)), rep(c(max_char), length(tax_char_vars)))

dedup_deed_vars <- c(deed_cleaned_cols, deed_addr,
                     "owner_name",  "owner1_ln", "owner1_fn", 
                     "owner2_ln", "owner2_fn", "seller1_name",
                     deed_num_vars)
dedup_deed_fns <- c(rep(c(argmax_na), length(dedup_deed_vars)-length(deed_num_vars)), rep(c(mean_na), length(deed_num_vars)))

# We will allow for duplicate collapse columns, as long as the dedup functions are different
# stopifnot(!any(c(duplicated(dedup_tax_vars), duplicated(dedup_deed_vars))))
stopifnot(length(dedup_tax_vars) == length(dedup_tax_fns))
stopifnot(length(dedup_deed_vars) == length(dedup_deed_fns))

dedup_deed_check <- paste0(dedup_deed_vars, dedup_deed_fns)
dedup_tax_check <- paste0(dedup_tax_vars, dedup_tax_fns)
stopifnot(all(!duplicated(dedup_deed_check)))
stopifnot(all(!duplicated(dedup_tax_check)))

# ============= (1) Read Tax/Deed Data ==========================================
# We will restrict the script to state-level parsing for now
print("Initial memory:")
getAvailMem()

# Read input files ---------------------------------------------------- 
files <- list.files(paste0(master_path, c("deed", "tax")))
tax_files <- paste0(master_path, "tax/", str_subset(files, paste0("Tax.+",state,"\\.txt")))
deed_files <- paste0(master_path, "deed/", str_subset(files, paste0("Deed.+", state, "\\.txt")))

tax_size <- sum(file.info(tax_files)$size)/1e9
deed_size <- sum(file.info(deed_files)$size)/1e9

print(paste("Total size of", state, "tax files:", tax_size, "GB"))
print(paste("Total size of", state, "deed files:", deed_size, "GB"))

# For testing
# tax_files <- tax_files[1]
# deed_files <- deed_files[1]

if (l == F){
  print("Reading from raw CL data...")
  t0 <- Sys.time()
  l_tax <- lapply(tax_files, function(x) cl_fread(x, tax_vars))
  tax <- rbindlist(l_tax, use.names=T, fill=T)
  
  l_deed <- lapply(deed_files, function(x) cl_fread(x, deed_vars))
  deed <- rbindlist(l_deed, use.names=T, fill=T)
  t1 <- Sys.time()
  print(paste("Reading data took", difftime(t1,t0,units='mins'), "minutes."))
  rm(l_tax)
  rm(l_deed)
} else{
  saved_raw <- paste0(master_path, "pipeline_raw/")
  print(paste0("Reading saved city-restricted data from ", saved_raw))
  files <- list.files(saved_raw)
  tax <- fread(paste0(saved_raw, state, "_tax.csv"), colClasses = "character")
  deed <- fread(paste0(saved_raw, state, "_deed.csv"), colClasses = "character")
}

print("Available memory after reading and binding")
getAvailMem() 

# Drop variables where fips or p_id is missing
print(paste("Out of ", nrow(deed), "rows in deeds data,"))
deed <- deed[!(is.na(fips)|fips==""|is.na(apn_unformatted)|apn_unformatted=="")]
print(paste("we will keep", nrow(deed), "rows with non-missing fips and apn_unformatted."))

print(paste("Out of ", nrow(tax), "rows in tax data,"))
tax <- tax[!(is.na(fips)|fips==""|is.na(apn_unformatted)|apn_unformatted=="")]
print(paste("we will keep", nrow(tax), "rows with non-missing fips and apn_unformatted."))

# Sometimes there are duplicate column names
deed[, which(duplicated(names(deed))) := NULL]
tax[, which(duplicated(names(tax))) := NULL]

# HOLD OFF ON THIS UNTIL AFTER WE'VE FIXED THE MASTER FILE
# ============= (2) Merge in Master ID/Address ==========================================
# master <- fread(paste0(master_path, "corelogic_smartystreet_cleaned.csv"))
# 
# # Clean colnames 
# colnames(master) <- gsub("\\[|\\]", "", names(master))
# 
# # For reference
# smty_addr_names <- names(master)[c(39:52,14:15)]
# ids <- c("id_cl", "apn_unformatted", "fips", "id_smty1", "id_smty2", "precision")
# id_addr_names <- c(ids, smty_addr_names)
# 
# master <- master[,..id_addr_names]

# Rename ct and block to identify with address vars
setnames(tax, c("ct", "block"), c("st_ct", "st_map_block"), skip_absent = T)

# Set empty strings in address columns to NA
for (c in c(tax_addr, "st_ct", "st_map_block", tax_owner_addr)){
  tax[get(c) == "", (c) := NA]
}
for (c in c(deed_addr, deed_owner_addr)){
  deed[get(c) == "", (c) := NA]
}

# Clean cities
deed[, st_city := trimws(gsub("[^A-Za-z[[:space:]]]", "", st_city))]
deed[, st_city := toupper(gsub("\\s\\s+", " ", st_city))]

tax[, st_city := trimws(gsub("[^A-Za-z[[:space:]]]", "", st_city))]
tax[, st_city := toupper(gsub("\\s\\s+", " ", st_city))]

# Restrict to cities
if (length(cities) > 0){
  deed <- deed[st_city %in% cities]
  tax <- tax[st_city %in% cities]
  
  if (l == F){
    # Save city-restricted data 
    fwrite(deed, paste0(master_path, "pipeline_raw/", state, "_deed.csv"))
    fwrite(tax, paste0(master_path, "pipeline_raw/", state, "_tax.csv"))
  }
}

# Clean variable levels
clean_tax(tax, tax_num_vars)
clean_deed(deed, deed_num_vars)

# Remove uncleaned variables
tax[,(tax_to_clean) := NULL]
deed[,(deed_to_clean) := NULL]

# Create temporary parcel identification variable ----------------------------
# Set IDs 
deed[,id := .GRP, by=id_vars]
tax[,id := .GRP, by=id_vars]

# ============= (2) Deduplication ==========================================
tax_n_row <- nrow(tax)
t10 <- Sys.time()
tax <- dedup(tax, id, tax_year, collp_vars = dedup_tax_vars, collp_fns = dedup_tax_fns, 
             set_key=T, hpc = "foreach", cores = dedup_cores)
# remove hpc = "foreach" option if not parallelizing
t11 <- Sys.time()
print(paste("Tax deduplication on", tax_n_row, "rows and", ncol(tax), "columns", 
            "took", difftime(t11,t10,units='mins'), "minutes."))

deed_n_row <- nrow(deed)
t20 <- Sys.time()
deed <- dedup(deed, id, sale_yr, collp_vars = dedup_deed_vars, collp_fns = dedup_deed_fns,
              set_key=T, hpc = "foreach", cores = dedup_cores)
t21 <- Sys.time()
print(paste("Deed deduplication on", deed_n_row, "rows and", ncol(deed), "columns", 
            "took", difftime(t21,t20,units='mins'), "minutes."))

print("Available memory:")
getAvailMem()

# ============= (3) Merge ==========================================
deed[,year:=sale_yr]
deed[,deed_event := 1] # deed event indicator
tax[,year:=as.integer(tax_year)]

# Set variables to keep in merge
deed_vars <- names(deed)
tax_vars <- names(tax)

t0 <- Sys.time() 
panel <- merge(deed[,..deed_vars], tax[,..tax_vars], 
               by=c(id_vars, "year"), all=T, suffixes = c("_deed", "_tax"))
t1 <- Sys.time()
print(paste("Merge with", nrow(panel), "rows and", ncol(panel),
            "columns took", difftime(t1, t0, units="mins"), "minutes."))
rm(deed)
rm(tax)
print("Available memory:")
getAvailMem()

# TODO: Merge in owner matched names ----------------------------------------


# ============= (4) Collate ====================================================================
# Cleaning -------------------------------------------------------------------- 
# Create new ID variable
panel[, `:=`(id = .GRP), by = id_vars]
panel[,`:=`(id_deed = NULL, id_tax =NULL)]

# Ensure data types 
panel[,year := as.integer(year)]
panel[,year_built_clean := as.integer(year_built_clean)]
panel[, deed_event := as.integer(deed_event)]

# All empty strings should be NA
panel[panel==""] <- NA

# Collate Inputs -------------------------------------------------------
key_vars <-c("n", "id", "year", "year_built_clean")

# Ranked fill 
collp_names <- grep("_deed$|_tax$", names(panel), value=T)
delin_vars <- unique(gsub("_deed$|_tax$", "", collp_names))
delin_vars <- delin_vars[delin_vars!="n"]
rfill_old <- lapply(delin_vars, function(x) paste0(x, c("_deed", "_tax")))
rfill_new <- c(delin_vars, "sqft", "mtg_amt", "land_sqft", "tot_val")
rfill_manual <- list(c("univ_bldg_sqft", "bldg_sqft", "gross_sqft", "adj_gross_sqft"), c("mtg_amt", "mtg_amt_1"), 
                     c("land_sqft", "acres"), c("tot_val", "assd_tot", "mkt_tot", "appr_tot"))
rfill_old <- c(rfill_old, rfill_manual)

# Grid expand
date_vars <- c("sale_yr", "sale_dt", "tax_year", "eff_year_built")
keep_vars <- c(date_vars, id_vars, "sale_price", "resale_or_new_clean", "foreclosure_clean", "cash_sale_clean", 
               "const_loan_clean", "n_deed", "n_tax", "deed_event", "trans_type_clean", "seller1_name",
               "seller1_name_clean", "seller1_name_bank")
misc_old_vars <- c("story_n_clean", "st_ct", "st_lat", "st_long", "st_num2", "tax_amt", "unit_n_clean")
new_vars <- unique(setdiff(c(rfill_new, misc_old_vars, "owner_name_bank", "owner_name_clean"), keep_vars))
full_vars <- c(id_vars, "sqft", "bldg_sqft_code", "tot_val", "land_sqft", "story_n_clean", "unit_n_clean", 
               "units_number_clean")

if (micro == T & length(cities) == 0){
  year_min <- 2000
} else{
  year_min <- 1970
}
year_max <- 2019
fill <- sapply(new_vars, function(x) ifelse(grepl("^st_", x), "full", ifelse(x %in% full_vars, "full", "deed")))

stopifnot(sum(duplicated(c(new_vars, keep_vars, key_vars))) == 0)
stopifnot(length(rfill_new) == length(rfill_old))

# Impute absentee owner -------------------------------------------------
# Concat key address fields
key_st_addr <- c("st_num1", "st_street", "st_city")
key_owner_addr <- c("owner_mail_num1", "owner_mail_street", "owner_mail_city")

panel[,bad_st := is.na(st_num1) | is.na(st_street) | is.na(st_city)]
panel[,bad_owner := is.na(owner_mail_num1) | is.na(owner_mail_street) | is.na(owner_mail_city)]

# NA units will be allowed to be empty string
for (c in c("st_unit", "owner_mail_unit")){
  panel[is.na(get(c)), (c) := ""]
}

panel[,st_key := paste(panel$st_num1, panel$st_street, panel$st_unit, panel$st_city)]
panel[,owner_key := paste(panel$owner_mail_num1, panel$owner_mail_street, panel$owner_mail_unit, panel$owner_mail_city)]

panel[bad_st == T, st_key := NA]
panel[bad_owner == T, owner_key := NA]

# Clean 
panel[,st_key := trimws(gsub("[^A-Za-z[:space:]]", "", st_key))]
panel[,st_key := toupper(gsub("\\s\\s+", " ", st_key))]
panel[,owner_key := trimws(gsub("[^A-Za-z[:space:]]", "", owner_key))]
panel[,owner_key := toupper(gsub("\\s\\s+", " ", owner_key))]

# Impute abs owners 
print("Original absentee distribution")
print(panel[,.N,abs_owner_clean])
panel[st_key != owner_key & is.na(abs_owner_clean), abs_owner_clean := "1"]
panel[st_key == owner_key & is.na(abs_owner_clean), abs_owner_clean := "0"]
print("Imputed absentee distribution")
print(panel[,.N,abs_owner_clean])

# If generating state micro data, then cut down size of data
if (micro == T & length(cities) == 0){
  panel[prop_type_clean %in% c("10", "11", "21", "22"), resid_clean := 1]
  
  # Residential properties
  resid_ids <- unique(panel[resid_clean == 1, id])
  panel <- panel[id %in% resid_ids]
  
  # Absentee owners
  panel <- panel[abs_owner_clean == "1"]
}
getAvailMem()
# Collapse columns ------------------------------------------------------
# Some tax vars need to be matched on sale year 
sale_match_vars <- c("sale_price", "trans_type_clean")
sale_var_inds <- grep(paste0(sale_match_vars, collapse="|"), rfill_new)
for (var in sale_match_vars){
  deed_var <- paste0(var, "_deed")
  tax_var <- paste0(var, "_tax")
  panel[,(var) := get(deed_var)]
  panel[(is.na(get(var))|get(var) == "") & sale_yr == year, (var) := get(tax_var)]
}
ranked_fill(panel, new_vars = rfill_new[-sale_var_inds], old_vars = rfill_old[-sale_var_inds])

# Impute owner name -----------------------------------------------------
# NA to empty string 
name_cols <- c("owner1_fn", "owner1_ln", "owner2_fn", "owner2_ln")
for (c in name_cols){
  panel[is.na(get(c)), (c) := ""]
}

panel[,owner_impute := trimws(paste(owner1_fn, owner1_ln))]
panel[owner_impute == "", owner_impute := trimws(paste(owner2_fn, owner2_ln))]
panel[is.na(owner_name)|owner_name=="", owner_name := owner_impute]

# Identify banks and adjust corporate owned flag ----------------------------------
identify_banks(panel, "owner_name") # Output indicator: owner_name_bank
print("Before adjusting for banks:")
print(panel[,.N,owner_corp_clean])
panel[owner_name_bank == TRUE, owner_corp_clean := "0"]
print("After bank adjustment:")
print(panel[,.N,owner_corp_clean])

identify_banks(panel, "seller1_name") # Output indicator: seller1_name_bank
# Collate --------------------------------------------------------------
# Don't parallel if collate_cores == 1
if (collate_cores == 1){
  parallel <- F
} else {parallel <- T}

t0 <- Sys.time()
panel <- coll_cols(panel, id, year, year_built_clean, deed_event,
                   new_vars = new_vars, year_min = year_min, year_max = year_max,
                   keep_vars = keep_vars, fill=fill,
                   parallel = parallel, cores = collate_cores)
t1 <- Sys.time()
print(paste("Collating panel with", nrow(panel), "rows and", ncol(panel),  
            "columns took", difftime(t1, t0, units="mins"), "minutes."))
print("Available mem:")
getAvailMem()

setcolorder(panel, c("id", "year", id_vars, "year_built_clean", sort(setdiff(new_vars, id_vars))))

# Misc data cleaning -------------------------------------------------------------------
# Inflation adjust sale price
price_adjust(panel, vars="sale_price", new_vars="sale_price")

# Residential prop types should have resid = 1
panel[prop_type_clean %in% c("10", "11", "21", "22"), resid_clean := 1]

# Create new variables --------------------------------------------------------------
# Define Zip5
panel[,Zip5 := substr(st_zip, 1, 5)]
panel[,Zip5 := as.integer(Zip5)]

# Define census tract and block
panel[,st_tract := substr(st_ct, 1,6)]
panel[,st_block := substr(st_ct, 7,10)]

# Clear the fields without the right character counts
panel[nchar(st_tract) != 6, st_tract := NA] 
panel[nchar(st_block) != 4, st_block := NA]

# Delineate foreclosures from REO sales
panel[, REO_Sale := F]
panel[seller1_name_bank == 1, REO_Sale := T] # If seller is a bank then it is REO sale
panel[REO_Sale == F, REO_Sale := (grepl("P", deed_code_clean, fixed=T) | trans_type_clean == "0") & owner_name_bank == F]
panel[, foreclosure_event := foreclosure_clean == 1 & REO_Sale == F]

# Get property value by unit
panel[,unit_n_clean := as.numeric(unit_n_clean)]
panel[,tot_val := as.numeric(tot_val)]

panel[is.na(unit_n_clean)|unit_n_clean < 1, unit_n_clean := 1]
panel[,tot_unit_val := tot_val/unit_n_clean]

# Set binary variables to numeric so we can average
binary_vars <- c("abs_owner_clean", "owner_corp_clean", "resid_clean", "resale_or_new_clean", 
                 "foreclosure_clean", "cash_sale_clean", "const_loan_clean", "foreclosure_event",
                 "owner_name_bank", "seller1_name_bank")
panel[,(binary_vars) := lapply(.SD, as.numeric), .SDcols = binary_vars]

# NA to 0 for specific binary variables
binary_na <- c("owner_corp_clean", "resid_clean", "foreclosure_clean")
for (var in binary_na){
  print(paste0("Imputing NA to 0 for ", var))
  print(panel[,.N,get(var)])
  panel[is.na(get(var)), (var) := 0]
}
# Save micro output -----------------------------------------------------------------
if (micro == T){
  if (length(cities) > 0){
    for (city in cities){
      temp <- panel[st_city == city]
      f <- paste0(city, ".csv")
      fwrite(temp, paste0(micro_save_path, "/", f))
    }
  }
  else {
    fwrite(panel, paste0("/gpfs/loomis/scratch60/humphries/rl874/pipeline_tmp/micro/", state, ".csv"))
  }
}