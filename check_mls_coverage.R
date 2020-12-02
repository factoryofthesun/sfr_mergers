# Check price coverage of MLS data -- how balanced is our panel?
rm(list = ls())
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

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

scratch_path <- "/gpfs/loomis/scratch60/humphries/rl874/"
zip_pipeline_path <- "/project/humphries/jeh232/rent_project/data/corelogic/city_by_zip/"
ct_pipeline_path <- "/project/humphries/jeh232/rent_project/data/corelogic/city_by_ct/"
micro_pipeline_path <- "/project/humphries/jeh232/rent_project/data/corelogic/city/"
plot_path <- "~/scratch60/mergers/price_coverage/"

states <- c("AZ", "TX", "FL", "GA")
state <- "AZ"

# MLS Cleaning Inputs -----------------------------------------------
id_vars <- c("CMAS_FIPS_CODE", "CMAS_PARCEL_ID", "CMAS_PARCEL_SEQ_NBR")
date_vars <- c("FA_ContractDate", "LeaseDate", "FA_CloseDate", "CloseDate", 
               "PurchaseContractDate", "ContingentDate", "FA_ListDate",
               "ListDate", "FA_OriginalListDate",  "OriginalListDate",
               "FA_PostDate", "FA_EntryDate")
p_vars <- c("FA_Rent_Sale_ind", "ListPrice", "ClosePrice", "RentPrice")

all_states <- list()
for (state in states){
  all_states[[state]] <- fread(paste0(scratch_path, "mls/quickhistory_state/mls_quickhistory_", state, ".csv"), 
               colClasses = list(character = c("CMAS_Zip5", id_vars)))
}
mls <- rbindlist(all_states)
rm(all_states)
print("After reading MLS:")
getAvailMem()

# date cleaning
ranked_fill(mls, "mls_date", list(date_vars))
mls[, mls_date := ymd_hms(mls_date)]
mls[, mls_year := year(mls_date)]

# zip cleaning
mls[CMAS_Zip5 == "", CMAS_Zip5 := NA]

# price cleaning
mls_p <- mls[!is.na(CMAS_FIPS_CODE) & CMAS_PARCEL_ID != "", .SD, .SDcols = c(id_vars, "mls_year", "mls_date", p_vars, "CMAS_Zip5")]
mls_p[FA_Rent_Sale_ind == "", FA_Rent_Sale_ind := "NA"]

# Enforce 1 zip code per property
mls_p[, temp_id := .GRP, by = c(id_vars, "FA_Rent_Sale_ind")]
mls_p[,CMAS_Zip5 := argmax_na(CMAS_Zip5), temp_id]

# Split price columns by indices 
mls_p <- dcast(mls_p, CMAS_FIPS_CODE+CMAS_PARCEL_ID+CMAS_PARCEL_SEQ_NBR+mls_year+mls_date+CMAS_Zip5~FA_Rent_Sale_ind,
                 value.var = c("ListPrice", "ClosePrice", "RentPrice"), fun.aggregate = mean_na0)

# Clean price columns ---------------------------------------------------------------
mls_p[,`:=`(SalePrice = ClosePrice_S, RentPrice = RentPrice_R)]
mls_p[is.na(SalePrice), SalePrice := ListPrice_S]
for (c in c("RentPrice_S", "ClosePrice_R", "ListPrice_R")){
  mls_p[is.na(RentPrice), RentPrice := get(c)]
}

# Inflation adjust prices
p_vars_all <- grep("price|Price", names(mls_p), value=T)
price_adjust(mls_p, vars = p_vars_all, new_vars = p_vars_all, yr_var = "mls_year")

# Dedup properties to year
mls_p[, id := .GRP, by = id_vars]
mls_p <- dedup(mls_p, id, mls_year, c("SalePrice", "RentPrice"),
               c(mean_na0, mean_na0), hpc = "foreach", cores = 5)
# Panel coverage diagnostics -----------------------------------------------------------------
panel_vars <- c("mls_year", id_vars)
p_vars <- c("RentPrice", "SalePrice")

# Properties over time 
for (p in p_vars){
  tmp <- mls_p[!is.na(get(p)),.(N = uniqueN(.SD)), .SDcols = id_vars, mls_year]
  ggplot(tmp, aes(x = mls_year, y = N)) + geom_line() + labs(x = "Year", y = "# Unique Properties",
                                                             title = paste("# Properties with", p)) +
    ggsave(paste0(plot_path, p, "_n_year.png"))
}

# Connected properties over time
for (p in p_vars){
  tmp <- mls_p[!is.na(get(p)),.(N = uniqueN(mls_year)), by = id_vars][order(-N)]
  print(head(tmp))
  print(paste(p, "average number of years covered by each property", mean(tmp$N)))
  print(paste(p, "median number of years covered by each property", median(tmp$N)))
}

for (p in p_vars){
  id_yr_colp <- mls_p[!is.na(get(p)),.(id = list(unique(id))), mls_year][order(mls_year)]
  persist_n <- list()
  persist_n[[1]] <- 0
  for(i in 2:length(id_yr_colp$id)){
    persist_n[[i]] <- length(intersect(id_yr_colp$id[[i]], id_yr_colp$id[[i-1]]))
  }
  tmp <- data.table(n_persist = unlist(persist_n), mls_year = id_yr_colp$mls_year)
  ggplot(tmp, aes(x = mls_year, y = n_persist)) + geom_line() + 
    labs(x = "Year", y = "# Persistent Properties", title = paste("# Persistent Properties with ", p)) +
    ggsave(paste0(plot_path, p, "_persist_year.png"))
}

# Prices over Time 
for (p in p_vars){
  tmp <- mls_p[mls_year > 1990 & mls_year < 2020,.(price = median_na0(get(p))), mls_year]
  cutoff <- quantile(tmp$price, .95, na.rm=T)
  ggplot(tmp[!is.na(price) & price <= cutoff], aes(x = mls_year, y = price)) + geom_line() + labs(x = "Year", y = p,
                                                             title = paste("Median", p, "over Time")) +
    ggsave(paste0(plot_path, p, "_med_year.png"))
}


# Zip-level diagnostics -------------------------------------------------------------
# Aggregate to zip 
mls_zip <- dedup(mls_p, CMAS_Zip5, mls_year, p_vars,
                 c(median_na0, median_na0), hpc = "foreach", cores = 5)

# Prices over Time 
for (p in p_vars){
  tmp <- mls_p[mls_year > 1990 & mls_year < 2020,.(price = median_na0(get(p))), mls_year]
  cutoff <- quantile(tmp$price, .95, na.rm=T)
  ggplot(tmp, aes(x = mls_year, y = price)) + geom_line() + labs(x = "Year", y = p,
                                                                 title = paste("Median Zip", p, "over Time")) +
    ggsave(paste0(plot_path, p, "_med_zip_year.png"))
}
