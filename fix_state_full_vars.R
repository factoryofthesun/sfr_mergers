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

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/pipeline_tmp/micro/"

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

# Command Line Inputs ---------------------------------------------------
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-s", "--state", type="character", default="WA", 
                    help="State file to build [default %(default)s]",
                    metavar = "abbrev")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
state <- args$state

cores <- 5
if (state == "TX" | state == "FL"){
  cores <- 2
}
  
# Read state MLS file
dt <- fread(paste0(data_path, state, ".csv"))
# dt <- fread(paste0(data_path, state, ".csv"), nrows=1e6)
getAvailMem()

# Remove NA ids from MLS merge
dt <- dt[!is.na(id)]
dt[apn_unformatted == "", apn_unformatted := NA]

# Send all empty strings to NA

# Full fill vars
ids <- c("fips", "apn_unformatted", "apn_num")
full_vars <- c(ids, "sqft", "tot_val", "land_sqft", "story_n_clean", "unit_n_clean")
keep_vars <- setdiff(names(dt), c(full_vars, "id", "year", "year_built_clean"))
fill <- rep("full", length(full_vars))
all_fill_vars <- c(full_vars, "id", "year", "deed_event", "year_built_clean")

# Filling algorithm
t2 <- Sys.time()
final_panel <- unique(dt[,..all_fill_vars])
setorder(final_panel, id, year)
final_panel[,deed_event_cum := cumsum(!is.na(deed_event))] # This works bc data is sorted by id, year
id_change <- final_panel[,c(TRUE, id[-1] != id[-.N])]
id_deed_change <- final_panel[,c(TRUE, (id[-1] != id[-.N]) | (deed_event_cum[-1] != deed_event_cum[-.N]))]

# Reverse for backfill
final_panel[order(-id, -year), deed_event_rev_cum := cumsum(!is.na(deed_event))]
rev_id_deed_change <- final_panel[order(-id, -year),c(TRUE, (id[-1] != id[-.N]) | (deed_event_rev_cum[-1] != deed_event_rev_cum[-.N]))]

# Remove deed_event because it is in keep_vars
final_panel[,deed_event := NULL]
final_panel[,deed_event_rev_cum := NULL]

print("Filling full options...")
print(paste(c("Full fill vars:", full_vars), collapse=" "))

# Front fill each variable to the first non-na 
require(parallel)
require(foreach)
require(doMC)
registerDoMC(cores)

t0 <- Sys.time()
panel_temp <- foreach(k = 1:length(full_vars), .combine = function(x,y) merge(x, y, by = c("id", "year", "year_built_clean")))%dopar%{
  tmp_var <- full_vars[k]
  temp <- final_panel[,.SD, .SDcols = c("id", "year", tmp_var, "year_built_clean")]
  temp[,(tmp_var) := get(tmp_var)[cummax((!is.na(get(tmp_var)) | id_change) * .I)]]
  # rm(final_panel)
  # gc()
  temp
}
print("Frontfill done")
panel_temp_full <- panel_temp

# Back fill each variable to built_year, if applicable
panel_temp <- foreach(k = 1:length(full_vars), .combine = function(x,y) merge(x, y, by = c("id", "year", "year_built_clean")))%dopar%{
  tmp_var <- full_vars[k]
  temp <- panel_temp_full[,.SD, .SDcols = c("id", "year", tmp_var, "year_built_clean")]
  temp[order(id, -year),(tmp_var) := get(tmp_var)[cummax((!is.na(get(tmp_var)) | id_change | year < year_built_clean) * .I)]]
  # rm(panel_temp_full)
  # gc()
  temp
}
print("Backfill done")
final_panel <- panel_temp
t1 <- Sys.time()
print(paste("Full fill time:", difftime(t1, t0, units="mins"), "minutes"))

t0 <- Sys.time()
if(length(keep_vars) != 0){
  keep_dt <- dt[, .SD[1], .SDcols = keep_vars, by=c("id", "year")]
  final_panel <- merge(final_panel, keep_dt, by=c("id", "year"), all.x=T)
}
t1 <- Sys.time() 
print(paste("Merging back time invariant var time:", difftime(t1, t0, units="mins"), "minutes"))

fwrite(final_panel[year >= year_built_clean], paste0(data_path, state, "_fixed.csv"))
