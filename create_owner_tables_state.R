# ===================================================================
# Create owner match tables 
# ===================================================================
# Take state input from command line and runs Josh's owner name matching script to get matching tables

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
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/richard/match_panel_tmp.R")

micro_path <- "/gpfs/loomis/scratch60/humphries/rl874/pipeline_out/micro"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project"

parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-s", "--state", type="character", default="IL", 
                    help="State file to build [default %(default)s]",
                    metavar = "abbrev")

args <- parser$parse_args()

# Set inputs
state <- args$state

# Read in micro data 
t0 <- Sys.time()
panel <- fread(paste0(micro_path, "/", state, "_mls.csv"), select = c("owner_name", "owner_corp_clean"))
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

# For testing
#panel <- fread(paste0("~/scratch60/pipeline_tmp/micro/CT.csv"), nrows = 1e5)

# Owner name matching ------------------------------------------------

# Create owner name matching table
unique_names <- unique(panel[owner_name != "" & !is.na(owner_name), owner_name])
n_bad <- nrow(panel[(owner_name == "" | is.na(owner_name))])
print(paste("Found", length(unique_names), "unique absentee names and", n_bad, "missing absentee name rows"))
cleaned_dt <- data.table(owner_name = unique_names)

rm(panel)

cleaned_dt_size <- object.size(cleaned_dt)/1e9
print("Cleaned names size:")
print(cleaned_dt_size)

print("Available memory after subsetting:")
getAvailMem() 

t0 <- Sys.time()
match_table<-build_matched_name_panel(cleaned_dt,"owner_name","match_name",c(),list(jw_threshold=.83,num_hashes=10L,hash_threshold=.9),c(),c(),"ref")
t1 <- Sys.time()
print(paste("Owner name match table construction with", nrow(cleaned_dt),
            "rows took", difftime(t1,t0,units='mins'), "minutes."))

match_names <- unlist(match_table$match_name) # Need to unlist match name column
match_table[,match_name := NULL]
match_table[,match_name := match_names]

# Write 
fwrite(match_table, paste0("~/scratch60/mergers/match_tables/", state, "_matched_owners.csv"))