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
library(reticulate)
library(argparse)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")
source_python("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/cluster_efficiently.py")

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
panel <- fread(paste0(micro_path, "/", state, "_mls.csv"), select = c("owner_name_clean", "owner_corp_clean", "abs_owner_clean", "resid_clean"))
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

# For testing
#panel <- fread(paste0("~/scratch60/pipeline_tmp/micro/IL.csv"), nrows = 1e5)

# Owner name matching ------------------------------------------------

# Create owner name matching table
unique_names <- unique(panel[abs_owner_clean == 1 & resid_clean == 1 & owner_corp_clean == 1 & owner_name_clean != "" & !is.na(owner_name_clean), owner_name_clean])
n_bad <- nrow(panel[(owner_name_clean == "" | is.na(owner_name_clean))])
print(paste("Found", length(unique_names), "unique names and", n_bad, "missing name rows"))
cleaned_dt <- data.table(owner_name_clean = unique_names)

rm(panel)

cleaned_dt_size <- nrow(cleaned_dt)
print("Cleaned names size:")
print(cleaned_dt_size)

print("Available memory after subsetting:")
getAvailMem() 


# Match names 
jw_thresh<-.82
hash_num<-10L
hash_thresh<-.8


t0 <- Sys.time()
matched_names <- match_unique(unique_names,jw_thresh,hash_num,hash_thresh)
t1 <- Sys.time()
print(paste("Owner name match table construction with", nrow(cleaned_dt),
            "rows took", difftime(t1,t0,units='mins'), "minutes."))
matched_df <- data.table(owner_name_clean = unique_names, owner_matched = matched_names)

# Write 
fwrite(matched_df, paste0("~/scratch60/mergers/match_tables/", state, "_matched_owners.csv"))