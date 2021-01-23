# ===================================================================
# Mergers Project: Identify banks, gov, builders, and legal 
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

# Read and id names ---------------------------------------------------
dt <- fread(paste0(mergers_data_path, state, "_mergers.csv"), colClasses=list(character = "apn_unformatted"))
identify_banks_gov(dt, "smty_owner_name")
builder_names <- c("builder", " loan "," loan$", "construction")
law_names <- c("legal", " law ", " law$", "^law ", "attorney")
dt[, `:=`(owner_builder = 0, owner_law = 0)]
dt[grepl(paste0(builder_names, collapse = "|"), smty_owner_name), owner_builder := 1]
dt[grepl(paste0(law_names, collapse = "|"), smty_owner_name), owner_law := 1]

# Write back 
fwrite(dt, paste0(mergers_data_path, state, "_mergers.csv"))
