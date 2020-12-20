# ==== Mergers: Merge in Geocoded Addresses and Plot Merge-Found Properties =======
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
Sys.umask(000)

mergers_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
geocodes_path <- "/gpfs/loomis/scratch60/humphries/js3572/geocodes/"
figs_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/"

# Read mergers data and restrict to merge-owned properties
mergers <- fread(paste0(mergers_path, "panel_hhi.csv"), select=c("fips", "apn_unformatted", "Merger_Owner_Name", "st_lat", "st_long", 
                                                                 "st_state"))
mergers <- mergers[is.na(Merger_Owner_Name) & Merger_Owner_Name != ""]

# Read and merge geocoded addresses
states <- c("AZ", "CA", "FL", "IL", "GA", "MI", "NC", "TX", "WA", "CT")
states_all <- list()
for (state in states){
  tmp <- fread(paste0(geocodes_path, "cl_addr_", state, ".csv"), select=c("USER_fips", "USER_apn_unformatted", 
                                                                          "X", "Y", "DisplayX", "DisplayY"))
  states_all[[state]] <- tmp
}
states_tot <- rbindlist(states_all)
rm(states_all)
getAvailMem()

mergers <- merge(mergers, states_tot, by.x = c("fips", "apn_unformatted"), by.y=c("USER_fips", "USER_apn_unformatted"), all.x = T)
n_bad <- nrow(mergers[is.na(X)|is.na(Y)])
print(paste0(n_bad, " rows without geocoded address. Imputing with DisplayX/DisplayY and original st-values."))
mergers[is.na(X), X := DisplayX]
mergers[is.na(Y), Y := DisplayY]
mergers[is.na(X), X := st_long] # Longitude is X
mergers[is.na(Y), Y := st_lat]
n_bad <- nrow(mergers[is.na(X)|is.na(Y)])
print(paste0(n_bad, " rows after imputing."))

# Plot on map 
library(usmap)

plot_usmap() + geom_point(data=mergers, aes(x = X, y = Y, color = Merger_Owner_Name)) + 
  ggsave(paste0(figs_path, "US_tot.png"))

# Plot each state individually 
for (state in states){
  tmp <- mergers[st_state == state]
  plot_usmap(include=c(state)) + geom_point(data=tmp, aes(x = X, y = Y, color = Merger_Owner_Name)) + 
    ggsave(paste0(figs_path, state, ".png"))
}



