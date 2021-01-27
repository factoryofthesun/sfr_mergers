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
figs_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/map/"

# Read mergers data and restrict to merge-owned properties
mergers <- fread(paste0(mergers_path, "panel_hhi.csv"),
                 select=c("fips", "apn_unformatted", "Merger_Owner_Fill", "RentPrice", "st_lat", "st_long","st_state"))
mergers <- mergers[!is.na(Merger_Owner_Fill) & Merger_Owner_Fill != ""]

# Read and merge geocoded addresses
states <- c("AZ", "CA", "FL", "IL", "GA", "MI", "NC", "TX", "WA", "CT")
states_all <- list()
for (state in states){
  tmp <- fread(paste0(geocodes_path, "cl_addr_", state, ".csv"), colClasses = list("character" = c("USER_apn_unformatted")),
               select=c("USER_fips", "USER_apn_unformatted", "X", "Y", "DisplayX", "DisplayY"), nrows=1e5)
  states_all[[state]] <- tmp
}
states_tot <- rbindlist(states_all)
rm(states_all)
getAvailMem()

mergers <- merge(mergers, states_tot, by.x = c("fips", "apn_unformatted"), by.y=c("USER_fips", "USER_apn_unformatted"), all.x = T)
n_bad <- nrow(mergers[is.na(X)|is.na(Y)])
print(paste0(n_bad, " out of ", nrow(mergers), " rows without geocoded address. Imputing with DisplayX/DisplayY and original st-values."))
mergers[is.na(X), X := DisplayX]
mergers[is.na(Y), Y := DisplayY]
mergers[is.na(X), X := st_long] # Longitude is X
mergers[is.na(Y), Y := st_lat]
n_bad <- nrow(mergers[is.na(X)|is.na(Y)])
print(paste0(n_bad, " bad rows after imputing."))

# Plot on map
theme_set(theme_bw())
library(maps)
library(ggmap)
library(ggthemes)

merge_labels <- c("Invitation Homes Inc", "American Homes 4 Rent", "Starwood Waypoint Residential",
                  "Colony American Homes Inc", "Silver Bay Realty Trust Corp", "Beazer Pre-Owned Rental Homes")
colors <- c("coral1", "forestgreen", "dodgerblue", "darkorange", "violet", "gold3")

states <- map_data("state")
ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group=group), color = "black", fill = "white") + 
  coord_fixed(1.3) + 
  geom_point(data=mergers[!is.na(Merger_Owner_Fill)], aes(x = X, y = Y, color = Merger_Owner_Fill),size = 0.1,shape=16) + 
  scale_color_manual(breaks = merge_labels, values = colors) + guides(color = guide_legend(override.aes = list(size=6))) + 
  labs(title = "SFR Owners in the US", color="Firm")+ theme_map() + 
  theme(legend.position = "bottom") +
  ggsave(paste0(figs_path, "US.png"), width = 10)

# Plot separate on just properties with rent price
ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group=group), color = "black", fill = "white") + 
  coord_fixed(1.3) + 
  geom_point(data=mergers[!is.na(Merger_Owner_Fill) & !is.na(RentPrice) & RentPrice != ""], aes(x = X, y = Y, color = Merger_Owner_Fill),size = 0.1,shape=16) + 
  scale_color_manual(breaks = merge_labels, values = colors) + theme_map() +
  theme(legend.position = "bottom") + guides(color = guide_legend(override.aes = list(size=6))) + 
  labs(title = "SFR Owners in the US", color="Firm") + 
  ggsave(paste0(figs_path, "US_rent.png"), width = 10)

# Plot each state individually 
for (state in unique(mergers$st_state)){
  state_name <- tolower(state.name[grep(state, state.abb)])
  tmp_state <- states[states$region == state_name,]
  # Drop bad coordinates
  min_long <- min(tmp_state$long)
  max_long <- max(tmp_state$long)
  min_lat <- min(tmp_state$lat)
  max_lat <- max(tmp_state$lat)
  tmp <- mergers[st_state == state & X >= min_long & X <= max_long & Y >= min_lat & Y <= max_lat]
  
  ggplot() + geom_polygon(data = tmp_state, aes(x = long, y = lat, group=group), color = "black", fill = "white") + 
    coord_fixed(1.3) + 
    geom_point(data=tmp[!is.na(Merger_Owner_Fill)], aes(x = X, y = Y, color = Merger_Owner_Fill),size = 0.1,shape=16) + 
    scale_color_manual(breaks = merge_labels, values = colors, guide=guide_legend(override.aes=list(size=3))) + 
    labs(title = paste0("SFR Owners in ", state), color="Firm") + theme_map() + 
    theme(legend.position = "bottom") +
    ggsave(paste0(figs_path, state, ".png"), width = 5, height = 5)
  
  # Plot separate on just properties with rent price
  ggplot() + geom_polygon(data = tmp_state, aes(x = long, y = lat, group=group), color = "black", fill = "white") + 
    coord_fixed(1.3) + 
    geom_point(data=tmp[!is.na(Merger_Owner_Fill) & !is.na(RentPrice) & RentPrice != ""], 
               aes(x = X, y = Y, color = Merger_Owner_Fill),size = 0.1,shape=16) + 
    scale_color_manual(breaks = merge_labels, values = colors, guide=guide_legend(override.aes=list(size=3))) + 
    labs(title = paste0("SFR Owners in ", state), color="Firm") + theme_map() + 
    theme(legend.position = "bottom") +
    ggsave(paste0(figs_path, state, "_rent.png"), width = 5, height = 5)
}



