# ================================================================================
# Mergers Analysis: Invitation Homes/Starwood Waypoint Post-Merger Marginal Costs
# ===============================================================================
# Zip level regressions 
#

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
library(lfe)
library(stargazer)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"
estimate_path <- paste0(mergers_path, "estimates/")

dt <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character")
quarters <- fread(paste0(mergers_path, "quarterly_financials.csv"))
properties <- fread(paste0(mergers_path, "property_details.csv"))
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))

# Clean stuff up
properties[quarter == "total", quarter := "4"]
quarters[quarter == "tot", quarter := "total"]

# Get last quarter operating costs 
ivh_three <- quarters[firm == "Invitation Homes" & quarter != "total", .(operating_expense = sum_na(operating_expense),
                                                                         rent_rev = sum_na(rent_rev)), .(year, firm)]
ivh_tot <- merge(ivh_three, quarters[firm == "Invitation Homes" & quarter == "total", .(year, firm, operating_expense, rent_rev)],
                 by = c("year", "firm"))
ivh_tot[, operating_expense := operating_expense.y - operating_expense.x]
ivh_tot[, rent_rev := rent_rev.y - rent_rev.x]
ivh_tot[, quarter := "4"]
quarters <- rbindlist(list(quarters, ivh_tot[,.(year, firm, quarter, operating_expense, rent_rev)]), fill = T)

# SWAY 2017 total
quarters[grepl("waypoint",firm, ignore.case = T), firm := "Starwood Waypoint"]
# sway_17 <- quarters[year == 2017 & firm == "Starwood Waypoint", .(operating_expense = sum(operating_expense),
#                                                                   rent_rev = sum(rent_rev), quarter = "total"), .(year, firm)]
# quarters <- rbindlist(list(quarters, sway_17), fill = T)

# Keep only IVH quarters and SWAY years
quarters_clean <- quarters[(firm == "Invitation Homes" & quarter != "total") | (firm == "Starwood Waypoint")]
quarters_clean[quarter == "total", quarter := "4"]

# Starwood divide 2016 q4 by 3 
quarters_clean[firm == "Starwood Waypoint" & quarter == "4", operating_expense := operating_expense/3]
quarters_clean[firm == "Starwood Waypoint" & quarter == "4", rent_rev := rent_rev/3]

starwood_q4 <- quarters_clean[firm == "Starwood Waypoint" & year == 2017, 
                              .(operating_expense = mean_na(operating_expense), rent_rev = mean_na(rent_rev),
                                quarter = "4"), .(year, firm)]
quarters_clean <- rbindlist(list(quarters_clean, starwood_q4), fill = T)
  
# Same thing for properties
property_q <- properties[, .(num_homes = sum(num_homes), tot_rent = sum(avg_rent * num_homes)), .(firm, year, quarter)]
property_q[firm == "Invitation Homes" & year == 2017 & quarter == "4", num_homes := 47917]

# Plot
dt <- merge(quarters_clean, property_q, by = c("firm", "year", "quarter"))
dt[, marginal_cost := operating_expense/num_homes]
dt[, q_year_label := paste0(year, "-", quarter)]
dt[, q_year := as.yearqtr(paste0(year, "-", quarter), "%Y-%q")]
ggplot(data = dt, aes(x = q_year, y = marginal_cost, color = firm)) + geom_line() + geom_point() + 
  geom_vline(xintercept = as.yearqtr("2017-4", "%Y-%q")) + scale_x_yearqtr(breaks = unique(dt$q_year)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(x = "Year-Quarter", y = "Per-Home Operating Expense (Thousands)", color = "Firm",
       title = "Invitation Homes and Starwood Waypoint Per-Home Operating Expenses") + 
  theme(legend.position = "bottom") +
  ggsave(paste0(mergers_path, "figs/ivh_starwood_costs.png"))

dt[, avg_rent := rent_rev/num_homes * 1000/3]
dt[firm == "Invitation Homes" & year == 2017 & quarter == "4", avg_rent := 1704]
ggplot(data = dt, aes(x = q_year, y = avg_rent, color = firm)) + geom_line() + geom_point() + 
  geom_vline(xintercept = as.yearqtr("2017-4", "%Y-%q")) + scale_x_yearqtr(breaks = unique(dt$q_year)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(x = "Year-Quarter", y = "Average Rent", title = "Invitation Homes and Starwood Waypoint Average Rent") + 
  ggsave(paste0(mergers_path, "figs/ivh_starwood_rent.png"))
