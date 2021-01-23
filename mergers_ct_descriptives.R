# ===================================================================
# Mergers Analysis: Descriptives
# ===================================================================
# Figures and descriptives for mergers
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
library(stringi)
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")
Sys.umask(000)

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/ct/prepost/"

# Read and concat merger identified panel data
files <- paste0(data_path, grep("_mergers\\.csv$", list.files(data_path), value = T))
size <- sum_na(file.info(files)$size)/1e9
print(paste("Total size of merger state files:", size, "GB"))

t0 <- Sys.time()
micro_list <- lapply(files, function(x) fread(x, integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted"))))
# micro_list <- lapply(files, function(x) fread(x, integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")),nrows=1e5))
dt <- rbindlist(micro_list, fill = T)
rm(micro_list)
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# ============= (0) Preprocessing ===================================================
# Impute tracts (not sure if necessary)
dt[, st_tract := argmax_na(st_tract), id]
dt[, st_tract := as.integer(st_tract)]

# Drop missing st_tract and out of bounds years
dt <- dt[!is.na(st_tract) & year <= 2020 & year >= 2000]

# Redefine id
dt[, id := .GRP, .(fips, apn_unformatted, apn_num)]

# Restrict to only SFR properties
sfr_ids <- unique(dt[prop_type_clean %in% c(10, 11), id])
dt <- dt[id %in% sfr_ids]

# Drop low property count tracts 
bad_tracts <- dt[,.(N = uniqueN(id)), .(st_tract)][N < 50, st_tract]
print(paste0("Dropping ", nrow(dt[st_tract %in% bad_tracts]), " rows of tracts with under 50 residential properties out of ", 
             nrow(dt), " total rows."))
dt <- dt[!(st_tract %in% bad_tracts)]

# Frontfill owner names
setorder(dt, id, year)
dt[,deed_event_cum := cumsum(!is.na(deed_event))] # This works bc data is sorted by id, year
id_deed_change <- dt[,c(TRUE, (id[-1] != id[-.N]) | (deed_event_cum[-1] != deed_event_cum[-.N]))]
final_panel[, owner_name_clean := owner_name_clean[cummax(((!is.na(owner_name_clean) & owner_name_clean != "") | id_deed_change) * .I)]] 

# Clean variables -------------------------------------------
# Data types
dt[,year := as.integer(year)]
dt[,st_tract := as.integer(st_tract)]

# Check and clean Beazer properties
beazer_search <- c("rental", "pre owned", "preowned")
print(paste0("Cleaning Beazer property identifiation. Pre-counts: ", uniqueN(dt[Merger_Owner_Impute == "Beazer Pre-Owned Rental Homes", id])))
dt[Merger_Owner_Impute == "Beazer Pre-Owned Rental Homes" & 
     grepl(paste0(beazer_search, collapse = "|"), owner_name_clean, ignore.case = T), good_beazer := 1]
dt[Merger_Owner_Name == "Beazer Pre-Owned Rental Homes", Merger_Owner_Name := NA]
dt[good_beazer == 1, Merger_Owner_Name := "Beazer Pre-Owned Rental Homes"]
print(paste0("Post-counts: ", uniqueN(dt[good_beazer == 1, id])))

# Impute owner names using smty addresses 
dt[is.na(Merger_Owner_Name), Merger_Owner_Name := ""]
for (name in unique(dt[Merger_Owner_Name != "", Merger_Owner_Name])){
  print(paste0("Pre-Impute Top owners for: ", name))
  print(dt[Merger_Owner_Name == name,.N, owner_name_clean][order(-N)][1:10])
}

# Impute on single owner addresses only 
dt[Merger_Owner_Name != "" & owner_smty_addr != "", n_merge_owned := uniqueN(Merger_Owner_Name), owner_smty_addr]
single_owner_addr <- unique(dt[n_merge_owned == 1, owner_smty_addr])
print(paste0("% merger properties associated with multi-address: ", 
             nrow(dt[Merger_Owner_Name != "" & !(owner_smty_addr %in% single_owner_addr)])/nrow(dt[Merger_Owner_Name != ""])))

# Don't allow address imputation on bank/gov names 
gov_addr <- unique(dt[(owner_name_clean_bank == 1 | owner_name_clean_gov == 1 | owner_builder == 1 | owner_law == 1),
                      owner_smty_addr])
good_addr <- setdiff(single_owner_addr, gov_addr)
print(paste0("# bank/gov addr: ", length(gov_addr)))
print(paste0("# unique good addr: ", length(good_addr)))
print(paste0("# rows with law/builder name: ", nrow(dt[owner_builder == 1 | owner_law == 1])))

# Remove bad owners from sample
n_gov_rows <- dt[!(owner_name_clean_bank == 1 | owner_name_clean_gov == 1 | owner_builder == 1 | owner_law == 1)]
print(paste0("Dropping ", n_gov_rows, " bank/gov rows out of ", nrow(dt), " from sample."))
dt <- dt[!(owner_name_clean_bank == 1 | owner_name_clean_gov == 1 | owner_builder == 1 | owner_law == 1)]

dt[,Merger_Owner_Impute := Merger_Owner_Name]
dt[owner_smty_addr != "" & (owner_smty_addr %in% single_owner_addr), 
   Merger_Owner_Impute := Merger_Owner_Name[Merger_Owner_Name != ""][1], owner_smty_addr]

print("Post-Impute Properties with Rent")
print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Impute])

# Merger owner name shouldn't change after first appearance
setorder(dt, id, year)
id_change <- dt[,c(TRUE, id[-1] != id[-.N])]
dt[, Merger_Owner_Fill := Merger_Owner_Impute]
dt[,merge_own_cum := cumsum(Merger_Owner_Impute != ""), id]
dt[merge_own_cum > 0, 
   Merger_Owner_Fill := Merger_Owner_Fill[1], id] 

dt[is.na(Merger_Owner_Impute), Merger_Owner_Impute := ""]
for (name in unique(dt[Merger_Owner_Impute != "", Merger_Owner_Impute])){
  print(paste0("Post-Impute Top owners for: ", name))
  print(dt[Merger_Owner_Impute == name,.N, owner_name_clean][order(-N)][1:10])
}

print("Post-Fill Properties with Rent")
print(dt[!is.na(RentPrice) & RentPrice != "",.(N = uniqueN(id)), Merger_Owner_Fill])

# Clean prices
p_vars_all <- str_subset(names(dt), "price|Price")
dt[,(p_vars_all) := lapply(.SD, as.numeric), .SDcols = p_vars_all]
for (p in p_vars_all){
  dt[get(p) == 0, (p) := NA]
}

# Define price vars 
dt[,log_rent := log(RentPrice)]
dt[is.infinite(log_rent), log_rent := NA]
dt[,rent_sqft := RentPrice/sqft]
dt[is.infinite(rent_sqft), rent_sqft := NA]
dt[,log_sqft := log(sqft)]
dt[is.infinite(log_sqft), log_sqft := NA]

# ============= (0.5) Diagnostics ===================================================
# Read in cleaned mergers file
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))

# Check property counts for each company
# SFRs should be residential with prop type 10 
companies <- unique(c(mergers$TargetName, mergers$AcquirorName))
for (company in companies){
  n_tot <- uniqueN(dt[Merger_Owner_Fill == company,  id])
  n_tot_rent <- uniqueN(dt[Merger_Owner_Fill == company & !is.na(RentPrice) & RentPrice != 0,  id])
  n_good_counts <- dt[Merger_Owner_Fill == company & year >= 2000 & !is.na(RentPrice) & RentPrice != 0, .N, year][order(year)]
  print(paste("Total properties with rent found for", company, ":", n_tot))
  print(paste("Total properties with rent found for", company, ":", n_tot_rent))
  print(paste("Annual counts with rent for", company))
  print(n_good_counts)
}
getAvailMem()

# ============= (1) Figures ===================================================
# Pooled merger trends --------------------------------------------------------------
# Consolidate into one chart -- grouping by firms involved in each respective merger 
# Concat separate panels for each merger to deal with ids involved in multiple merge events
merge_labels <- c("Beazer/Ellington - American Homes 4 Rent", "Beazer/Ellington - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
colors <- c("coral1","coral1", "dodgerblue", "darkorange", "forestgreen", "plum")
mergers$label <- merge_labels
mergers$colors <- colors

mergers_list <- list()
for (merge_id in unique(mergers$MergeID_1)){
  ids <- unique(dt[TargetID == merge_id|AcquirorID == merge_id, id])
  tmp <- dt[id %in% ids]
  merge_name <- unique(mergers[MergeID_1 == merge_id, label])
  mergers_list[[merge_id]] <- tmp[,.(Median_Rent = median_na0(RentPrice), Mean_Rent = mean_na0(RentPrice), merger = merge_name), year]    
}
dup_dt <- rbindlist(mergers_list)
rm(mergers_list)
getAvailMem()

ggplot(data = dup_dt[year >= 2010], aes(x = year, y = Median_Rent, color = merger)) + geom_line() + 
  scale_color_manual(values = unique(colors), breaks = unique(merge_labels), labels = unique(merge_labels)) + 
  geom_vline(xintercept = mergers$year_effective, color=mergers$colors) +
  geom_vline(xintercept = mergers$year_announced, color=mergers$colors, linetype="dashed") +
  scale_x_continuous(breaks = unique(dup_dt$year)) + 
  ggsave(paste0(mergers_path, "ct_mergers_median_trends.png"), width = 15, height = 6)

ggplot(data = dup_dt[year >= 2010], aes(x = year, y = Mean_Rent, color = merger)) + geom_line() + 
  scale_color_manual(values = unique(colors), breaks = unique(merge_labels), labels = unique(merge_labels)) + 
  geom_vline(xintercept = mergers$year_effective, color=mergers$colors) +
  geom_vline(xintercept = mergers$year_announced, color=mergers$colors, linetype="dashed") +
  scale_x_continuous(breaks = unique(dup_dt$year)) + 
  ggsave(paste0(mergers_path, "ct_mergers_mean_trends.png"), width = 15, height = 6)

# Property level event-time charts --------------------------------------------------------------
# For each merger: multiple scatterplots for each possible control group + plot multiple features
# 1) within-zip non-merging properties
# 2) outside-zip same-firm properties but in unmerged tracts
# 3) outside-zip unmerged tracts with presence of one firm 
vars <- c("RentPrice", "log_rent", "rent_sqft", "tot_unit_val", "SalePrice", "sqft")
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  # if (merge_id == 3){
  #   next 
  # }
  dt[, group := as.character(NA)]
  
  # Merger vars 
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
  acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
  norm_year <- unique(mergers[MergeID_1 == merge_id, year_effective])
  year_announced <- unique(mergers[MergeID_1 == merge_id, year_announced])
  norm_year_announced <- unique(mergers[MergeID_1 == merge_id, year_announced]) - norm_year
  
  # Set treated variables
  target_tracts <- unique(dt[year==norm_year & Merger_Owner_Fill %in% target_name, st_tract])
  acquiror_tracts <- unique(dt[year==norm_year & Merger_Owner_Fill %in% acquiror_name, st_tract])
  treated_tracts <- intersect(target_tracts, acquiror_tracts)
  dt[st_tract %in% treated_tracts & (Merger_Owner_Fill %in% target_name | Merger_Owner_Fill %in% acquiror_name), group := "Treated"]
  dt[,event_yr := as.integer(year - norm_year)]
  
  # Define different control groups 
  dt[is.na(group) & st_tract %in% treated_tracts, group := "Within-Tract Non-Merge"]
  dt[!(st_tract %in% treated_tracts) & (Merger_Owner_Fill %in% target_name | Merger_Owner_Fill %in% acquiror_name), group := "Outside-Tract Merge"]
  single_firm_tracts <- setdiff(c(target_tracts, acquiror_tracts), treated_tracts)
  dt[st_tract %in% single_firm_tracts & is.na(group), group := "Outside-Tract Non-Merge"]
  
  # Set group factor levels 
  dt[, group := factor(group, levels = c("Treated", "Within-Tract Non-Merge", "Outside-Tract Merge", "Outside-Tract Non-Merge"), ordered = T)]
  
  # Save variables
  post_var <- paste0("post_", merge_id)
  dt[, (post_var) := 0]
  dt[event_yr > 0, (post_var) := 1]
  
  treated_var <- paste0("treated_", merge_id)
  dt[, (treated_var) := 0]
  dt[st_tract %in% treated_tracts & (Merger_Owner_Fill %in% target_name | Merger_Owner_Fill %in% acquiror_name), (treated_var) := 1]
  
  sample_1_var <- paste0("sample_", merge_id, "_c1")
  dt[, (sample_1_var) := get(treated_var)]
  dt[st_tract %in% treated_tracts, (sample_1_var) := 1]
  
  sample_2_var <- paste0("sample_", merge_id, "_c2")
  dt[, (sample_2_var) := get(sample_1_var)]
  dt[(Merger_Owner_Fill %in% target_name | Merger_Owner_Fill %in% acquiror_name), (sample_2_var) := 1]
  
  sample_3_var <- paste0("sample_", merge_id, "_c3")
  dt[, (sample_3_var) := get(sample_1_var)]
  dt[st_tract %in% single_firm_tracts, (sample_3_var) := 1]

  # Target firms become assigned to acquiror firms in post-merger
  dt[Merger_Owner_Fill %in% acquiror_name | (Merger_Owner_Fill %in% target_name & get(post_var) == 1), owner_hhi := acquiror_name]
}

# Fill rest of owner hhi variable using owner address
dt[is.na(owner_hhi) | owner_hhi == "", owner_hhi := Merger_Owner_Fill]
dt[(is.na(owner_hhi) | owner_hhi == "") & owner_smty_addr != "" & !is.na(owner_smty_addr) & !(owner_smty_addr %in% gov_addr), 
   owner_hhi := as.character(.GRP), .(owner_smty_addr)]

# Fill rest with owner name
dt[(is.na(owner_hhi) | owner_hhi == "") & owner_name_clean != "" & !is.na(owner_name_clean), 
   owner_hhi := owner_name_clean]

# Remove non-rental owners
dt[owner_hhi == "", owner_hhi := NA]

# Tract level event-time charts --------------------------------------------------------------
# Merge with dt 
dt_tract <- dt[,.(median_rent = median_na0(RentPrice), mean_rent = mean_na0(RentPrice),
                median_log_rent = median_na0(log_rent), median_rent_sqft = median_na0(rent_sqft),
                median_tot_unit_val = median_na0(tot_unit_val), mean_tot_unit_val = mean_na0(tot_unit_val), 
                median_sale = median_na0(SalePrice), mean_sale = mean_na0(SalePrice), 
                median_sqft = median_na0(sqft), mean_sqft = mean_na0(sqft), 
                median_log_sqft = median_na0(log_sqft), N = .N),.(st_tract, year)]

# Compute HHI (assume filled owner strings are representative sample)
dt_owner <- dt[!is.na(owner_hhi) & owner_hhi != "",.N, .(owner_hhi, st_tract, year)]
dt_owner[,share := N/sum_na(N) * 100, .(st_tract, year)]
dt_owner[is.infinite(share), share := NA]
dt_hhi <- dt_owner[,.(hhi = sum_na(share^2)), .(st_tract, year)]
dt_tract <- merge(dt_tract, dt_hhi, by = c("st_tract", "year"), all.x = T)
n_bad_hhi <- nrow(dt_tract[is.na(hhi)])
print(paste0(n_bad_hhi, " zip-years with no HHI computed."))

# Plot event-time charts
vars <- c("tot_unit_val", "sale", "hhi", "sqft")
hhi_wide_list <- list() # Save hhi tables
hhi_long_list <- list()
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  # if (merge_id == 3){
  #   next 
  # }
  dt_tract[,group := as.character(NA)]
  
  # Merger vars 
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
  acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
  norm_year <- unique(mergers[MergeID_1 == merge_id, year_effective])
  year_announced <- unique(mergers[MergeID_1 == merge_id, year_announced]) 
  norm_year_announced <- year_announced - norm_year
  norm_month <- max(unique(mergers[MergeID_1 == merge_id, month_effective]))
  norm_month_announced <- max(unique(mergers[MergeID_1 == merge_id, month_announced]))
  
  # Set treated variables
  target_tracts <- unique(dt[year==norm_year & Merger_Owner_Fill %in% target_name, st_tract])
  acquiror_tracts <- unique(dt[year==norm_year & Merger_Owner_Fill %in% acquiror_name, st_tract])
  treated_tracts <- intersect(target_tracts, acquiror_tracts)
  dt_tract[st_tract %in% treated_tracts, group := "Treated"]
  dt_tract[,event_yr := as.integer(year - norm_year)]
  
  # Define control group: single-firm tracts
  single_firm_tracts <- unique(dt[!(st_tract %in% treated_tracts) & (Merger_Owner_Fill %in% target_name | Merger_Owner_Fill %in% acquiror_name), st_tract])
  dt_tract[st_tract %in% single_firm_tracts, group := "Control"]
  
  for (var in vars){
    if (var == "hhi") {
      # Annual counts 
      n_dt_tract <- dt_tract[!is.na(group) & event_yr >= -5 & !is.na(hhi) & event_yr <= 5, .N, .(group, event_yr)]
      n_dt_tract[,log_N := log(N)]
      ggplot(n_dt_tract, aes(x = event_yr, y = log_N, fill = group, group = group)) + geom_bar(stat="identity", position="dodge") +
        labs(y = "log N", title = paste0("Tract HHI Counts, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "n_hhi_", merge_id, ".png"), dpi = "screen")
      
      # Median HHI per year 
      dt_med_tmp <- dt_tract[!is.na(group) & event_yr >= -5 & event_yr <= 5,.(median_hhi = median_na0(hhi)), .(year, group)]
      ggplot(data = dt_med_tmp, aes(x = year, y = median_hhi, color = group)) + geom_point(shape = 16, size = 3, alpha = 0.5) +
        geom_line(linetype = "dashed") + geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") +
        scale_x_continuous(breaks = unique(dt_med_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Median Tract HHI by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_med_hhi_",merge_id, ".png"), width = 15, height = 6)
      
      # Mean HHI per year 
      dt_mean_tmp <- dt_tract[!is.na(group) & event_yr >= -5 & event_yr <= 5,.(mean_hhi = mean_na0(hhi)), .(year, group)]
      ggplot(data = dt_mean_tmp, aes(x = year, y = mean_hhi, color = group)) + geom_point(shape = 16, size = 3, alpha = 0.5) +
        geom_line(linetype = "dashed") + geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") +
        scale_x_continuous(breaks = unique(dt_mean_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Mean Tract HHI by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_mean_hhi_",merge_id, ".png"), width = 15, height = 6)
      
      # Save delta HHI tables
      dt[, merge_group := NA]
      dt[st_tract %in% treated_tracts & Merger_Owner_Fill %in% target_name, merge_group := "Target"]
      dt[st_tract %in% treated_tracts & Merger_Owner_Fill %in% acquiror_name, merge_group := "Acquiror"]
      dt[,event_yr := as.integer(year - norm_year)]
      dt_delta_hhi <- dt[event_yr == -1 & !is.na(owner_hhi) & owner_hhi != "", .(delta_hhi = 2 * sum_na(merge_group == "Target") * sum_na(merge_group == "Acquiror")/(.N^2)*100^2,
                                                                                 acquiror_share = sum_na(merge_group == "Acquiror")/.N*100, 
                                                                                 target_share = sum_na(merge_group == "Target")/.N*100), .(st_tract)]
      # dt_delta_hhi[is.na(delta_hhi), delta_hhi := 0]
      # dt_delta_hhi[is.na(acquiror_share), acquiror_share := 0]
      # dt_delta_hhi[is.na(target_share), target_share := 0]
      dt_tract_tmp <- merge(dt_tract, dt_delta_hhi, by = "st_tract", all.x = T)
      dt_tract_tmp[,merge_label := merge_label]
      dt_tract_tmp[,merge_id := merge_id]
      dt_hhi_tmp <- dcast(dt_tract_tmp[event_yr >= -5 & group == "Treated"], 
                          st_tract + delta_hhi + acquiror_share + target_share + merge_label + merge_id ~ event_yr, value.var = "hhi")
      setcolorder(dt_hhi_tmp, "merge_label")
      
      hhi_wide_list[[merge_id]] <- dt_hhi_tmp
      hhi_long_list[[merge_id]] <- dt_tract_tmp[event_yr >= -5 & group == "Treated", .(merge_id, merge_label, st_tract, event_yr, delta_hhi, hhi, 
                                                                                     acquiror_share, target_share)]
      
    } else{
      dt_tmp <- melt(dt_tract, id.vars = c("st_tract", "event_yr","year", "group"), measure.vars = paste0(c("median_", "mean_"), var), 
                     variable.name = "agg_var", value.name = var)
      
      # Annual counts 
      n_dt_tract <- dt_tmp[!is.na(group) & !is.na(get(var)) & event_yr >= -5 & event_yr <= 5, .N, by = c("group", "agg_var", var, "event_yr")]
      n_dt_tract[,log_N := log(N)]
      ggplot(n_dt_tract, aes(x = agg_var, y = log_N, fill = group, group = group)) + geom_bar(stat="identity", position="dodge") +
        labs(y = "log N", title = paste0("Tract ", var, " Counts, Merger:",merge_label)) + facet_wrap(~ event_yr) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        ggsave(paste0(prepost_figs, "n_", var, "_", merge_id, ".png"), dpi = "screen", width = 6, height = 6)
      
      # Median across tracts
      dt_med_tmp <- dt_tmp[!is.na(group) & !is.na(get(var)) & event_yr >= -5 & event_yr <= 5,.(median_var = median_na(get(var))), .(agg_var, year, group)]
      ggplot(data = dt_med_tmp, aes(x = year, y = median_var, color = group, linetype = group, shape = agg_var)) + geom_point(size = 3, alpha = 0.5) +
        geom_line() + scale_linetype_manual(values=c("dashed", "solid")) + 
        geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") +
        scale_x_continuous(breaks = unique(dt_med_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Median Tract", var, "by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_med_", var, "_",merge_id, ".png"), width = 15, height = 6)
    }
  }
}
hhi_wide <- rbindlist(hhi_wide_list, fill = T)
hhi_wide[is.na(delta_hhi), delta_hhi := 0]
hhi_wide[is.na(acquiror_share), acquiror_share := 0]
hhi_wide[is.na(target_share), target_share := 0]
setorder(hhi_wide, merge_label, -delta_hhi)
fwrite(hhi_wide, paste0(mergers_path, "tract/", "hhi_wide.csv"))

hhi_long <- rbindlist(hhi_long_list)
setorder(hhi_long, merge_label, event_yr, -delta_hhi)
fwrite(hhi_long, paste0(mergers_path, "tract/", "hhi_long.csv"))

# Record multi_merge tracts
treated_names <- grep("^treated_", names(dt), value = T)
dt[,treated_overlap := rowSums(.SD, na.rm=T), .SDcols = treated_names]
multi_tracts <- unique(dt[treated_overlap > 1, st_tract])
dt[,multi_merge := 0]
dt[st_tract %in% multi_tracts, multi_merge := 1]

# Delta HHI long to wide for each merger
hhi_merge_wide <- dcast(hhi_wide, st_tract ~ merge_id, value.var = c("delta_hhi", "merge_label", "acquiror_share", "target_share"))

# Save sample panel with HHI
sample_names <- grep("^sample_", names(dt), value = T)
dt[,sample_overlap := rowSums(.SD, na.rm=T), .SDcols = sample_names]
dt <- dt[sample_overlap >= 1]
dt <- merge(dt, dt_hhi, by = c("st_tract", "year"), all.x = T)
dt <- merge(dt, hhi_merge_wide, by = c("st_tract"), all.x = T)

# NA delta HHI is 0 by definition
hhi_names <- grep("^delta_hhi_", names(hhi_merge_wide), value = T)
for (hhi_col in hhi_names){
  dt[is.na(get(hhi_col)), (hhi_col) := 0]
}
fwrite(dt, paste0(data_path, "tract/", "panel_hhi.csv"))

# Save rent-restricted panel 
fwrite(dt[!is.na(RentPrice) & RentPrice != 0], paste0(data_path, "tract/", "panel_hhi_rent.csv"))
