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
setwd("~/project")

source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")
Sys.umask(000)

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"

# Read and concat merger identified panel data
files <- paste0(data_path, setdiff(grep("_mergers\\.csv$", list.files(data_path), value = T), c("GA_mergers_good.csv")))
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
# Drop missing Zip5 and out of bounds years
dt <- dt[!is.na(Zip5) & year <= 2020]

# Clean variables -------------------------------------------
# Data types
dt[,year := as.integer(year)]
dt[,Zip5 := as.integer(Zip5)]

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

# Redefine ids 
dt[,id := .GRP, .(fips, apn_unformatted, apn_num)]

# Clean Beazer properties
# print(paste0("Cleaning Beazer property identifiation. Pre-counts: ", nrow(dt[Merger_Owner_Name == "Beazer Pre-Owned Rental Homes"])))
# dt[Merger_Owner_Name == "Beazer Pre-Owned Rental Homes" & grepl("rental", owner_name_clean, ignore.case = T), good_beazer := 1]
# dt[Merger_Owner_Name == "Beazer Pre-Owned Rental Homes", Merger_Owner_Name := NA]
# dt[good_beazer == 1, Merger_Owner_Name := "Beazer Pre-Owned Rental Homes"]
# print(paste0("Post-counts: ", nrow(dt[good_beazer == 1])))

# ============= (0.5) Diagnostics ===================================================
# Read in cleaned mergers file
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))

# Check property counts for each company
# SFRs should be residential with prop type 10 
companies <- unique(c(mergers$TargetName, mergers$AcquirorName))
for (company in companies){
  n_tot <- uniqueN(dt[Merger_Owner_Name == company, id])
  n_year_counts <- dt[Merger_Owner_Name == company & year >= 2000, .N, year][order(year)]
  n_good_counts <- dt[Merger_Owner_Name == company & year >= 2000 & !is.na(RentPrice) & RentPrice != 0, .N, year][order(year)]
  print(paste("Total properties found for", company, ":", n_tot))
  print(dt[Merger_Owner_Name == company, .N, .(prop_type_clean)])
  print(paste("Annual counts for", company))
  print(n_year_counts)
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
  ggsave(paste0(mergers_path, "mergers_median_trends.png"), width = 15, height = 6)

ggplot(data = dup_dt[year >= 2010], aes(x = year, y = Mean_Rent, color = merger)) + geom_line() + 
  scale_color_manual(values = unique(colors), breaks = unique(merge_labels), labels = unique(merge_labels)) + 
  geom_vline(xintercept = mergers$year_effective, color=mergers$colors) +
  geom_vline(xintercept = mergers$year_announced, color=mergers$colors, linetype="dashed") +
  scale_x_continuous(breaks = unique(dup_dt$year)) + 
  ggsave(paste0(mergers_path, "mergers_mean_trends.png"), width = 15, height = 6)

# Property level event-time charts --------------------------------------------------------------
# For each merger: multiple scatterplots for each possible control group + plot multiple features
# 1) within-zip non-merging properties
# 2) outside-zip same-firm properties but in unmerged zips
# 3) outside-zip unmerged zips with presence of one firm 
vars <- c("RentPrice", "log_rent", "rent_sqft", "tot_unit_val", "SalePrice", "sqft")
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id == 3){
    next 
  }
  dt[, group := as.character(NA)]
  
  # Merger vars 
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
  acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
  norm_year <- unique(mergers[MergeID_1 == merge_id, year_effective])
  year_announced <- unique(mergers[MergeID_1 == merge_id, year_announced])
  norm_year_announced <- unique(mergers[MergeID_1 == merge_id, year_announced]) - norm_year
    
  # Set treated variables
  target_zips <- unique(dt[year==norm_year & Merger_Owner_Name %in% target_name, Zip5])
  acquiror_zips <- unique(dt[year==norm_year & Merger_Owner_Name == acquiror_name, Zip5])
  treated_zips <- intersect(target_zips, acquiror_zips)
  dt[Zip5 %in% treated_zips & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), group := "Treated"]
  dt[,event_yr := as.integer(year - norm_year)]

  # Define different control groups 
  dt[is.na(group) & Zip5 %in% treated_zips, group := "Within-Zip Non-Merge"]
  dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), group := "Outside-Zip Merge"]
  single_firm_zips <- setdiff(c(target_zips, acquiror_zips), treated_zips)
  dt[Zip5 %in% single_firm_zips & is.na(group), group := "Outside-Zip Non-Merge"]
  
  # Set group factor levels 
  dt[, group := factor(group, levels = c("Treated", "Within-Zip Non-Merge", "Outside-Zip Merge", "Outside-Zip Non-Merge"), ordered = T)]
  
  # Save variables
  post_var <- paste0("post_", merge_id)
  dt[, (post_var) := 0]
  dt[event_yr > 0, (post_var) := 1]
  
  treated_var <- paste0("treated_", merge_id)
  dt[, (treated_var) := 0]
  dt[Zip5 %in% treated_zips & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), (treated_var) := 1]
  
  sample_1_var <- paste0("sample_", merge_id, "_c1")
  dt[, (sample_1_var) := get(treated_var)]
  dt[Zip5 %in% treated_zips, (sample_1_var) := 1]
  
  sample_2_var <- paste0("sample_", merge_id, "_c2")
  dt[, (sample_2_var) := get(sample_1_var)]
  dt[(Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), (sample_2_var) := 1]
  
  sample_3_var <- paste0("sample_", merge_id, "_c3")
  dt[, (sample_3_var) := get(sample_1_var)]
  dt[Zip5 %in% single_firm_zips, (sample_3_var) := 1]
  
  for (var in vars){
    dt[,tmp := get(var)]
    
    # Annual counts 
    n_dt <- dt[!is.na(group) & !is.na(tmp) & event_yr >= -5 & event_yr <= 5, .N, .(group, event_yr)]
    n_dt[,log_N := log(N)]
    ggplot(n_dt, aes(x = event_yr, y = log_N, fill = group, group = group)) + geom_bar(stat="identity", position="dodge") +
      labs(y = "log N", title = paste0("Counts with ", var, " , Merger:",merge_label)) + 
      ggsave(paste0(prepost_figs, "n_", var, "_", merge_id, ".png"), dpi = "screen", width = 6, height = 6)
    
    # Median per year 
    dt_tmp <- dt[!is.na(group) & event_yr >= -5 & event_yr <= 5,.(tmp = as.numeric(median_na0(tmp))), .(group, year)]
    ggplot(data = dt_tmp, aes(x = year, y = tmp, color = group)) + geom_point(shape = 16, size = 3, alpha = 0.5) +
      geom_line(aes(linetype = group)) + scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed")) +
      geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") + 
      scale_x_continuous(breaks = unique(dt_tmp$year)) + 
      labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Merger:",merge_label)) + 
      ggsave(paste0(prepost_figs, "prepost_median_", var,"_",merge_id, ".png"), width = 15, height = 6)
  
    # Mean per year 
    # dt_tmp <- dt[!is.na(group) & event_yr >= -5 & event_yr <= 5,.(tmp = as.numeric(mean_na0(tmp))), .(group, year)]
    # ggplot(data = dt_tmp, aes(x = year, y = tmp, color = group)) + geom_point(shape = 16, size = 3, alpha = 0.5) +
    #   geom_line(linetype = "dashed") + geom_vline(xintercept=0) + geom_vline(xintercept=norm_year_announced, linetype="dashed") +
    #   scale_x_continuous(breaks = unique(dt_tmp$year)) + 
    #   labs(x = "Normalized Year", y = var, title = paste("Mean", var, "by Year,Merger:",merge_label)) + 
    #   ggsave(paste0(prepost_figs, "prepost_mean_", var,"_",merge_id, ".png"), width = 15, height = 6)
  }
  # Target firms become assigned to acquiror firms in post-merger
  dt[Merger_Owner_Name == acquiror_name | (Merger_Owner_Name %in% target_name & get(post_var) == 1), owner_hhi := acquiror_name]
}

# Fill rest of owner hhi variable
dt[is.na(owner_hhi) | owner_hhi == "", owner_hhi := Merger_Owner_Name]
dt[is.na(owner_hhi) | owner_hhi == "", owner_hhi := owner_name_clean]

# Zip level event-time charts --------------------------------------------------------------
# Clean Zillow
zillow_file <- "/project/humphries/jeh232/rent_project/data/zillow/raw/zori_all_zip.csv"
zillow <- fread(zillow_file)
zillow_long <- melt(zillow, id.vars = 1:4, variable.name = "Date", value.name = "ZORI")
zillow_long[,Date := as.character(Date)]
zillow_long[,year := as.integer(substr(Date,1,4))]
zillow_long[,month := as.integer(substr(Date,6,7))]
zillow_long[,ZORI := as.numeric(ZORI)]
setnames(zillow_long, "RegionName", "Zip5", skip_absent = T)
zillow_yr <- zillow_long[,.(mean_zori = mean_na0(ZORI), median_zori = median_na0(ZORI)), .(Zip5, year)]

# Merge with dt 
dt_zip <- dt[,.(median_rent = median_na0(RentPrice), mean_rent = mean_na0(RentPrice),
                median_log_rent = median_na0(log_rent), median_rent_sqft = median_na0(rent_sqft),
                median_tot_unit_val = median_na0(tot_unit_val), mean_tot_unit_val = mean_na0(tot_unit_val), 
                median_sale = median_na0(SalePrice), mean_sale = mean_na0(SalePrice), 
                median_sqft = median_na0(sqft), mean_sqft = mean_na0(sqft), 
                median_log_sqft = median_na0(log_sqft), N = .N),.(Zip5, year)]
dt_zip_month <- merge(dt_zip, zillow_long[,.(Zip5, year, month, ZORI)], by=c("Zip5", "year"), all.x=T, allow.cartesian=T)
dt_zip <- merge(dt_zip, zillow_yr, by=c("Zip5", "year"), all.x=T)

n_bad_zip <- length(setdiff(unique(dt_zip$Zip5), unique(zillow_yr$Zip5)))
n_bad_zori <- nrow(dt_zip_month[is.na(ZORI)])
print(paste0(n_bad_zip, " zips not matched with zillow out of ", uniqueN(dt_zip_month$Zip5), " zips."))
print(paste0(n_bad_zori, " month-zips with no ZORI out of ", nrow(dt_zip_month), " rows."))

# Compute HHI (assume filled owner strings are representative sample)
dt_owner <- dt[!is.na(owner_hhi) & owner_hhi != "",.N, .(owner_hhi, Zip5, year)]
dt_owner[,share := N/sum_na(N) * 100, .(Zip5, year)]
dt_owner[is.infinite(share), share := NA]
dt_hhi <- dt_owner[,.(hhi = sum_na(share^2)), .(Zip5, year)]
dt_zip <- merge(dt_zip, dt_hhi, by = c("Zip5", "year"), all.x = T)
n_bad_hhi <- nrow(dt_zip[is.na(hhi)])
print(paste0(n_bad_hhi, " zip-years with no HHI computed."))

# Plot event-time charts
vars <- c("ZORI", "tot_unit_val", "sale", "hhi", "sqft")
hhi_wide_list <- list() # Save hhi tables
hhi_long_list <- list()
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id == 3){
    next 
  }
  dt_zip[,group := as.character(NA)]
  
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
  target_zips <- unique(dt[year==norm_year & Merger_Owner_Name %in% target_name, Zip5])
  acquiror_zips <- unique(dt[year==norm_year & Merger_Owner_Name == acquiror_name, Zip5])
  treated_zips <- intersect(target_zips, acquiror_zips)
  dt_zip[Zip5 %in% treated_zips, group := "Treated"]
  dt_zip[,event_yr := as.integer(year - norm_year)]
  
  # Define control group: single-firm zips
  single_firm_zips <- unique(dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), Zip5])
  dt_zip[Zip5 %in% single_firm_zips, group := "Control"]
  
  # Save variables
  post_var <- paste0("post_", merge_id)
  dt_zip_month[, (post_var) := 0]
  dt_zip_month[year == norm_year & month > norm_month, (post_var) := 1]
  dt_zip_month[year > norm_year, (post_var) := 1]
  
  treated_var <- paste0("treated_", merge_id)
  dt_zip_month[, (treated_var) := 0]
  dt_zip_month[Zip5 %in% treated_zips, (treated_var) := 1]
  
  sample_var <- paste0("sample_", merge_id)
  dt_zip_month[, (sample_var) := get(treated_var)]
  dt_zip_month[Zip5 %in% single_firm_zips, (sample_var) := 1]
  
  for (var in vars){
    if (var == "ZORI"){
      dt_tmp <- melt(dt_zip, id.vars = c("Zip5", "event_yr", "year", "group"), measure.vars = c("median_zori", "median_rent"), 
                     variable.name = "rent_type", value.name = "price")
      
      # Annual counts 
      n_dt_zip <- dt_tmp[!is.na(group) & !is.na(price) & event_yr >= -5 & event_yr <= 5, .N, .(group, rent_type, event_yr)]
      n_dt_zip[,log_N := log(N)]
      ggplot(n_dt_zip, aes(x = rent_type, y = log_N, fill = group, group = group)) + geom_bar(stat="identity", position="dodge") +
        labs(y = "log N", title = paste0("Zip Price Counts, Merger:",merge_label)) + facet_wrap(~ event_yr) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
        ggsave(paste0(prepost_figs, "zip/n_zori_", merge_id, ".png"), dpi = "screen")
      
      # Median across zips
      dt_med_tmp <- dt_tmp[!is.na(group) & !is.na(price) & event_yr >= -5 & event_yr <= 5,.(median_price = median_na0(price)), .(group, rent_type, year)]
      ggplot(data = dt_med_tmp, aes(x = year, y = median_price, color = group, linetype = group, shape = rent_type)) + geom_point(size = 3, alpha = 0.5) +
        geom_line() + scale_linetype_manual(values=c("dashed", "solid")) + 
        geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") + 
        scale_x_continuous(breaks = unique(dt_med_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Median Zip Price by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "zip/prepost_med_zori_",merge_id, ".png"), width = 15, height = 6)
      
      # Prepost monthly 
      
      
    } else if (var == "hhi") {
      # Annual counts 
      n_dt_zip <- dt_zip[!is.na(group) & event_yr >= -5 & !is.na(hhi) & event_yr <= 5, .N, .(group, event_yr)]
      n_dt_zip[,log_N := log(N)]
      ggplot(n_dt_zip, aes(x = event_yr, y = log_N, fill = group, group = group)) + geom_bar(stat="identity", position="dodge") +
        labs(y = "log N", title = paste0("Zip HHI Counts, Merger:",merge_label)) + 
      ggsave(paste0(prepost_figs, "zip/n_hhi_", merge_id, ".png"), dpi = "screen")
      
      # Median HHI per year 
      dt_med_tmp <- dt_zip[!is.na(group) & event_yr >= -5 & event_yr <= 5,.(median_hhi = median_na0(hhi)), .(year, group)]
      ggplot(data = dt_med_tmp, aes(x = year, y = median_hhi, color = group)) + geom_point(shape = 16, size = 3, alpha = 0.5) +
        geom_line(linetype = "dashed") + geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") +
        scale_x_continuous(breaks = unique(dt_med_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Median Zip HHI by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "zip/prepost_med_hhi_",merge_id, ".png"), width = 15, height = 6)
      
      # Mean HHI per year 
      dt_mean_tmp <- dt_zip[!is.na(group) & event_yr >= -5 & event_yr <= 5,.(mean_hhi = mean_na0(hhi)), .(year, group)]
      ggplot(data = dt_mean_tmp, aes(x = year, y = mean_hhi, color = group)) + geom_point(shape = 16, size = 3, alpha = 0.5) +
        geom_line(linetype = "dashed") + geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") +
        scale_x_continuous(breaks = unique(dt_mean_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Mean Zip HHI by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "zip/prepost_mean_hhi_",merge_id, ".png"), width = 15, height = 6)
      
      # Save delta HHI tables
      dt[Zip5 %in% treated_zips & Merger_Owner_Name %in% target_name, merge_group := "Target"]
      dt[Zip5 %in% treated_zips & Merger_Owner_Name %in% acquiror_name, merge_group := "Acquiror"]
      dt[,event_yr := as.integer(year - norm_year)]
      dt_delta_hhi <- dt[event_yr == -1 & !is.na(owner_hhi) & owner_hhi != "", .(delta_hhi = 2 * sum_na(merge_group == "Target") * sum_na(merge_group == "Acquiror")/(.N^2)*100^2,
                                                                                 acquiror_share = sum_na(merge_group == "Acquiror")/.N*100, 
                                                                                 target_share = sum_na(merge_group == "Target")/.N*100), .(Zip5)]
      dt_delta_hhi[is.na(delta_hhi), delta_hhi := 0]
      dt_delta_hhi[is.na(acquiror_share), acquiror_share := 0]
      dt_delta_hhi[is.na(target_share), target_share := 0]
      dt_zip_tmp <- merge(dt_zip, dt_delta_hhi, by = "Zip5", all.x = T)
      dt_zip_tmp[,merge_label := merge_label]
      dt_zip_tmp[,merge_id := merge_id]
      dt_hhi_tmp <- dcast(dt_zip_tmp[event_yr >= -5 & group == "Treated"], 
                          Zip5 + delta_hhi + acquiror_share + target_share + merge_label + merge_id ~ event_yr, value.var = "hhi")
      setcolorder(dt_hhi_tmp, "merge_label")
      
      hhi_wide_list[[merge_id]] <- dt_hhi_tmp
      hhi_long_list[[merge_id]] <- dt_zip_tmp[event_yr >= -5 & group == "Treated", .(merge_id, merge_label, Zip5, event_yr, delta_hhi, hhi, 
                                                                                     acquiror_share, target_share)]
      
      } else{
      dt_tmp <- melt(dt_zip, id.vars = c("Zip5", "event_yr","year", "group"), measure.vars = paste0(c("median_", "mean_"), var), 
                     variable.name = "agg_var", value.name = var)
      
      # Annual counts 
      n_dt_zip <- dt_tmp[!is.na(group) & !is.na(get(var)) & event_yr >= -5 & event_yr <= 5, .N, by = c("group", "agg_var", var, "event_yr")]
      n_dt_zip[,log_N := log(N)]
      ggplot(n_dt_zip, aes(x = agg_var, y = log_N, fill = group, group = group)) + geom_bar(stat="identity", position="dodge") +
        labs(y = "log N", title = paste0("Zip ", var, " Counts, Merger:",merge_label)) + facet_wrap(~ event_yr) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      ggsave(paste0(prepost_figs, "zip/n_", var, "_", merge_id, ".png"), dpi = "screen", width = 6, height = 6)
      
      # Median across zips
      dt_med_tmp <- dt_tmp[!is.na(group) & !is.na(get(var)) & event_yr >= -5 & event_yr <= 5,.(median_var = median_na(get(var))), .(agg_var, year, group)]
      ggplot(data = dt_med_tmp, aes(x = year, y = median_var, color = group, linetype = group, shape = agg_var)) + geom_point(size = 3, alpha = 0.5) +
        geom_line() + scale_linetype_manual(values=c("dashed", "solid")) + 
        geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced, linetype="dashed") +
        scale_x_continuous(breaks = unique(dt_med_tmp$year)) +
        labs(x = "Normalized Year", y = var, title = paste("Median Zip", var, "by Year, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "zip/prepost_med_", var, "_",merge_id, ".png"), width = 15, height = 6)
      
      # Mean across zips
      # dt_mean_tmp <- dt_tmp[!is.na(group) & !is.na(get(var)) & event_yr >= -5 & event_yr <= 5,.(mean_var = mean_na(get(var))), .(agg_var, year, group)]
      # ggplot(data = dt_mean_tmp, aes(x = event_yr, y = mean_var, color = group, shape = agg_var)) + geom_point(size = 3, alpha = 0.5) +
      #   geom_line(linetype = "dashed") + geom_vline(xintercept=norm_year) + geom_vline(xintercept=year_announced) +
      #   scale_x_continuous(breaks = unique(dt_mean_tmp$year)) +
      #   labs(x = "Normalized Year", y = var, title = paste("Mean Zip", var, "by Year, Merger:",merge_label)) + 
      #   ggsave(paste0(prepost_figs, "zip/prepost_mean_", var, "_",merge_id, ".png"), width = 15, height = 6)
      }
  }
}
hhi_wide <- rbindlist(hhi_wide_list, fill = T)
setorder(hhi_wide, merge_label, -delta_hhi)
fwrite(hhi_wide, paste0(mergers_path, "hhi_wide.csv"))

hhi_long <- rbindlist(hhi_long_list)
setorder(hhi_long, merge_label, event_yr, -delta_hhi)
fwrite(hhi_long, paste0(mergers_path, "hhi_long.csv"))

# Record multi_merge zips
treated_names <- grep("^treated_", names(dt), value = T)
dt[,treated_overlap := rowSums(.SD, na.rm=T), .SDcols = treated_names]
multi_zips <- unique(dt[treated_overlap > 1, Zip5])
dt[,multi_merge := 0]
dt[Zip5 %in% multi_zips, multi_merge := 1]
dt_zip_month[,multi_merge := 0]
dt_zip_month[Zip5 %in% multi_zips, multi_merge := 1]

# Delta HHI long to wide for each merger
hhi_merge_wide <- dcast(hhi_wide, Zip5 ~ merge_id, value.var = c("delta_hhi", "merge_label", "acquiror_share", "target_share"))

# Save zip panel with HHI and Zillow 
dt_zip_month <- merge(dt_zip_month, dt_hhi, by = c("Zip5", "year"), all.x = T)
dt_zip_month <- merge(dt_zip_month, hhi_merge_wide, by = c("Zip5"), all.x = T)

# NA delta HHI is 0 by definition
hhi_names <- grep("^delta_hhi_", names(hhi_merge_wide), value = T)
for (hhi_col in hhi_names){
  dt_zip_month[is.na(get(hhi_col)), (hhi_col) := 0]
}
fwrite(dt_zip_month, paste0(data_path, "panel_zip_hhi.csv"))

# Save sample panel with HHI
sample_names <- grep("^sample_", names(dt), value = T)
dt[,sample_overlap := rowSums(.SD, na.rm=T), .SDcols = sample_names]
dt_tot <- merge(dt[sample_overlap >= 1], dt_hhi, by = c("Zip5", "year"), all.x = T)
dt_tot <- merge(dt_tot, hhi_merge_wide, by = c("Zip5"), all.x = T)

# NA delta HHI is 0 by definition
hhi_names <- grep("^delta_hhi_", names(hhi_merge_wide), value = T)
for (hhi_col in hhi_names){
  dt_tot[is.na(get(hhi_col)), (hhi_col) := 0]
}
fwrite(dt_tot, paste0(data_path, "panel_hhi.csv"))

# Save rent-restricted panel 
fwrite(dt_tot[!is.na(RentPrice) & RentPrice != 0], paste0(data_path, "panel_hhi_rent.csv"))
