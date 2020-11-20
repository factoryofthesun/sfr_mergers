# ===================================================================
# Mergers Analysis
# ===================================================================
# Figures and initial DiD of the effect of SFR mergers 
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

data_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"
prepost_figs <- "/gpfs/loomis/project/humphries/rl874/mergers_project/figs/prepost/"

# Read and concat merger identified panel data
files <- paste0(data_path, setdiff(grep("_mergers\\.csv$", list.files(data_path), value = T), c("GA_mergers_good.csv")))
size <- sum_na(file.info(files)$size)/1e9
print(paste("Total size of merger state files:", size, "GB"))

t0 <- Sys.time()
micro_list <- lapply(files, function(x) fread(x, integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted"))))
# micro_list <- lapply(files, function(x) fread(x, integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")),nrows=1e6))
dt <- rbindlist(micro_list, fill = T)
rm(micro_list)
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# ============= (0) Preprocessing ===================================================
# Clean variables
# Fix total unit value 
dt[is.na(unit_n_clean)|unit_n_clean==0, unit_n_clean := 1]
dt[,tot_unit_val := tot_val/unit_n_clean]

# Redefine ids 
dt[,id := .GRP, .(fips, apn_unformatted, apn_num)]

#TODO: CAN REMOVE THE SUBSIDIARIES ONCE WE BUILD + SAVE IDENTIFIED COMMON MERGER NAMES
# Read in mergers data 
company_suffixes <- c("inc", "llc")

mergers <- fread(paste0(mergers_path, "mergers_final_old.csv"))
names(mergers) <- gsub("[[:space:]]", "", names(mergers))
mergers[,DateEffective := as.POSIXct(DateEffective, format = "%m/%d/%y")]
mergers[,DateAnnounced := as.POSIXct(DateAnnounced, format = "%m/%d/%y")]
mergers[,Year := year(DateEffective)]
mergers[,Target_Entities := list()]
mergers[,Acquiror_Entities := list()]

# Entity data
base_company_names <- c("American Homes 4 Rent", "Invitation Homes Inc", "Starwood Waypoint Residential", 
                        "Colony American Homes Inc", "Ellington Housing", "American Residential Ppty Inc", 
                        "Tricon Capital Group Inc", "Silver Bay Realty Trust Corp", "Beazer Pre-Owned Rental Homes",
                        "Broadtree Residential", "Mainstreet Renewal LLC")
entity_file_names <- c("american_homes", "invitation_homes", "colony_starwood", "colony_american", "ellington_housing",
                       "american_residential_ppty", "tricon_american_homes", "silver_bay")

for (i in 1:length(base_company_names)){
  base_name <- base_company_names[i]
  tmp <- c(base_name)
  if (!(base_name %in% c("Beazer Pre-Owned Rental Homes",
                         "Broadtree Residential", "Mainstreet Renewal LLC", "Amherst Holdings LLC"))){ # No subsidiaries for these
    entity_tmp <- setDT(read.table(paste0(mergers_path, "entities/", entity_file_names[i], "_entities.txt"), header = F,
                                   sep = "\t", strip.white=T, fill = T))
    entity_tmp <- entity_tmp[,.(V1)]
    setnames(entity_tmp, "V1", "Entity_Name")
    entity_tmp[,Entity_Name := levels(Entity_Name)[as.numeric(Entity_Name)]]
    tmp <- c(tmp, entity_tmp$Entity_Name)
    entity_tmp[,`:=`(Year = NA, Base_Name = base_name)]
    if (i == 1){
      entity_dt <- entity_tmp
    } else{
      entity_dt <- rbindlist(list(entity_dt, entity_tmp), use.names = T)
    }
  }
  if (base_name == "Mainstreet Renewal LLC"){ tmp <- c(tmp, "Amherst Holdings LLC")}
  mergers[TargetName == base_name, Target_Entities := list(tmp)]
  mergers[AcquirorName == base_name, Acquiror_Entities := list(tmp)]
  entity_dt <- rbindlist(list(entity_dt, data.table(Year = NA, Entity_Name = base_name, Base_Name = base_name)), use.names=T)
}

mergers[,Target_Entities_Clean := list(c("placeholder"))]
mergers[,Target_Entities_Clean := as.list(Target_Entities_Clean)]
mergers[,Acquiror_Entities_Clean := list(c("placeholder"))]
mergers[,Acquiror_Entities_Clean := as.list(Acquiror_Entities_Clean)]
for (i in 1:nrow(mergers)){
  tmp_target <- unlist(mergers[i, Target_Entities])
  tmp_target <- trimws(gsub("[[:punct:]]", " ", tmp_target))
  tmp_target <- trimws(gsub(paste0(company_suffixes, collapse = "|"), "", tmp_target, ignore.case = T))
  tmp_target <- unique(toupper(gsub("\\s\\s+", " ", tmp_target)))
  mergers[i, Target_Entities_Clean := list(c(tmp_target))]
  
  tmp <- unlist(mergers[i, Acquiror_Entities])
  tmp <- trimws(gsub("[[:punct:]]", " ", tmp))
  tmp <- trimws(gsub(paste0(company_suffixes, collapse = "|"), "", tmp, ignore.case = T))
  tmp <- unique(setdiff(toupper(gsub("\\s\\s+", " ", tmp)), tmp_target)) # Acquiror shouldn't own same properties
  mergers[i, Acquiror_Entities_Clean := list(tmp)]
}

mergers[,Year := year(DateEffective)]
mergers[is.na(Year), Year := year(DateAnnounced)]

mergers[,MergeID_1 := .GRP, .(Year, AcquirorName)]
mergers[,MergeID_2 := MergeID_1]

# ============= (0.5) Diagnostics ===================================================
# Label merging company properties with common name 
#TODO: REMOVE THIS UPON NEXT BUILD
for (i in 1:nrow(mergers)){
  yr <- mergers[i, Year]
  targets_to_check <- unlist(mergers[i, Target_Entities_Clean])
  target_name <- mergers[i, TargetName]
  target_id <- mergers[i, MergeID_1]
  acquirors_to_check <- unlist(mergers[i, Acquiror_Entities_Clean])
  acquiror_name <- mergers[i, AcquirorName]
  acquiror_id <- mergers[i, MergeID_2]
  
  # Assign standardized merging firm names 
  dt[merger_name %in% targets_to_check, Merger_Owner_Name := target_name] 
  dt[merger_name %in% acquirors_to_check, Merger_Owner_Name := acquiror_name]
}
dt[!is.na(Merger_Owner_Name), merge_affected := T]
dt[,log_rent := log(RentPrice)]
dt[is.infinite(log_rent), log_rent := NA]
dt[,rent_sqft := RentPrice/sqft]
dt[is.infinite(rent_sqft), rent_sqft := NA]

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

# ============= (1) Figures ===================================================
# Pre-post median rents --------------------------------------------------------------

# Consolidate into one chart -- grouping by firms involved in each respective merger 
# Concat separate panels for each merger to deal with ids involved in multiple merge events
merge_labels <- c("Beazer/Ellington - American Homes 4 Rent", "Beazer/Ellington - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Broadtree Residential - Mainstreet Renewal", 
                  "Broadtree Residential - Mainstreet Renewal", "Silver Bay - Tricon Capital")
colors <- c("coral1","coral1", "dodgerblue", "darkorange", "forestgreen", "gold", "gold", "plum")
mergers$label <- merge_labels
mergers$colors <- colors

mergers_list <- list()
for (merge_id in unique(mergers$MergeID_1)){
  # Exclude Broadtree/Mainstreet Renewal
  if (!(merge_id %in% c(5,6))){
    ids <- unique(dt[TargetID == merge_id|AcquirorID == merge_id, id])
    tmp <- dt[id %in% ids]
    merge_name <- unique(mergers[MergeID_1 == merge_id, label])
    mergers_list[[merge_id]] <- tmp[,.(Median_Rent = median_na0(RentPrice), Mean_Rent = mean_na0(RentPrice), merger = merge_name), year]    
  }
}
dup_dt <- rbindlist(mergers_list)
rm(mergers_list)
getAvailMem()

ggplot(data = dup_dt[year >= 2000], aes(x = year, y = Median_Rent, color = merger)) + geom_line() + 
  scale_color_manual(values = unique(colors[-6:-7]), breaks = unique(merge_labels[-6:-7]), labels = unique(merge_labels[-6:-7])) + 
  geom_vline(data = mergers[-6:-7], aes(xintercept = Year, color=label)) +
  ggsave(paste0(mergers_path, "mergers_median_trends.png"), width = 15, height = 6)

ggplot(data = dup_dt[year >= 2000], aes(x = year, y = Mean_Rent, color = merger)) + geom_line() + 
  scale_color_manual(values = unique(colors[-6:-7]), breaks = unique(merge_labels[-6:-7]), labels = unique(merge_labels[-6:-7])) + 
  geom_vline(data = mergers[-6:-7], aes(xintercept = Year, color=label)) +
  ggsave(paste0(mergers_path, "mergers_mean_trends.png"), width = 15, height = 6)

# For each merger: multiple scatterplots for each possible control group + plot multiple features
# 1) within-zip non-merging properties
# 2) outside-zip same-firm properties but in unmerged zips
# 3) outside-zip unmerged zips with presence of one firm 

vars <- c("RentPrice", "sqft", "tot_unit_val", "SalePrice", "unit_n_clean", "foreclosure_clean", "resale_or_new", 
          "REO_Sale", "log_rent", "rent_sqft")
for (merge_id in unique(mergers$MergeID_1)){
  # Exclude Broadtree/Mainstreet Renewal
  if (!(merge_id %in% c(5,6))){
    # Merger vars 
    merge_label <- unique(mergers[MergeID_1 == merge_id, label])
    target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
    acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
    norm_year <- unique(mergers[MergeID_1 == merge_id, Year])
    
    # Adjust panel depending on merger
    target_zips <- unique(dt[year==norm_year & Merger_Owner_Name %in% target_name, Zip5])
    acquiror_zips <- unique(dt[year==norm_year & Merger_Owner_Name == acquiror_name, Zip5])
    treated_zips <- intersect(target_zips, acquiror_zips)
    dt[Zip5 %in% treated_zips & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), group := "Treated"]
    dt[,event_yr := as.integer(year - norm_year)]
    dt[event_yr > 6, event_yr := NA] # Sometimes strange range 
    
    # 1) Control = merging-zip non-affected 
    dt[is.na(group) & Zip5 %in% treated_zips, group := "Control"]
    # Annual counts 
    n_dt <- dt[!is.na(group), .N, .(group, event_yr)]
    ggplot(n_dt, aes(x = event_yr, y = N, fill = group, group = group)) + geom_bar(stat="identity", position="stack") +
      labs(y = "count", title = paste0("Control: within-zip non-firm props, Merger:",merge_label)) + 
      ggsave(paste0(prepost_figs, "n_control1_", merge_id, ".png"), dpi = "screen")
    for (var in vars){
      dt[,tmp := get(var)]
      
      if (!(var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale"))){
      ggplot(data = dt[!is.na(group) & event_yr >= -10], aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 2, alpha = 0.5) +
        geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste(var, "by Year, Control: within-zip non-firm props, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_control1_", var,"_",merge_id, ".png"), width = 15, height = 6)
      }
      
      # V2: median per year 
      if (var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale")){
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(mean_na(tmp))), .(group, event_yr)]
      } else {
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(median_na0(tmp))), .(group, event_yr)]
      }
      ggplot(data = dt_tmp, aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 4, alpha = 0.75) +
        geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Control: within-zip non-firm props, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_control1_median_", var,"_",merge_id, ".png"), width = 15, height = 6)
    
      # V3: mean per year 
      if (!(var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale"))){
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(mean_na(tmp))), .(group, event_yr)]
        ggplot(data = dt_tmp, aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 4, alpha = 0.75) +
          geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Control: within-firm unmerged zips, Merger:",merge_label)) + 
          ggsave(paste0(prepost_figs, "prepost_control1_mean_", var,"_",merge_id, ".png"), width = 15, height = 6)
      }  
      
    }
    
    # 2) Control = merging firms in unmerged zips
    dt[group == "Control", group := NA]
    dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), group := "Control"]
    # Annual counts 
    n_dt <- dt[!is.na(group), .N, .(group, event_yr)]
    ggplot(n_dt, aes(x = event_yr, y = N, fill = group, group = group)) + geom_bar(stat="identity", position="stack") +
      labs(y = "count", title = paste0("Control: within-firm unmerged zips, Merger:",merge_label)) + 
      ggsave(paste0(prepost_figs, "n_control2_", merge_id, ".png"), dpi = "screen")
    for (var in vars){
      dt[,tmp := get(var)]
      if (!(var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale"))){
      ggplot(data = dt[!is.na(group) & event_yr >= -10], aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 2, alpha = 0.5) +
        geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste(var, "by Year, Control: within-firm unmerged zips, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_control2_", var,"_",merge_id,".png"), width = 15, height = 6)
      }
      
      # V2: median per year 
      if (var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale")){
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(mean_na(tmp))), .(group, event_yr)]
      } else {
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(median_na0(tmp))), .(group, event_yr)]
      }      
      ggplot(data = dt_tmp, aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 4, alpha = 0.75) +
        geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Control: within-firm unmerged zips, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_control2_median_", var,"_",merge_id, ".png"), width = 15, height = 6)
      
      # V3: mean per year 
      if (!(var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale"))){
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(mean_na(tmp))), .(group, event_yr)]
        ggplot(data = dt_tmp, aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 4, alpha = 0.75) +
          geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Control: within-firm unmerged zips, Merger:",merge_label)) + 
          ggsave(paste0(prepost_figs, "prepost_control2_mean_", var,"_",merge_id, ".png"), width = 15, height = 6)
      }      
    }
   
    # 3) Control = all properties unmerged zips with 1 firm
    single_firm_zips <- unique(dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), Zip5])
    dt[group == "Control", group := NA]
    dt[Zip5 %in% single_firm_zips, group := "Control"]
    # Annual counts 
    n_dt <- dt[!is.na(group), .N, .(group, event_yr)]
    ggplot(n_dt, aes(x = event_yr, y = N, fill = group, group = group)) + geom_bar(stat="identity", position="stack") +
      labs(y = "count", title = paste0("Control: single-firm zips, Merger:",merge_label)) + 
      ggsave(paste0(prepost_figs, "n_control3_", merge_id, ".png"), dpi = "screen")
    for (var in vars){
      dt[,tmp := get(var)]
      if (!(var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale"))){
      ggplot(data = dt[!is.na(group) & event_yr >= -10], aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 2, alpha = 0.5) +
        geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste(var, "by Year, Control: single-firm zips, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_control3_", var,"_",merge_id, ".png"), width = 15, height = 6)
      }
      
      # V2: median per year 
      if (var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale")){
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(mean_na(tmp))), .(group, event_yr)]
      } else {
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(median_na0(tmp))), .(group, event_yr)]
      }
      ggplot(data = dt_tmp, aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 4, alpha = 0.75) +
        geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Control: single-firm zips, Merger:",merge_label)) + 
        ggsave(paste0(prepost_figs, "prepost_control3_median_", var,"_",merge_id, ".png"), width = 15, height = 6)
      
      # V3: mean per year 
      if (!(var %in% c("foreclosure_clean", "resale_or_new", "REO_Sale"))){
        dt_tmp <- dt[!is.na(group) & event_yr >= -10,.(tmp = as.numeric(mean_na(tmp))), .(group, event_yr)]
        ggplot(data = dt_tmp, aes(x = event_yr, y = tmp, color = group)) + geom_point(shape = 16, size = 4, alpha = 0.75) +
          geom_vline(xintercept=0) + labs(x = "Normalized Year", y = var, title = paste("Median", var, "by Year, Control: within-firm unmerged zips, Merger:",merge_label)) + 
          ggsave(paste0(prepost_figs, "prepost_control3_mean_", var,"_",merge_id, ".png"), width = 15, height = 6)
      }  
      
    }
  }
}

# Robustness checks --------------------------------------------------------------
#TODO: property characteristics across different groups (within-zip properties by merge affected, across zip different mergers), etc


# ============= (2) Estimation ===================================================
estimate_path <- paste0(mergers_path, "estimates/")
library(lfe)
library(stargazer)

# Basic DiD (2-way FE with just the treated props and different time periods)
reg0 <- felm(RentPrice ~ treated|factor(year) + factor(id), data = dt[merge_affected == T & year >= 2000])
reg1 <- felm(RentPrice ~ treated + sqft + tot_unit_val|factor(year) + factor(id), data = dt[merge_affected == T & year >= 2000])
reg2 <- felm(log_rent ~ treated|factor(year) + factor(id), data = dt[merge_affected == T & year >= 2000])
reg3 <- felm(log_rent ~ treated + sqft + tot_unit_val|factor(year) + factor(id), data = dt[merge_affected == T & year >= 2000])
reg4 <- felm(rent_sqft ~ treated|factor(year) + factor(id), data = dt[merge_affected == T & year >= 2000])
reg5 <- felm(rent_sqft ~ treated + sqft + tot_unit_val|factor(year) + factor(id), data = dt[merge_affected == T & year >= 2000])

out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), title = "2-Way FE"))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)
cat(paste(out, "\n\n"), file = paste0(estimate_path, "initial_did.tex"), append=F)

# Each merger: basic DiD -- pre-post flags 
for (merge_id in unique(mergers$MergeID_1)){
  # Exclude Broadtree/Mainstreet Renewal
  if (!(merge_id %in% c(5,6))){
    # Merger vars 
    merge_label <- unique(mergers[MergeID_1 == merge_id, label])
    target_name <- unique(mergers[MergeID_1 == merge_id, TargetName])
    acquiror_name <- unique(mergers[MergeID_1 == merge_id, AcquirorName])
    norm_year <- unique(mergers[MergeID_1 == merge_id, Year])
    
    # Adjust panel depending on merger
    target_zips <- unique(dt[year==norm_year & Merger_Owner_Name %in% target_name, Zip5])
    acquiror_zips <- unique(dt[year==norm_year & Merger_Owner_Name == acquiror_name, Zip5])
    treated_zips <- intersect(target_zips, acquiror_zips)
    dt[,event_yr := as.integer(year - norm_year)]
    dt[Zip5 %in% treated_zips & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), treated_tmp := 1]
    dt[, post := as.integer(event_yr > 0)]
    
    # 1) Control = merging-zip non-affected 
    dt[is.na(treated_tmp) & Zip5 %in% treated_zips, treated_tmp := 0]
    reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    
    out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                    column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                    title = paste0("Merger: ", merge_label,", Control: Within-Zip Non-Firm")))
    
    # Wrap tabular environment in resizebox
    out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
    out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
    
    # Set position
    out <- gsub("!htbp", "H", out, fixed = T)
    cat(paste(out, "\n\n"), file = paste0(estimate_path, "initial_did.tex"), append=T)
    
    # 2) Control = merging firms in unmerged zips
    dt[treated_tmp == 0, treated_tmp := NA]
    dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), treated_tmp := 0]
    reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    
    out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                    column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                    title = paste0("Merger: ", merge_label,", Control: Unmerged Firm")))
    
    # Wrap tabular environment in resizebox
    out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
    out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
    
    # Set position
    out <- gsub("!htbp", "H", out, fixed = T)
    cat(paste(out, "\n\n"), file = paste0(estimate_path, "initial_did.tex"), append=T)
    
    # 3) Control = all properties unmerged zips with 1 firm
    single_firm_zips <- unique(dt[!(Zip5 %in% treated_zips) & (Merger_Owner_Name %in% target_name | Merger_Owner_Name == acquiror_name), Zip5])
    dt[treated_tmp == 0, treated_tmp := NA]
    dt[Zip5 %in% single_firm_zips, treated_tmp := 0]
    reg0 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg1 <- felm(RentPrice ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    reg2 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg3 <- felm(log_rent ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    reg4 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post, data = dt[year >= 2000])
    reg5 <- felm(rent_sqft ~ treated_tmp + post + treated_tmp:post + sqft + tot_unit_val, data = dt[year >= 2000])
    
    out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                    column.labels = c("Rent", "Log Rent", "Rent/Sqft"), column.separate = c(2,2,2), 
                                    title = paste0("Merger: ", merge_label,", Control: Single Firm Zips")))
    
    # Wrap tabular environment in resizebox
    out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
    out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
    
    # Set position
    out <- gsub("!htbp", "H", out, fixed = T)
    cat(paste(out, "\n\n"), file = paste0(estimate_path, "initial_did.tex"), append=T)
    
  }
}


