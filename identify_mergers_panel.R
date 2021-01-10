# ===================================================================
# Identifying Merge Affected Rows 
# ===================================================================
# Script takes the following steps
#   - Concats micro MLS merged data for identified states
#   - Merges in Josh's owner names matching table 
#   - String matches against owner names to identify the merger affected properties and time periods

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
library(RecordLinkage)
setwd("~/project")
Sys.umask(000)
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_dedup_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_misc_rent.R")
source("/gpfs/loomis/project/humphries/rl874/rent_project/code/cleaning/fn_clean_cl.R")

micro_path <- "/gpfs/loomis/scratch60/humphries/rl874/pipeline_out/micro/"
match_path <- "/gpfs/loomis/scratch60/humphries/rl874/mergers/match_tables/"
mergers_path <- "/gpfs/loomis/project/humphries/rl874/mergers_project/"

parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-s", "--state", type="character", default="AZ", 
                    help="State file to build [default %(default)s]",
                    metavar = "abbrev")

args <- parser$parse_args()

# Set inputs
state <- args$state

# Read in micro data 
t0 <- Sys.time()
panel <- fread(paste0(micro_path, state, "_mls.csv"))
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# Testing
#panel <- fread(paste0(micro_path, "/", state, "_mls.csv"), nrows = 1e4)

# Owner name matching ------------------------------------------------
# Merge in owner matched names
matched_names <- fread(paste0(match_path, state, "_matched_owners.csv"))
n_matched_names <- uniqueN(matched_names$owner_name_clean)
n_owner_names <- uniqueN(panel$owner_name_clean)
print(paste("There are", n_matched_names, "unique names in matched owners data and", n_owner_names, "unique owner names."))
panel <- merge(panel, matched_names, by = "owner_name_clean", all.x = T)
n_bad <- nrow(panel[(is.na(owner_matched) | owner_matched == "") & !(is.na(owner_name_clean) | owner_name_clean == "")])
if (n_bad > 0){
   print(paste("Warning: there are", n_bad,"names that have not been matched. Imputing with original name..."))
   panel[is.na(owner_matched) | owner_matched == "", owner_matched := owner_name_clean]
}
rm(matched_names)

# Check owner names in panel
n_good <- nrow(panel[!is.na(owner_name_clean) & owner_name_clean != ""])
print(paste("There are", n_good, "rows with owner name out of", nrow(panel)))

# Read in mergers data 
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
mergers[,Target_Entities := list()]
mergers[,Acquiror_Entities := list()]

# Entity data
base_company_names <- c("American Homes 4 Rent", "Invitation Homes Inc", "Starwood Waypoint Residential", 
                        "Colony American Homes Inc", "Ellington Housing", "American Residential Ppty Inc", 
                        "Tricon Capital Group Inc", "Silver Bay Realty Trust Corp", "Beazer Pre-Owned Rental Homes")
entity_file_names <- c("american_homes", "invitation_homes", "colony_starwood", "colony_american", "ellington_housing",
                       "american_residential_ppty", "tricon_american_homes", "silver_bay", "beazer")

for (i in 1:length(base_company_names)){
   base_name <- base_company_names[i]
   tmp <- c(base_name)
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
   mergers[TargetName == base_name, Target_Entities := list(tmp)]
   mergers[AcquirorName == base_name, Acquiror_Entities := list(tmp)]
   entity_dt <- rbindlist(list(entity_dt, data.table(Year = NA, Entity_Name = base_name, Base_Name = base_name)), use.names=T)
}

# Clean up all the names
company_suffixes <- c("inc", "llc")

panel[,owner_matched := trimws(gsub("[[:punct:]]", " ", owner_matched))]
panel[,owner_matched := trimws(gsub(paste0(company_suffixes, collapse = "|"), "", owner_matched, ignore.case = T))]
panel[,owner_matched := toupper(gsub("\\s\\s+", " ", owner_matched))]

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

# =============================== Mergers Identification ===============================
# Step 1: direct string match on names
for (i in 1:nrow(mergers)){
   yr <- mergers[i, Year]
   targets_to_check <- unlist(mergers[i, Target_Entities_Clean])
   target_name <- mergers[i, TargetName]
   target_id <- mergers[i, MergeID_1]
   acquirors_to_check <- unlist(mergers[i, Acquiror_Entities_Clean])
   acquiror_name <- mergers[i, AcquirorName]
   acquiror_id <- mergers[i, MergeID_2]
   
   panel[year == yr & owner_name_clean %in% targets_to_check, `:=`(TargetID = target_id)]   
   panel[year == yr & owner_name_clean %in% acquirors_to_check, `:=`(AcquirorID = acquiror_id)]
   
   # Direct string match on original owner name
   panel[owner_name_clean %in% targets_to_check, Merger_Owner_Name := target_name] 
   panel[owner_name_clean %in% acquirors_to_check, Merger_Owner_Name := acquiror_name]
   panel[owner_name_clean %in% targets_to_check, source := "direct"] 
   panel[owner_name_clean %in% acquirors_to_check, source := "direct"]
}
print("Names captured from direct string matching")
print(panel[!is.na(Merger_Owner_Name), .N, .(Merger_Owner_Name, year)][order(-N)])

# Step 2: Use RecordLinkage on remaining names
# First on original names
unique_panel_names <- unique(panel[!is.na(owner_name_clean) & owner_name_clean != "" & is.na(Merger_Owner_Name), owner_name_clean])
d1 <- data.frame("owner_name_clean" = unique_panel_names)
d2 <- data.frame("merger_name" = unique(c(unlist(mergers$Target_Entities_Clean), unlist(mergers$Acquiror_Entities_Clean))))
n_owners <- nrow(d1)
n_merge_entities <- nrow(d2)
print(paste("Linkage for", n_owners, "original owner names and", n_merge_entities, "merger entities."))
t0 <- Sys.time()
d2_tmp <- setDT(copy(d2))
d2_tmp[,id2 := .I]
i <- 1
pairs_list <- list()
while (i <= nrow(d1)){
   end <- min(i+5e5-1, nrow(d1))
   d1_tmp <- data.frame("owner_name_clean" = d1[i:end,])
   output <- compare.linkage(d1_tmp, d2, strcmp = T)
   tmp_pairs <- setDT(output$pairs)
   tmp_pairs[owner_name_clean >= 0.95,is_match := 1]
   
   tmp_pairs <- tmp_pairs[is_match == 1]
   setnames(tmp_pairs, "owner_name_clean", "jw_score")
   d1_tmp <- setDT(d1_tmp)
   d1_tmp[,id1 := .I]
   
   # Choose highest score match for a given id1 if multiple 
   tmp_pairs[,max_jw := max_na(jw_score), id1]
   tmp_pairs <- tmp_pairs[jw_score == max_jw]
   tmp_pairs <- tmp_pairs[!duplicated(tmp_pairs$id1)] # In case of tie
   tmp_pairs <- merge(tmp_pairs, d1_tmp, by = "id1")
   tmp_pairs <- merge(tmp_pairs, d2_tmp, by = "id2")
   pairs_list[[i]] <- tmp_pairs
   i <- i + 5e5
   print(paste0("i: ", i))
   getAvailMem()
}
matched_pairs <- rbindlist(pairs_list)
matched_pairs[,owner_name_clean := levels(owner_name_clean)[as.numeric(owner_name_clean)]]
matched_pairs[,merger_name := levels(merger_name)[as.numeric(merger_name)]]
t1 <- Sys.time()
print(paste("Record linkage took", 
            difftime(t1, t0, units="mins"), "minutes."))
rm(pairs_list)

# ID all merge affected properties 
panel <- merge(panel, matched_pairs[,.(jw_score, owner_name_clean, merger_name)], by = "owner_name_clean", all.x = T)
rm(matched_pairs)
getAvailMem()

# ID each merge event separately
for (i in 1:nrow(mergers)){
   yr <- mergers[i, Year]
   targets_to_check <- unlist(mergers[i, Target_Entities_Clean])
   target_name <- mergers[i, TargetName]
   target_id <- mergers[i, MergeID_1]
   acquirors_to_check <- unlist(mergers[i, Acquiror_Entities_Clean])
   acquiror_name <- mergers[i, AcquirorName]
   acquiror_id <- mergers[i, MergeID_2]
   
   panel[year == yr & merger_name %in% targets_to_check & !is.na(TargetID), `:=`(TargetID = target_id)]   
   panel[year == yr & merger_name %in% acquirors_to_check & !is.na(AcquirorID), `:=`(AcquirorID = acquiror_id)]
   
   # Assign standardized merging firm names 
   panel[merger_name %in% targets_to_check & is.na(Merger_Owner_Name), Merger_Owner_Name := target_name] 
   panel[merger_name %in% acquirors_to_check & is.na(Merger_Owner_Name), Merger_Owner_Name := acquiror_name]
   panel[merger_name %in% targets_to_check & is.na(Merger_Owner_Name), source := "linkage"] 
   panel[merger_name %in% acquirors_to_check & is.na(Merger_Owner_Name), source := "linkage"]
}
print(paste0("Total merging firm observations found after RecordLinkage: ", 
             nrow(panel[!is.na(Merger_Owner_Name) & Merger_Owner_Name != ""])))

# Second on matched names -----------------------------------------------------------------
# unique_panel_names <- unique(panel[!is.na(owner_matched) & owner_matched != "" & is.na(Merger_Owner_Name), owner_matched])
# d1 <- data.frame("owner_matched" = unique_panel_names)
# d2 <- data.frame("merger_name" = unique(c(unlist(mergers$Target_Entities_Clean), unlist(mergers$Acquiror_Entities_Clean))))
# n_owners <- nrow(d1)
# n_merge_entities <- nrow(d2)
# print(paste("Linkage for", n_owners, "original owner names and", n_merge_entities, "merger entities."))
# t0 <- Sys.time()
# d2_tmp <- setDT(copy(d2))
# d2_tmp[,id2 := .I]
# i <- 1
# pairs_list <- list()
# while (i <= nrow(d1)){
#    end <- min(i+5e5-1, nrow(d1))
#    d1_tmp <- data.frame("owner_matched" = d1[i:end,])
#    output <- compare.linkage(d1_tmp, d2, strcmp = T)
#    tmp_pairs <- setDT(output$pairs)
#    tmp_pairs[owner_matched >= 0.95,is_match := 1]
#    
#    tmp_pairs <- tmp_pairs[is_match == 1]
#    setnames(tmp_pairs, "owner_matched", "jw_score")
#    d1_tmp <- setDT(d1_tmp)
#    d1_tmp[,id1 := .I]
#    
#    # Choose highest score match for a given id1 if multiple 
#    tmp_pairs[,max_jw := max_na(jw_score), id1]
#    tmp_pairs <- tmp_pairs[jw_score == max_jw]
#    tmp_pairs <- tmp_pairs[!duplicated(tmp_pairs$id1)] # In case of tie
#    tmp_pairs <- merge(tmp_pairs, d1_tmp, by = "id1")
#    tmp_pairs <- merge(tmp_pairs, d2_tmp, by = "id2")
#    pairs_list[[i]] <- tmp_pairs
#    i <- i + 5e5
#    print(paste0("i: ", i))
#    getAvailMem()
# }
# matched_pairs <- rbindlist(pairs_list)
# matched_pairs[,owner_matched := levels(owner_matched)[as.numeric(owner_matched)]]
# matched_pairs[,merger_name := levels(merger_name)[as.numeric(merger_name)]]
# t1 <- Sys.time()
# print(paste("Record linkage took", 
#             difftime(t1, t0, units="mins"), "minutes."))
# rm(pairs_list)
# 
# # ID all merge affected properties 
# panel0 <- panel[!is.na(Merger_Owner_Name)]
# panel1 <- panel[is.na(Merger_Owner_Name)]
# panel1[matched_pairs[,.(jw_score, owner_matched, merger_name)], on="owner_matched", `:=`(jw_score = i.jw_score, merger_name = i.merger_name)]
# rm(matched_pairs)
# getAvailMem()
# 
# # ID each merge event separately
# for (i in 1:nrow(mergers)){
#    yr <- mergers[i, Year]
#    targets_to_check <- unlist(mergers[i, Target_Entities_Clean])
#    target_name <- mergers[i, TargetName]
#    target_id <- mergers[i, MergeID_1]
#    acquirors_to_check <- unlist(mergers[i, Acquiror_Entities_Clean])
#    acquiror_name <- mergers[i, AcquirorName]
#    acquiror_id <- mergers[i, MergeID_2]
#    
#    panel1[year == yr & merger_name %in% targets_to_check & !is.na(TargetID), `:=`(TargetID = target_id)]   
#    panel1[year == yr & merger_name %in% acquirors_to_check & !is.na(AcquirorID), `:=`(AcquirorID = acquiror_id)]
#    
#    # Assign standardized merging firm names 
#    panel1[merger_name %in% targets_to_check & is.na(Merger_Owner_Name), Merger_Owner_Name := target_name] 
#    panel1[merger_name %in% acquirors_to_check & is.na(Merger_Owner_Name), Merger_Owner_Name := acquiror_name]
# }
# panel <- rbindlist(list(panel0, panel1))

panel[,merge_affected := any(!is.na(Merger_Owner_Name)), .(id)] # Props owned by any firm that merged 
panel[,merge_event := length(intersect(TargetID[!is.na(TargetID)], AcquirorID[!is.na(AcquirorID)])) > 0, .(year, Zip5)] # Merge event: zip-years where any TargetID == AcquirorID
panel[order(Zip5, year),merge_cum := cumsum(merge_event), .(Zip5)]
panel[,treated := merge_cum & merge_affected] # Treated variable in DiD: property of one of merged firms post-merger 

n_tot_owned <- uniqueN(panel[merge_affected == T, id])
print(paste("There are", n_tot_owned, "total unique properties owned by any of the merging firms in the dataset."))
zip_yr_with_merge <- unique(panel[merge_event == 1, .(Zip5, year)])
multi_merge_zips <- unique(zip_yr_with_merge[duplicated(Zip5), Zip5]) # Get zips affected by multiple mergers
zips_with_merge <- unique(zip_yr_with_merge$Zip5)
print(paste("Found", length(multi_merge_zips), "multi-merge zips out of", length(zips_with_merge), 
            "total zips with a merge event."))
# Zips with one of merged firms but not both 
affected_zips_no_merge <- unique(panel[, .(n_affected = sum(merge_affected), n_events = sum(merge_event)), .(Zip5)][n_affected > 0 & n_events == 0, Zip5])
print(paste("There are", length(affected_zips_no_merge), "zips with affected properties but no merge event."))

# Approximate sample sizes
n_props_merged_zips <- uniqueN(panel[Zip5 %in% zips_with_merge, id])
n_props_unmerged_zips <- uniqueN(panel[Zip5 %in% affected_zips_no_merge, id])

print(paste("There are", n_props_merged_zips, "unique properties in the merged zips."))
print(paste("There are", n_props_unmerged_zips, "unique properties in the zips with only 1 of any merging firm."))

# Save full panel
fwrite(panel, paste0("/gpfs/loomis/scratch60/humphries/rl874/mergers/", state, "_full.csv"))

# Save restricted panel 
# relevant_zips <- unique(panel[merge_affected == T, Zip5])
# panel <- panel[Zip5 %in% relevant_zips]
# fwrite(panel, paste0("/gpfs/loomis/scratch60/humphries/rl874/mergers/", state, "_mergers.csv"))
