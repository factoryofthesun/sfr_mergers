# ===================================================================
# Mergers Analysis: Zip Regressions
# ===================================================================
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

t0 <- Sys.time()
dt <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character")
# dt <- fread(paste0(data_path, "panel_zip_hhi.csv"), integer64 = "character", colClasses = list("character" = c("st_ct", "apn_unformatted")), nrows=1e6)
t1 <- Sys.time() 
print(paste("Reading took", difftime(t1,t0,units='mins'), "minutes."))

print("Available memory after reading and binding")
getAvailMem() 

# Read in mergers data 
mergers <- fread(paste0(mergers_path, "mergers_final_cleaned.csv"))
merge_labels <- c("Beazer/Ellington - American Homes 4 Rent", "Beazer/Ellington - American Homes 4 Rent", 
                  "Colony American - Starwood Waypoint", "American Residential Properties - American Homes 4 Rent",
                  "Starwood Waypoint - Invitation Homes", "Silver Bay - Tricon Capital")
colors <- c("coral1","coral1", "dodgerblue", "darkorange", "forestgreen", "plum")
mergers$label <- merge_labels
mergers$colors <- colors
# ============= (0) Preprocessing ===================================================
# Year between 2000 and 2020
dt <- dt[year >= 2000 & year <= 2020]

# Define new vars 
dt[,log_zori := log(ZORI)]
dt[!is.na(month) & !is.na(year),monthyear := parse_date_time(paste0(year, "-", month), "ym")]
treated_names <- grep("^treated_", names(dt), value=T)
dt[,treated_overlap := rowSums(.SD, na.rm = T), .SDcols = treated_names]

# Document multi-merge zips 
dt[treated_overlap > 1, multi_merge := 1] # Why is multi-merge not being recorded properly?
n_multi_zips <- uniqueN(dt[multi_merge == 1, Zip5])
print(paste0(n_multi_zips, " zips out of ", uniqueN(dt$Zip5), " with multi-merge."))
treated_zips <- unique(dt[treated_overlap >= 1, Zip5])
dt_multi_check <- dt[Zip5 %in% treated_zips,.(median_zori = as.numeric(median_na0(ZORI))), .(multi_merge, monthyear)]

ggplot(dt_multi_check, aes(x = monthyear, y = median_zori, color = factor(multi_merge))) + geom_line() + 
  labs(title = "Median Monthly ZORI for Treated Zips") + ggsave(paste0(mergers_path, "figs/diagnostics/multi_merge_zip_rent.png"))

# ============= (1) Zip Level Regressions ===================================================
# Basic pre-post, each merger 

# 2-way FE + delta HHI, all mergers, merger clustered SE
for (merge_id in unique(mergers$MergeID_1)){
  # Skip American Residential Ppty (no props found)
  if (merge_id == 3){
    next 
  }
  merge_label <- unique(mergers[MergeID_1 == merge_id, label])
  
  # Relevant column names based on merge id
  treated_var <- paste0("treated_", merge_id)
  post_var <- paste0("post_", merge_id)
  sample_var <- paste0("sample_", merge_id)
  
  dt_tmp <- dt[get(sample_var) == 1]
  dt_tmp[, treated := get(treated_var)]
  dt_tmp[, post := get(post_var)]
  avg_zori <- mean_na0(dt_tmp$ZORI)
  avg_log_zori <- mean_na0(dt_tmp$log_zori)
  avg_log_rent <- mean_na0(dt_tmp$median_log_rent)
  
  reg0 <- felm(ZORI ~ delta_hhi:post|factor(monthyear) + factor(Zip5), data = dt_tmp)
  reg1 <- felm(ZORI ~ delta_hhi:post + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
  reg2 <- felm(log_zori ~ delta_hhi:post|factor(monthyear) + factor(Zip5), data = dt_tmp)
  reg3 <- felm(log_zori ~ delta_hhi:post + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
  reg4 <- felm(median_log_rent ~ delta_hhi:post|factor(monthyear) + factor(Zip5), data = dt_tmp)
  reg5 <- felm(median_log_rent ~ delta_hhi:post + median_log_sqft + tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
  

  out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                        column.labels = c("Zori", "Log Zori", "Median Log Rent"), column.separate = c(2,2,2), 
                        title = paste0("2-Way FE Zip, Merger: ", merge_label)))

  # Wrap tabular environment in resizebox
  out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
  out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)
  
  # Set position
  out <- gsub("!htbp", "H", out, fixed = T)
  
  # Replace notes
  note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE. 
                       Sample average log ZORI: ", avg_log_zori, ". Sample average ZORI: ", avg_zori,
                       " Sample average log rent: ", avg_log_rent,
                       "ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
  note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
  out[grepl("Note",out)] <- note.latex
  
  cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=F)
}

# 2-way FE + delta HHI, all mergers ---------
#TODO: multiple additive delta HHI????
#TODO: What should sample be here? Keeping single-firm zips for now
dt_tmp <- dt[!is.na(monthyear)]
post_names <- grep("^post_", names(dt), value = T)
dt_tmp[,post := 0]
dt_tmp[,post := as.integer(rowSums(.SD) == 1), .SDcols = post_names]

label_names <- grep("^merge_label_", names(dt), value = T)
dt_tmp[,merge_label := as.character(NA)]
for (label in label_names){
  dt_tmp[is.na(merge_label)|merge_label == "", merge_label := get(label)]
}

hhi_names <- grep("^delta_hhi_", names(dt), value = T)
dt_tmp[,delta_hhi := 0]
# Delta HHI for multi-merged zips will be discontinuous jumps (add new delta HHI after every additional post period)
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  hhi_var <- hhi_names[i]
  dt_tmp[get(post_var) == 1, delta_hhi := delta_hhi + get(hhi_var)]
}

avg_zori <- mean_na0(dt_tmp$ZORI)
avg_log_zori <- mean_na0(dt_tmp$log_zori)
avg_log_rent <- mean_na0(dt_tmp$median_log_rent)

reg0 <- felm(ZORI ~ delta_hhi:post|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg1 <- felm(ZORI ~ delta_hhi:post + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg2 <- felm(log_zori ~ delta_hhi:post|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg3 <- felm(log_zori ~ delta_hhi:post + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg4 <- felm(median_log_rent ~ delta_hhi:post|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg5 <- felm(median_log_rent ~ delta_hhi:post + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)


out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                column.labels = c("Zori", "Log ZORI", "Median Log Rent"), column.separate = c(2,2,2), 
                                title = paste0("2-Way FE Zip, All Mergers, No Multi-Merge Zips")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

# Replace notes
note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE. 
                       Sample average ZORI: ", avg_zori, ". Sample average log ZORI: ", avg_log_zori,
                     " Sample average median log rent: ", avg_log_rent, "ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
out[grepl("Note",out)] <- note.latex

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=F)

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)
cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)

# 2-way FE + delta HHI : event-time dummy (-36 to 36), all mergers/drop multi-merged zips ----
#TODO: Any way to keep multi-merge zips here?
dt_tmp <- dt_tmp[treated_overlap <= 1]
setorder(dt_tmp, monthyear)

# Event-time variable for each merger
dt_tmp[, event_month := -999]
for (i in 1:length(post_names)){
  post_var <- post_names[i]
  treated_var <- treated_names[i]
  post_date <- last(dt_tmp[get(post_var) == 0, monthyear])
  dt_tmp[get(treated_var) == 1, event_month := 12 * (as.yearmon(monthyear) - as.yearmon(post_date))]
}

reg0 <- felm(ZORI ~ delta_hhi:event_month|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg1 <- felm(ZORI ~ delta_hhi:event_month + median_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg2 <- felm(log_zori ~ delta_hhi:event_month|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg3 <- felm(log_zori ~ delta_hhi:event_month + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg4 <- felm(median_log_rent ~ delta_hhi:event_month|factor(monthyear) + factor(Zip5), data = dt_tmp)
reg5 <- felm(median_log_rent ~ delta_hhi:event_month + median_log_sqft + median_tot_unit_val|factor(monthyear) + factor(Zip5), data = dt_tmp)


out <- capture.output(stargazer(reg0,reg1, reg2, reg3, reg4,reg5,
                                column.labels = c("Zori", "Log ZORI", "Median Log Rent"), column.separate = c(2,2,2), 
                                title = paste0("2-Way FE Zip with Event-Month, All Mergers, No Multi-Merge Zips")))

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)

# Replace notes
note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{\\textwidth}{ \\textit{Notes:} Sample is zip level, with month-year and zip FE. 
                       Sample average ZORI: ", avg_zori, ". Sample average log ZORI: ", avg_log_zori,
                     " Sample average median log rent: ", avg_log_rent, "ZORI sample covers years 2014-2020. MLS rent sample covers years 2007-2018.}} \\\\")
note.latex <- gsub("[\t\r\v]|\\s\\s+", " ", note.latex)
out[grepl("Note",out)] <- note.latex

cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=F)

# Wrap tabular environment in resizebox
out <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", out, fixed = T)
out <- gsub("\\end{tabular}", "\\end{tabular}}", out, fixed = T)

# Set position
out <- gsub("!htbp", "H", out, fixed = T)
cat(paste(out, "\n\n"), file = paste0(estimate_path, "zip_did.tex"), append=T)
