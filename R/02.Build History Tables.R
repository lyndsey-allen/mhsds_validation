# ------------------------------------------------------------------------------
# MHSDS Validation - Build History Tables
# Author: Lyndsey Allen
# Purpose: Append monthly CSVs into cumulative tables
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(fs)
  library(readr)
  library(dplyr)
  library(stringr)
  library(janitor)
})

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

processed_dir <- "data/processed"
history_dir   <- "data/history"
if (!dir_exists(history_dir)) dir_create(history_dir, recurse = TRUE)

message("Working directory: ", getwd())
message("Searching processed files in: ", processed_dir)
message("Saving history outputs to: ", history_dir)

# ------------------------------------------------------------------------------
# 1. LIST AND MATCH MONTHLY FILES (MATCH ON BASENAME)
# ------------------------------------------------------------------------------

all_files <- fs::dir_ls(processed_dir, type = "file")

if (length(all_files) == 0) {
  stop("No files found in data/processed/. Extractor may not have run yet.")
}

# Match on *filename only* so full path does not disrupt regex
fn <- basename(all_files)

aggregation_files <- all_files[str_detect(fn, "^[0-9]{6}_.*aggregation.*\\.csv$")]
dq_files         <- all_files[str_detect(fn, "^[0-9]{6}_.*data_quality.*\\.csv$")]
diagnostics_files <- all_files[str_detect(fn, "^[0-9]{6}_.*diagnostics.*\\.csv$")]
validation_files <- all_files[str_detect(fn, "^[0-9]{6}_.*validation.*\\.csv$")]

message("Found ", length(aggregation_files), " aggregation files.")
message("Found ", length(dq_files), " data quality files.")
message("Found ", length(diagnostics_files), " diagnostics files. ")
message("Found ", length(validation_files), " validation files.")

if (length(validation_files) == 0 && length(dq_files) == 0) {
  stop("No matching monthly CSVs found.")
}

# ------------------------------------------------------------------------------
# 2. LOAD MONTHLY FILE AS CHARACTER-ONLY (FIX TYPE MISMATCH ISSUES)
# ------------------------------------------------------------------------------

load_month <- function(path) {
  yyyymm <- str_extract(basename(path), "^[0-9]{6}")
  
  df <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = "c")   # FORCE ALL COLUMNS AS CHARACTER
  ) |>
    janitor::clean_names() |>
    mutate(month = yyyymm)
  
  return(df)
}

# ------------------------------------------------------------------------------
# 3. BUILD AGGREGATION HISTORY
# ------------------------------------------------------------------------------

if (length(aggregation_files) > 0) {
  aggregation_list <- lapply(aggregation_files, load_month)
  
  aggregation_history <- bind_rows(aggregation_list)
  
  latest_month <- max(aggregation_history$month)
  
  out_a <- path(history_dir, 
                paste0(latest_month, "_aggregation_history_", timestamp, ".csv"))
  Sys.sleep(0.2)
  write_csv(aggregation_history, out_a)
  
  message("Aggregation history saved: ", out_a)
} else {
  message("No aggregation files found — skipping aggregation history.")
}

# ------------------------------------------------------------------------------
# 4. BUILD DATA QUALITY HISTORY
# ------------------------------------------------------------------------------

if (length(dq_files) > 0) {
  dq_list <- lapply(dq_files, load_month)
  
  dq_history <- bind_rows(dq_list)
  
  latest_month_dq <- max(dq_history$month)
  
  out_q <- path(history_dir, paste0(latest_month_dq,
                                    "_data_quality_history_", timestamp, ".csv"))
  Sys.sleep(0.2)
  write_csv(dq_history, out_q)
  
  message("Data quality history saved: ", out_q)
} else {
  message("No data quality files found — skipping data quality history.")
}

# ------------------------------------------------------------------------------
# 5. BUILD DIAGNOSTICS HISTORY
# ------------------------------------------------------------------------------

if (length(diagnostics_files) > 0) {
  diagnostics_list <- lapply(diagnostics_files, load_month)
  
  diagnostics_history <- bind_rows(diagnostics_list)
  
  latest_month_diagnostics <- max(diagnostics_history$month)
  
  out_d <- path(history_dir, paste0(latest_month_diagnostics,
                                    "_diagnostics_history_", timestamp, ".csv"))
  Sys.sleep(0.2)
  write_csv(diagnostics_history, out_d)
  
  message("Diagnostics history saved: ", out_d)
} else {
  message("No diagnostics files found — skipping diagnostics history.")
}

# ------------------------------------------------------------------------------
# 6. BUILD VALIDATION HISTORY
# ------------------------------------------------------------------------------

if (length(validation_files) > 0) {
  validation_list <- lapply(validation_files, load_month)
  
  validation_history <- bind_rows(validation_list)
  
  latest_month_validation <- max(validation_history$month)
  
  out_v <- path(history_dir, paste0(latest_month_validation,
                                    "_validation_history_", timestamp, ".csv"))
  Sys.sleep(0.2)
  write_csv(validation_history, out_v)
  
  message("Validation history saved: ", out_v)
} else {
  message("No validation files found — skipping validation history.")
}

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------

message("History tables built successfully.")
