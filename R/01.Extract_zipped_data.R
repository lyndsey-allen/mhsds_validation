# ------------------------------------------------------------------------------
# MHSDS Validation - Extract zipped data
# Author: Lyndsey Allen
# Purpose: Standardised data load for rule engine
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(stringr)
  library(readr)
  library(zip)
})

# ---- Config (adjust paths if needed) ----
extract_root   <- "A:/MHSDS/submission/DQMI Reports"  
# where monthly ZIPs arrive
processed_root <- "A:/MHSDS/4. Data Quality Improvement/mhsds_validation/data/processed"   
# where extracted + prefixed CSVs go
state_file     <- path(processed_root, ".processed_zips.csv")

# Ensure output folder exists
if (!dir_exists(processed_root)) dir_create(processed_root, recurse = TRUE)

# ---- Read/write state of processed ZIPs ----
read_state <- function() {
  if (file_exists(state_file)) {
    tryCatch(
      readr::read_csv(state_file, show_col_types = FALSE),
      error = function(e) data.frame(zip = character(), yyyymm = character(), stringsAsFactors = FALSE)
    )
  } else {
    data.frame(zip = character(), yyyymm = character(), stringsAsFactors = FALSE)
  }
}

write_state <- function(df) {
  readr::write_csv(df, state_file)
}

# ---- Derive YYYYMM from filename or parent folder ----
get_yyyymm <- function(zip_path) {
  # 1) try from file name e.g., "202501 Summary Reports.zip" or "..._202501_..."
  y <- str_extract(path_file(zip_path), "(?<!\\d)(20\\d{2}(0[1-9]|1[0-2]))(?!\\d)")
  if (!is.na(y) && nchar(y) == 6) return(y)
  
  # 2) fallback to parent folder name containing a yyyymm
  parent <- path_file(path_dir(zip_path))
  y <- str_extract(parent, "(?<!\\d)(20\\d{2}(0[1-9]|1[0-2]))(?!\\d)")
  if (!is.na(y) && nchar(y) == 6) return(y)
  
  # 3) if all else fails, use current year-month (MVP fallback)
  format(Sys.Date(), "%Y%m")
}

# ---- Find all zip files in extract_root ----
zips <- dir_ls(extract_root, recurse = FALSE, type = "file", glob = "*.zip")

# Load previously processed zips
st <- read_state()


# Filter to only new zips
new_zips <- setdiff(zips, st$zip)

if (length(new_zips) == 0) {
  message("No new ZIP files found in: ", extract_root)
  # Do NOT quit the session — just stop this script cleanly:
  if (interactive()) {
    message("Nothing to process; exiting script without closing R.")
  }
  # Simply end the script here:
  } else {
  message("Found ", length(new_zips), " new ZIP(s).")
  # proceed with your loop...
}


# ---- Process each new zip ----
processed_now <- list()

for (zp in new_zips) {
  yyyymm <- get_yyyymm(zp)
  message("Processing: ", path_file(zp), "  [", yyyymm, "]")
  
  # Create a temp folder to unzip
  tmp_dir <- path_temp(paste0("mhsds_", yyyymm, "_", as.integer(runif(1, 1e6, 9e6))))
  dir_create(tmp_dir)
  
  # --- Always init these so later code can safely refer to them
  extracted_files <- character(0)
  keep <- character(0)
  
  # Unzip (guarded)
  unzip_ok <- TRUE
  tryCatch(
    {
      utils::unzip(zp, exdir = tmp_dir)
    },
    error = function(e) {
      unzip_ok <<- FALSE
      message("  ! unzip failed for ", zp, " : ", e$message)
    }
  )
  if (!unzip_ok) {
    # record in state and move to next zip
    processed_now[[length(processed_now) + 1]] <- data.frame(zip = zp, yyyymm = yyyymm)
    # best-effort cleanup
    if (dir_exists(tmp_dir)) { 
      try(dir_delete(tmp_dir), silent = TRUE) 
    }
    next
  }
  
  # List extracted files (guarded)
  list_ok <- TRUE
  tryCatch(
    {
      extracted_files <- dir_ls(tmp_dir, recurse = TRUE, type = "file")
    },
    error = function(e) {
      list_ok <<- FALSE
      message("  ! listing extracted files failed: ", e$message)
    }
  )
  if (!list_ok || length(extracted_files) == 0) {
    message("  - No files found after unzip.")
    processed_now[[length(processed_now) + 1]] <- data.frame(zip = zp, yyyymm = yyyymm)
    try(dir_delete(tmp_dir), silent = TRUE)
    next
  }
  
  # Match file type (ALWAYS define 'keep' before using it later)
  keep <- extracted_files[
    str_detect(tolower(path_file(extracted_files)), "aggregation") |
      str_detect(tolower(path_file(extracted_files)), "data_quality") |
      str_detect(tolower(path_file(extracted_files)), "diagnostics") |
      str_detect(tolower(path_file(extracted_files)), "validation")
  ]
  
  keep_csv <- keep[str_detect(tolower(keep), "\\.csv$")]
  
  if (length(keep_csv) == 0) {
    message("  - No matching CSVs in this ZIP.")
    try(dir_delete(tmp_dir), silent = TRUE)
    processed_now[[length(processed_now) + 1]] <- data.frame(zip = zp, yyyymm = yyyymm)
    next
  }
  
  # Copy into processed_root with YYYYMM_ prefix
  for (src in keep_csv) {
    base <- path_file(src)
    out  <- path(processed_root, paste0(yyyymm, "_", base))
    file_copy(src, out, overwrite = TRUE)
    message("  + ", path_file(out))
  }
  
  # Clean temp
  try(dir_delete(tmp_dir), silent = TRUE)
  
  processed_now[[length(processed_now) + 1]] <- data.frame(zip = zp, yyyymm = yyyymm)
}


# ---- Normalise yyyymm to character everywhere ----
st$yyyymm <- as.character(st$yyyymm)

processed_now_df <- if (length(processed_now)) {
  processed_now_df <- dplyr::bind_rows(processed_now)
  processed_now_df$yyyymm <- as.character(processed_now_df$yyyymm)
  processed_now_df
} else {
  tibble::tibble(zip = character(), yyyymm = character())
}


# ---- Update state ----
new_state <- bind_rows(st, bind_rows(processed_now)) |>
  distinct(zip, .keep_all = TRUE)

write_state(new_state)

message("Done. Extracted CSVs saved to: ", processed_root)

