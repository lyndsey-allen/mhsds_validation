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
extract_root   <- "data/extracts"    # where monthly ZIPs arrive
processed_root <- "data/processed"   # where extracted + prefixed CSVs go
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
  quit(save = "no")
}

message("Found ", length(new_zips), " new ZIP(s).")

# ---- Process each new zip ----
processed_now <- list()

for (zp in new_zips) {
  yyyymm <- get_yyyymm(zp)
  message("Processing: ", path_file(zp), "  [", yyyymm, "]")
  
  # Create a temp folder to unzip
  tmp_dir <- tempfile(pattern = paste0("mhsds_", yyyymm, "_"))
  dir_create(tmp_dir)
  
  
  # Unzip
  tryCatch({
    zip::unzip(zp, exdir = tmp_dir)
  }, error = function(e) {
    message("ERROR during unzip: ", e$message)
    next
  })
  
  Sys.sleep(0.5)  # DELAY allows Windows Defender/OneDrive to release file handles
  
# ONLY LOOKS AT TOP LEVEL UNZIPPED CSV
  extracted_files <- dir_ls(tmp_dir, recurse = FALSE, type = "file")
  
  Keep <- extracted_files[
    str_detect(tolower(path_file(extracted_files)),
               "aggregation|data_quality|diagnostics|validation") &
      str_detect(tolower(extracted_files), "\\.csv$")
  ]
  
  if (length(keep) == 0) {
    message("  - No matching CSVs.")
  } else {
    for (src in keep) {
      out <- path(processed_root, paste0(yyyymm, "_", path_file(src)))
      file_copy(src, out, overwrite = TRUE)
      message("  + Saved: ", out)
    }
  }
  
  # Cleanup (wrap in try)
  tryCatch({
    Sys.sleep(0.2)
    dir_delete(tmp_dir)
  }, error = function(e) {
    message("Temp folder couldn't be deleted; OS may still be locking it.")
  })
  
  processed_now[[length(processed_now) + 1]] <-
    tibble(zip = zp, yyyymm = yyyymm)
}

# ---- Update state ----
new_state <- bind_rows(st, bind_rows(processed_now)) |>
  distinct(zip, .keep_all = TRUE)

write_state(new_state)

message("Done. Extracted CSVs saved to: ", processed_root)
