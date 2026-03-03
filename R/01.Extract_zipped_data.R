# ------------------------------------------------------------------------------
# MHSDS Validation - Load Data
# Author: Lyndsey Allen
# Purpose: Standardised data load for rule engine
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(fs)
  library(stringr)
  library(readr)   # only for writing the state file robustly
})

# ---- Config (adjust paths if needed) ----
extract_root   <- "data/extracts"    # where monthly ZIPs arrive
processed_root <- "data/processed"   # where extracted + prefixed CSVs go
state_file     <- path(processed_root, ".processed_zips.csv")

# Ensure output folder exists
if (!dir_exists(processed_root)) dir_create(processed_root, recurse = TRUE)

# ---- Helper: read/write state of processed ZIPs ----
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

# ---- Helper: derive YYYYMM from filename or parent folder ----
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

# ---- Find all zip files in extract_root (non-recursive or recursive as needed) ----
# If your ZIPs are directly under data/extracts, set recurse = FALSE
zips <- dir_ls(extract_root, recurse = TRUE, type = "file", glob = "*.zip")

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
  tmp_dir <- path_temp(paste0("mhsds_mvp_", yyyymm, "_", as.integer(runif(1, 1e6, 9e6))))
  dir_create(tmp_dir)
  
  # Unzip
  utils::unzip(zp, exdir = tmp_dir)
  
  # List all extracted files and keep only CSVs with desired patterns
  extracted_files <- dir_ls(tmp_dir, recurse = TRUE, type = "file")
  
  # Match patterns: validation / data_quality (case-insensitive)
  keep <- extracted_files[
#    str_detect(tolower(path_file(extracted_files)), "aggregation") |
#      str_detect(tolower(path_file(extracted_files)), "data_quality" |
#      str_detect(tolower(path_file(extracted_files)), "diagnostics" |
#      str_detect(tolower(path_file(extracted_files)), "validation")
  str_detect(tolower(path_file(extracted_files)), "aggregation") |
       str_detect(tolower(path_file(extracted_files)), "diagnostics")
  ]
  
  keep_csv <- keep[str_detect(tolower(keep), "\\.csv$")]
  
  if (length(keep_csv) == 0) {
    message("  - No matching CSVs (validation/data_quality) in this ZIP.")
    dir_delete(tmp_dir)
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
  dir_delete(tmp_dir)
  
  processed_now[[length(processed_now) + 1]] <- data.frame(zip = zp, yyyymm = yyyymm)
}

# ---- Update state file ----
processed_now_df <- if (length(processed_now)) dplyr::bind_rows(processed_now) else
  tibble::tibble(zip = character(), yyyymm = character(), processed_at = character())

new_state <- dplyr::bind_rows(st, processed_now_df) %>%
  dplyr::distinct(zip, .keep_all = TRUE) %>%
  dplyr::arrange(yyyymm, zip)

write_state(new_state)


message("Done. Extracted CSVs are in: ", processed_root)

