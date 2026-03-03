# ------------------------------------------------------------------------------
# MHSDS Validation - Load Data
# ------------------------------------------------------------------------------

#install.packages(c(  "tidyverse",  "data.table",  "yaml",  "fs",  "readr",
#  "zip",  "janitor"))


library(fs)
library(yaml)


config <- list(
  extract_path = "X:/Lyndsey_Analytics & Reporting/3. Reporting Products/
  3. Statutory & National Returns/MHSDS/4. Data Quality Improvement/
  mhsds_validation/4.data/extracts",
  processed_path = "data/processed"
)


config <- yaml::read_yaml("config.yml")
extract_dir <- config$extract_path

get_monthly_zips <- function() {
  files <- dir_ls(extract_dir, regexp = "[0-9]{6} Summary Reports\\.zip$")
  tibble::tibble(
    file = files,
    month = stringr::str_extract(files, "[0-9]{6}")
  )
}

