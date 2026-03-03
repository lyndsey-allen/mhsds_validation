# ------------------------------------------------------------------------------
# MHSDS Validation - Load Data
# Author: Lyndsey Allen
# Purpose: Standardised data load for rule engine
# ------------------------------------------------------------------------------

# ---- 1. Load libraries ----
suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(janitor)
  library(dplyr)
  library(fs)
  library(yaml)
  library(stringr)
  library(tibble)
})

# ---- 2. Load config ----
config <- yaml::read_yaml("config.yml")
extract_dir <- config$extract_path   # folder where monthly ZIPs are stored

# ---- 3. Helper: Identify monthly ZIP files ----
get_monthly_zips <- function() {
  files <- fs::dir_ls(extract_dir,
                      regexp = "[0-9]{6} Summary Reports\\.zip$")
  
  tibble(
    file = files,
    month = str_extract(files, "[0-9]{6}")
  )
}

# ---- 4. Generic file loader function ----
load_data <- function(path) {
  
  ext <- tools::file_ext(path)
  
  if (ext %in% c("csv", "txt")) {
    message("Loading CSV/TXT: ", path)
    df <- readr::read_csv(path, show_col_types = FALSE)
    
  } else if (ext %in% c("xlsx", "xls")) {
    message("Loading Excel: ", path)
    df <- readxl::read_excel(path)
    
  } else {
    stop("Unsupported file type: ", ext)
  }
  
  # ---- 5. Clean column names ----
  df <- df |> janitor::clean_names()
  
  # ---- 6. Remove empty rows/cols ----
  df <- df |> janitor::remove_empty(which = c("rows", "cols"))
  
  # ---- 7. Trim whitespace across character columns ----
  df <- df |> mutate(across(where(is.character), trimws))
  
  return(df)
}

# ---- 8. Select input file ----
# User sets this manually OR you can automate selection
input_file <- config$input_file  # define in config.yml

# ---- 9. Run loader ----
data <- load_data(input_file)

# ---- 10. Print summary ----
message("Data loaded successfully.")
message("Rows: ", nrow(data), " | Columns: ", ncol(data))
print(glimpse(data))