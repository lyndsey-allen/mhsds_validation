# ------------------------------------------------------------------------------
# MHSDS Validation – SPC Charts for Aggregation History
# Author: Lyndsey Allen
# Purpose: Create SPC time series charts with NHS Making Data Count rules
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(fs)
  library(readr)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
    if (!requireNamespace("NHSRplotthedots", quietly = TRUE)) {
    install.packages("NHSRplotthedots")
  }
  library(NHSRplotthedots)
  
})

# ------------------------------------------------------------------------------
# 1. Load latest aggregation_history file from data/history
# ------------------------------------------------------------------------------

history_dir <- "data/history"
out_root    <- "outputs/spc_aggregation"

all_hist <- fs::dir_ls(history_dir, type = "file")

agg_files <- all_hist[
  grepl("^[0-9]{6}_aggregation_history(?:_\\d{8}_\\d{6})?\\.csv$", basename(all_hist))
]

if (length(agg_files) == 0) {
  stop("No aggregation history files matched in data/history. Check filenames and rerun 02 script.")
}

# Choose the most recently modified file (newest run)
info <- fs::file_info(agg_files)
latest_file <- agg_files[which.max(info$modification_time)]

message("Using history file: ", latest_file)

# ------------------------------------------------------------------------------
# 2. Ensure SPC-ready types (date + numeric)
# ------------------------------------------------------------------------------

agg <- readr::read_csv(latest_file, show_col_types = FALSE) |>
  janitor::clean_names()

# Ensure proper date column exists
if (!"month_date" %in% names(agg)) {
  agg <- agg |>
    dplyr::mutate(month_date = as.Date(paste0(month, "01"), format = "%Y%m%d"))
}

# Ensure the value field is numeric
# (change 'numerator' to your actual metric column if different)
agg <- agg |>
  dplyr::mutate(
    month_date = as.Date(month_date),
    volume = suppressWarnings(as.numeric(numerator))
  ) |>
  dplyr::filter(!is.na(volume))

# ------------------------------------------------------------------------------
# 3. Ensure continuous time series per code (NHSRplotthedots requirement)
# ------------------------------------------------------------------------------

agg <- agg |>
  dplyr::group_by(code) |>
  tidyr::complete(month_date = seq(min(month_date), 
                                   max(month_date), by = "1 month")) |>
  dplyr::ungroup() |>
  dplyr::mutate(volume = tidyr::replace_na(volume, 0))

# ------------------------------------------------------------------------------
# 4. Output folder for SPC charts
# ------------------------------------------------------------------------------

latest_month <- max(format(agg$month_date, "%Y%m"))
out_dir <- path(out_root, latest_month)

if (!dir_exists(out_dir)) dir_create(out_dir, recurse = TRUE)

message("SPC charts will be written to: ", out_dir)

# ------------------------------------------------------------------------------
# 5. Restrict to the latest 24 months of data
# ------------------------------------------------------------------------------
cutoff_date <- max(agg$month_date, na.rm = TRUE) %m-% months(23)

agg <- agg |>
  filter(month_date >= cutoff_date)

# Create readable facet labels e.g. "MHS102: Community Treatment Orders"
agg <- agg |>
  mutate(
    facet_label = ifelse(
      !is.na(description) & description != "",
      paste0(code, ": ", description),
      code
    )
  )

# ------------------------------------------------------------------------------
# 6. Create SPC charts per code using NHSRplotthedots
# ------------------------------------------------------------------------------

codes <- unique(agg$code)

for (cd in codes) {
  
  df_cd <- agg |>
    filter(code == cd)
  
  # Extract description for title
  desc <- unique(df_cd$description)[1]
  if (is.na(desc) || desc == "") desc <- "No description available"
  
  # ptd_spc = compute SPC object (MDC XmR logic)
  spc_obj <- NHSRplotthedots::ptd_spc(
    .data = df_cd,
    value_field = volume,
    date_field  = month_date,
    improvement_direction = "neutral"    # volumes have no good/bad direction
  )
  
  # Create ggplot (MDC styling, colours, icons)
  p <- NHSRplotthedots::ptd_create_ggplot(
    spc_obj,
    main_title = paste0("SPC Chart – ", cd, ": ", desc),
    x_axis_label = "Month",
    y_axis_label = "Volume",
    icons_position = "top right"         # show variation icons
  )
  
  # Safe filename
  cd_safe <- str_replace_all(cd, "[^A-Za-z0-9_]", "_")
  
  # Save chart
  out_png <- path(out_dir, paste0("SPC_", cd_safe, ".png"))
  ggsave(out_png, p, width = 10, height = 6, dpi = 300)
  
  message("Created SPC chart: ", out_png)
}

# ------------------------------------------------------------------------------
# 7. MULTI-FACET SPC (one chart showing all codes)
# ------------------------------------------------------------------------------

spc_multi <- NHSRplotthedots::ptd_spc(
  .data = agg,
  value_field = volume,
  date_field  = month_date,
  facet_field = facet_label,             # <--- KEY LINE
  improvement_direction = "neutral"
)

p_multi <- NHSRplotthedots::ptd_create_ggplot(
  spc_multi,
  main_title   = "SPC Chart – All Codes (Faceted)",
  x_axis_label = "Month",
  y_axis_label = "Volume",
  icons_position = "top right"
)

out_multi <- fs::path(out_dir, "SPC_ALL_CODES_FACETED.png")
ggplot2::ggsave(out_multi, p_multi, width = 16, height = 12, dpi = 300)

message("Combined SPC faceted chart saved: ", out_multi)


# ------------------------------------------------------------------------------
# 8. ALL SPC PLOTS INTO A SINGLE PDF
# ------------------------------------------------------------------------------

pdf_path <- fs::path(out_dir, "SPC_ALL_CODES.pdf")
pdf(pdf_path, width = 11, height = 8.5)

for (cd in codes) {
  df_cd <- agg |>
    dplyr::filter(code == cd)
  
  spc_obj <- NHSRplotthedots::ptd_spc(
    .data = df_cd,
    value_field = volume,
    date_field  = month_date,
    improvement_direction = "neutral"
  )
  
  p <- NHSRplotthedots::ptd_create_ggplot(
    spc_obj,
    main_title = paste0("SPC Chart – ", cd, ": ", desc),
    x_axis_label = "Month",
    y_axis_label = "Volume",
    icons_position = "top right"
  )
  
  print(p)
}

dev.off()
message("PDF containing all SPC charts saved: ", pdf_path)


# ------------------------------------------------------------------------------
message("All SPC charts generated successfully.")