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

# ------------------------------------------------------------------
# 2. Ensure SPC‑ready date + numeric values
# ------------------------------------------------------------------

agg <- readr::read_csv(latest_file, show_col_types = FALSE) |>
  clean_names()

agg <- agg %>%
  mutate(
    month_date = case_when(
      !is.na(month_date) ~ as.Date(month_date),
      !is.na(month) ~ as.Date(paste0(month, "01"), format = "%Y%m%d"),
      TRUE ~ NA_Date_
      ),
    volume = suppressWarnings(as.numeric(numerator))   # <<< IMPORTANT
    )

# ------------------------------------------------------------------------------
# 3. Restrict to latest 24 months
# ------------------------------------------------------------------------------

cutoff_date <- max(agg$month_date, na.rm = TRUE) %m-% months(23)
agg <- agg |> filter(month_date >= cutoff_date)

# ------------------------------------------------------------------------------
# 4. Create readable label and remove duplicates per month
# ------------------------------------------------------------------------------



# Use tbl_name if present, else fallback to code
agg <- agg |>
  mutate(
    label = if ("tbl_name" %in% names(agg))
      paste0(code, ": ", tbl_name) else code
  )

# Ensure ONE row per (code, month)
agg <- agg |>
  group_by(code, month_date) |>
  summarise(
    volume = max(volume, na.rm = TRUE),
    label  = first(label),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 5. Output folder
# ------------------------------------------------------------------------------

latest_month <- stringr::str_extract(basename(latest_file), "^[0-9]{6}")
out_dir <- fs::path(out_root, latest_month)

if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir, recurse = TRUE)

message("SPC charts will be written to: ", out_dir)

# ------------------------------------------------------------------------------
# 6. Create SPC charts per code
# ------------------------------------------------------------------------------

codes <- unique(agg$code)

for (cd in codes) {
  
  df_cd <- agg |> filter(code == cd)
  desc  <- unique(df_cd$label)[1]
  
  spc_obj <- ptd_spc(
    .data = df_cd,
    value_field = volume,
    date_field  = month_date,
    improvement_direction = "neutral"
  )
  
  p <- ptd_create_ggplot(
    spc_obj,
    main_title   = paste0("SPC Chart – ", desc),
    x_axis_label = "Month",
    y_axis_label = "Volume",
    icons_position = "top right"
  )
  
  cd_safe <- str_replace_all(cd, "[^A-Za-z0-9_]", "_")
  out_png <- fs::path(out_dir, paste0("SPC_", cd_safe, ".png"))
  ggsave(out_png, p, width = 10, height = 6, dpi = 300)
  
  message("Created SPC chart: ", out_png)
}


# ------------------------------------------------------------------------------
# 7. Multi-facet SPC (one combined chart)
# ------------------------------------------------------------------------------

spc_multi <- ptd_spc(
  .data = agg,
  value_field = volume,
  date_field  = month_date,
  facet_field = label,
  improvement_direction = "neutral"
)

p_multi <- ptd_create_ggplot(
  spc_multi,
  main_title   = "SPC Chart – All Codes (Faceted)",
  x_axis_label = "Month",
  y_axis_label = "Volume",
  icons_position = "top right"
)

out_multi <- fs::path(out_dir, "SPC_ALL_CODES_FACETED.png")
ggplot2::ggsave(out_multi, p_multi, width = 16, height = 12, dpi = 300)
message("Combined faceted SPC saved: ", out_multi)

# ------------------------------------------------------------------------------
# 8. All SPC charts into a PDF
# ------------------------------------------------------------------------------

pdf_path <- fs::path(out_dir, "SPC_ALL_CODES.pdf")
pdf(pdf_path, width = 11, height = 8.5)

for (cd in codes) {
  df_cd <- agg |> filter(code == cd)
  desc  <- unique(df_cd$label)[1]
  
  spc_obj <- ptd_spc(
    .data = df_cd,
    value_field = volume,
    date_field  = month_date,
    improvement_direction = "neutral"
  )
  
  p <- ptd_create_ggplot(
    spc_obj,
    main_title   = paste0("SPC Chart – ", desc),
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

