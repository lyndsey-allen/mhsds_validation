# ------------------------------------------------------------------------------
# MHSDS Validation – Evaluate Validation & DQ Issues
# Author: Lyndsey Allen
# Purpose: Ingest the Validation & DQ summary files and evaluate
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  if (!requireNamespace("NHSRplotthedots", quietly = TRUE)) {
    install.packages("NHSRplotthedots")
  }
  library(NHSRplotthedots)
  
})


# Define the earliest month you can refresh
fy_start <- as.Date("2025-04-01")

# Restrict to current financial year only
all_errors_fy <- all_errors %>%
  filter(month_date >= fy_start)

# ------------------------------------------------------------------------------
# 1. Load latest data_quality_history & validation_history file from data/history
# ------------------------------------------------------------------------------

history_dir <- "data/history"

out_root    <- "outputs/dq_validation"

all_hist <- fs::dir_ls(history_dir, type = "file")

dq_files <- all_hist[
  grepl("^[0-9]{6}_data_quality_history(?:_\\d{8}_\\d{6})?\\.csv$", basename(all_hist))
]

if (length(dq_files) == 0) {
  stop("No dq history files matched in data/history. Check filenames and rerun 02 script.")
}

val_files <- all_hist[
  grepl("^[0-9]{6}_validation_history(?:_\\d{8}_\\d{6})?\\.csv$", basename(all_hist))
]

if (length(val_files) == 0) {
  stop("No validation history files matched in data/history. Check filenames and rerun 02 script.")
}

# Choose the most recently modified file (newest run)
info <- fs::file_info(dq_files)
latest_dq_file <- dq_files[which.max(info$modification_time)]

info <- fs::file_info(val_files)
latest_val_file <- val_files[which.max(info$modification_time)]

message("Using dq history file: ", latest_dq_file)
message("Using val history file: ", latest_val_file)


dq <- read_csv(latest_dq_file)
val <- read_csv(latest_val_file)

# ------------------------------------------------------------------------------
# 2. Standardise Column Names and Combine Files 
# ------------------------------------------------------------------------------

all_errors <- bind_rows(
  dq %>% mutate(source = "data_quality"),
  val %>% mutate(source = "validation")
)

# ------------------------------------------------------------------------------
# 3. Create Useful Variables
# ------------------------------------------------------------------------------

all_errors <- all_errors %>%
  mutate(
    category = case_when(
      str_detect(description, regex("rejected", ignore_case = TRUE)) ~ "Rejection",
      str_detect(description, regex("warning", ignore_case = TRUE)) ~ "Warning",
      TRUE ~ "Other"
    ),
    month_date = ymd(month_date)
  )
# ------------------------------------------------------------------------------
# 4. Output folder
# ------------------------------------------------------------------------------

latest_month <- stringr::str_extract(basename(latest_file), "^[0-9]{6}")
out_dir <- fs::path(out_root, latest_month)

if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir, recurse = TRUE)

message("DQ Validation reports will be written to: ", out_dir)

# ------------------------------------------------------------------------------
# 5. Summary Table 
# ------------------------------------------------------------------------------

error_summary <- all_errors %>%
  group_by(code, description, category) %>%
  summarise(
    count = n(),
    first_month = min(month_date),
    last_month = max(month_date),
    .groups = "drop"
  )

latest_month_safe <- stringr::str_replace_all(latest_month, "[^A-Za-z0-9_]", "_")

out_error_summary <- fs::path(
  out_dir,
  paste0(latest_month_safe, "_error_summary.csv")
)

readr::write_csv(error_summary, out_error_summary)

message("Created error summary CSV output: ", out_error_summary)


# ------------------------------------------------------------------------------
# 6. Calculate monthly trends
# ------------------------------------------------------------------------------

error_monthly <- all_errors %>%
  group_by(month_date, code, category) %>%
  summarise(count = n(), .groups = "drop")

print(error_monthly)   

latest_month_safe <- stringr::str_replace_all(latest_month, "[^A-Za-z0-9_]", "_")

out_error_monthly <- fs::path(
  out_dir,
  paste0(latest_month_safe, "_error_monthly.csv")
)

readr::write_csv(error_monthly, out_error_monthly)

message("Created error_monthly CSV output: ", out_error_monthly)

# ------------------------------------------------------------------------------
# 6. Compile list of issues
# ------------------------------------------------------------------------------

compiled_issues <- all_errors_fy %>%
  pivot_longer(
    cols = starts_with("data_item"),
    names_to = "item_name",
    values_to = "item_value"
  ) %>%
  filter(!is.na(item_value)) %>%
  unite("item", item_name, item_value, sep = ": ") %>%
  group_by(across(-item)) %>%
  summarise(extra_info = paste(item, collapse = "; "), .groups = "drop")

latest_month_safe <- stringr::str_replace_all(latest_month, "[^A-Za-z0-9_]", "_")

out_compiled_issues <- fs::path(
  out_dir,
  paste0(latest_month_safe, "_compiled_issues.csv")
)

readr::write_csv(compiled_issues, out_compiled_issues)

message("Created compiled_issues CSV output (FY only): ", out_compiled_issues)

# ------------------------------------------------------------------------------
# 7. Connect Validation Codes to the Master Validation Rules
# ------------------------------------------------------------------------------

ref_dir <- "data/reference"

all_ref <- fs::dir_ls(ref_dir, type = "file")

ref_files <- all_ref[
  grepl("^MHSDS_validation_rules_v6\\..*\\.csv$", basename(all_ref))
]

if (length(ref_files) == 0) {
  stop("No v6.x validation rule CSV found in data/reference")
}

latest_ref_file <- ref_files[which.max(fs::file_info(ref_files)$modification_time)]

message("Using validation rules file: ", latest_ref_file)

validation_library <- read_csv(latest_ref_file)

# ------------------------------------------------------------------------------
# 8. Clean up and standardise Validation rules library
# ------------------------------------------------------------------------------

validation_library <- validation_library %>%
  janitor::clean_names() %>%
  rename(
    validation_type = validation_type,
    table = table,
    field_uid = uid_table_id,
    field_name = data_item_name_data_dict_element,
    xml_element = xml_element_name,
    validation_level = validation_level_file_group_record,
    generic_type = generic_validation_type_m_r_f_n,
    error_level = error_level,
    error_code = error_code,
    error_text = validation_text_with_action,
    guidance = additional_guidance_not_for_inclusion_in_error_message
  ) %>%
  mutate(error_code = as.character(error_code))

#check combined history has the same join field
all_errors <- all_errors %>%
  mutate(code = as.character(code))

# ------------------------------------------------------------------------------
# 9. Join validation rules to combined historic
# ------------------------------------------------------------------------------

errors_joined <- all_errors_fy %>%
  left_join(validation_library, by = c("code" = "error_code"))
            
         
#convert all-errors into a string for readability

all_errors_fy <- all_errors_fy %>%
  mutate(error_id = row_number())

extra_info <- all_errors_fy %>%
  pivot_longer(
    cols = starts_with("data_item"),
    names_to = "item_name",
    values_to = "info_value"
  ) %>%
  filter(!is.na(info_value)) %>%
  group_by(error_id) %>%
  summarise(
    additional_info = paste(info_value, collapse = "; "),
    .groups = "drop"
  )

errors_joined <- errors_joined %>%
  left_join(extra_info, by = "error_id")

print(errors_joined)

# ------------------------------------------------------------------------------
# 10. Create output tables
# ------------------------------------------------------------------------------

error_summary <- errors_joined %>%
  group_by(code, error_text, error_level) %>%
  summarise(
    count = n(),
    first_month = min(month_date),
    last_month = max(month_date),
    .groups = "drop"
  )
print(error_summary)

full_val_errors <- errors_joined %>%
  group_by(table, code, error_text, guidance) %>%
  summarise(count = n(),.groups = "drop") %>%
  arrange(desc(count))

print(full_val_errors)

out_full_val_errors <- fs::path(
  out_dir,
  paste0(latest_month_safe, "_full_val_errors.csv")
)

readr::write_csv(full_val_errors, out_full_val_errors)

message("Created full_val_errors CSV output (FY only): ", out_full_val_errors)
# ------------------------------------------------------------------------------
# 11. Pareto Chart of Most Impactful MHSDS Errors
# ------------------------------------------------------------------------------


pareto_data <- errors_joined %>%
  count(code, error_text, sort = TRUE) %>%
  mutate(
    cum_pct = cumsum(n) / sum(n) * 100,
    pct = n / sum(n) * 100
  )

ggplot(pareto_data, aes(x = reorder(code, -n), y = n)) +
  geom_col(fill = "#2C7BB6") +
  geom_line(
    aes(y = cum_pct * max(n) / 100, group = 1),
    color = "red",
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = cum_pct * max(n) / 100),
    color = "red",
    size = 2
  ) +
  scale_y_continuous(
    name = "Error Count",
    sec.axis = sec_axis(~ . / max(pareto_data$n) * 100,
                        name = "Cumulative %")
  ) +
  labs(
    title = "Pareto Chart of MHSDS Submission Errors",
    x = "Error Code",
    caption = "Bars = Error count, Red line = cumulative percentage"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

# ------------------------------------------------------------------------------
# 12. Top 5 Errors to prioritise
# ------------------------------------------------------------------------------


severity_weights <- c(
  "Record rejected" = 1,
  "Severity 1 Warning" = 1,
  "Warning" = 1
)

top10_priority <- errors_joined %>%
  mutate(weight = severity_weights[error_level]) %>%
  count(
    code, error_text, error_level, wt = weight, 
    name = "priority_score", sort = TRUE) %>%
  slice_head(n = 10)

top10_priority



            