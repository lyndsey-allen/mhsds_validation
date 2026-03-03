
# MHSDS Validation

This repository contains a Reproducible Analytical Pipeline (RAP) for validating Mental Health Services Data Set (MHSDS) extracts using R.  
The pipeline is designed to be automated, transparent, and fully version-controlled, supporting all Baseline RAP requirements.


## Purpose
The MHSDS Validation:

- Reads and validates MHSDS extract files (CSV format)
- Applies structured YAML-defined validation rules
- Produces record-level and summary-level validation outputs
- Ensures reproducibility through configuration, documentation, and automation
- Provides optional Shiny UI for non-technical users

# Project Structure
mhsdsValidation/
├── R/                     # All R scripts for processing
│   ├── load_data.R
│   ├── rule_engine.R
│   ├── run_validations.R
│   ├── helpers_dates.R
│   ├── helpers_codes.R
│   ├── output_export.R
│   └── logging.R
│
├── rules/                 # YAML rule files (one per table or domain)
│   ├── rules_header.yml
│   ├── rules_mpi.yml
│   ├── rules_gp.yml
│   └── ...
│
├── config/                # Global configuration
│   ├── reporting_period.yml
│   ├── file_paths.yml
│   └── global.yml
│
├── data/
│   ├── extracts/          # User-supplied MHSDS tables to validate
│   └── reference/         # ODS, national code lists, SNOMED, etc.
│
├── tests/                 # Automated unit tests (testthat)
│   ├── test_rules.R
│   ├── test_dates.R
│   ├── test_linkage.R
│   └── fixtures/
│
├── outputs/               # Validation outputs and logs
│   ├── validation_record_level.csv
│   ├── validation_summary.xlsx
│   └── logs/
│
├── docs/                  # Documentation files
│   ├── project_structure.md
│   ├── rule_system.md
│   ├── reference_data.md
│   ├── qa_process.md
│   ├── shiny_overview.md
│   └── pipeline_overview.md
│
├── README.md
└── mhsdsValidator.Rproj

# Getting started

## 1. Install dependencies

`r
install.packages(c(
  "data.table",
  "yaml",
  "lubridate",
  "openxlsx",
  "testthat"
))
`

## 2. Place input data

Extract CSVs → data/extracts/
Reference datasets → data/reference/

Full details: /docs/reference_data.md.

## 3. Run the validation pipeline
`r
source("R/run_validations.R")

results <- run_full_validation(
  extract_dir = "data/extracts/",
  rules_dir   = "rules/"
)

export_validation_results(results)
`

Outputs are written to outputs/:

validation_record_level.csv
validation_summary.xlsx
Log file in outputs/logs/

## Testing
Run all tests with:
`r
testthat::test_dir("tests/")
`

## Rule System Overview
Validation rules are stored as YAML, e.g.:

`r
- rule_id: MHS00001
  table: MHS000Header
  field: DatSetVer
  type: rejection
  description: "DatSetVer is blank."
  expression: "is.na(DatSetVer) | DatSetVer == ''"
  help: "Populate with the correct dataset version, e.g., '6.0'"
  impacts: []
  `
  
The pipeline dynamically parses these expressions and applies them to each dataset.
See /docs/rule_system.md for a full explanation.

## Configuration
Configuration files in config/ allow the pipeline to run without editing code:

Reporting period
File paths
Global settings

## Reproducibility & RAP Compliance
This project:

Uses open-source tooling (R)
Is designed to run entirely via scripts or Makefile
Uses Git for full version control + audit trail
Includes tests, documentation, and clear folder structure
Separates logic, rules, data, and outputs
Supports peer review and collaborative development

See /docs/qa_process.md for QA details.

## Contributing
Pull requests and discussions are welcome.
Please ensure all contributions:
- Pass tests
- Include documentation updates
- Follow the existing folder structure
- Are peer-reviewed before merging


## Licence
Choose an appropriate open-source licence (MIT recommended).

## Contact
Maintainer: [Add your name or team]
Email: [contact]
