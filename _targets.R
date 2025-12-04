################################################################################
#
# Project build script
#
################################################################################

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here("src"), full.names = TRUE)) source(f)
tar_option_set(
  seed = 42
)
data_dir <- here("data")
output_dir <- here("outputs")

#-------------------------------------------------------------------
## 1. Data input
#-------------------------------------------------------------------

input_targets <- tar_plan(
  tar_target(
    raw_dengue,
    load_dengue(data_dir),
    format = "rds"
  ),
  tar_target(
    adm0,
    load_adm()$adm0,
    format = "rds"
  ),
  tar_target(
    adm1,
    load_adm()$adm1,
    format = "rds"
  )
)

#-------------------------------------------------------------------
## 2. Data processing
#-------------------------------------------------------------------
processing_targets <- tar_plan(
  tar_target(
    cleaned_dengue,
    clean_dengue(raw_dengue, adm0, adm1),
    format = "rds"
  )
)

#-------------------------------------------------------------------
## 3. Analysis
#-------------------------------------------------------------------
analysis_targets <- tar_plan()

#-------------------------------------------------------------------
## 4. Outputs
#-------------------------------------------------------------------

outputs_targets <- tar_plan(
  tar_target(
    dengue_coverage_plot,
    plot_dengue_coverage(cleaned_dengue, adm0, adm1, output_dir),
    format = "file"
  )
)

#-------------------------------------------------------------------
list(
  input_targets,
  processing_targets,
  analysis_targets,
  outputs_targets
)
