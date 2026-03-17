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
    gaul_recode,
    readr::read_csv("data/fao_gaul/gaul_recode.csv"),
    format = "rds"
  ),
  # TODO: set a CRS (currently NA!)
  tar_target(
    shp,
    sf::st_read("data/fao_gaul/g2015_2014_2/g2015_2014_2.shp"),
    format = "rds"
  ),
  tar_target(
    raw_pop,
    load_pop(data_dir),
    format = "rds"
  )
)

#-------------------------------------------------------------------
## 2. Data processing
#-------------------------------------------------------------------
processing_targets <- tar_plan(
  tar_target(
    df_fix,
    match_places(raw_dengue, gaul_recode, shp),
    format = "rds"
  ),
  tar_target(
    locations,
    data.frame(
      iso_a0 = c("VNM", "COL", "MEX", "BRA"),
      location = c("ha noi city", "cali", "distrito federal", "rio de janeiro"),
      level = c("adm1", "adm2", "adm1", "adm2")
    )
  ),
  tar_target(
    subset,
    extract_ts(df_fix, locations),
    format = "rds"
  ),
  tar_target(
    breakdown,
    subset |>
      dplyr::count(iso_a0, year, gaul_level, t_res) |>
      tidyr::pivot_wider(
        names_from = c(gaul_level, t_res),
        values_from = n,
        values_fill = 0
      ),
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
  # tar_target(
  #   dengue_coverage_plot,
  #   plot_dengue_coverage(cleaned_dengue, adm0, adm1, output_dir),
  #   format = "file"
  # )
)

#-------------------------------------------------------------------
list(
  input_targets,
  processing_targets,
  analysis_targets,
  outputs_targets
)
