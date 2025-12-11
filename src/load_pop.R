#' Load UN WPP global population projections (ADM0 level)
#'
#' @title load_pop
#' @param data_dir File path
#' @returns pop_raw Raw population data frame
#' @export

load_pop <- function(data_dir) {
  url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz"

  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  gz_file <- file.path(data_dir, "WPP2024_Demographic_Indicators_Medium.csv.gz")
  csv_file <- file.path(data_dir, "WPP2024_Demographic_Indicators_Medium.csv")

  # download if not already saved
  if (!file.exists(csv_file)) {
    download.file(url, gz_file, mode = "wb")
    R.utils::gunzip(gz_file, destname = csv_file, remove = FALSE)
  }


  pop_raw <- readr::read_csv(csv_file, show_col_types = FALSE)
  return(pop_raw)
}
