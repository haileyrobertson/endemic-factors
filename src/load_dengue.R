#' Load latest Open Dengue data
#'
#' @title load_dengue
#' @param data_dir Directory where data will be stored
#' @returns Raw Open Dengue data frame
#' @export

load_dengue <- function(data_dir) {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  zip_url <- "https://github.com/OpenDengue/master-repo/raw/main/data/releases/V1.3/Spatial_extract_V1_3.zip"

  zip_path <- file.path(data_dir, "Spatial_extract_V1_3.zip")
  out_dir <- file.path(data_dir, "Spatial_extract_V1_3")

  if (!file.exists(zip_path)) {
    download.file(zip_url, destfile = zip_path, mode = "wb")
  }

  if (!dir.exists(out_dir)) {
    unzip(zip_path, exdir = out_dir)
  }

  csv_file <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)

  # read raw dengue data
  df_raw <- read.csv(csv_file, stringsAsFactors = FALSE)

  return(df_raw)
}
