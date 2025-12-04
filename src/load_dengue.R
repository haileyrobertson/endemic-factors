#' Load latest Open Dengue data
#'
#' @title load_dengue
#' @param data_dir Directory
#' @returns A data frame with the raw Open Dengue data
#' @export

load_dengue <- function(data_dir) {
  url <- "https://raw.githubusercontent.com/OpenDengue/master-repo/refs/heads/main/data/raw_data/masterDB_V1.3.csv"

  dest <- file.path(data_dir, "masterDB_V1.3.csv")

  # check if the file exists
  if (!file.exists(dest)) {
    download.file(url, destfile = dest, mode = "wb")
  }

  df_raw <- read.csv(dest, header = TRUE, sep = ",", stringsAsFactors = FALSE)

  return(df_raw)
}
