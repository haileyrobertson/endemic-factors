#' Clean Open Dengue data
#' @title clean_data

library(tidyr)
library(dplyr)
library(rnaturalearth)
library(countrycode)

url <- "https://raw.githubusercontent.com/OpenDengue/master-repo/refs/heads/main/data/raw_data/masterDB_V1.2.csv"

# Convert country names to ISO3 codes
df <- read.csv(url)

# Manually fix country names
name_dict <- c(
  "FSM" = "FSM",
  "Saint Martin" = "MAF"
)

df <- df %>%
  mutate(
    adm_0_iso3 = countrycode(adm_0_name,
      origin = "country.name",
      destination = "iso3c",
      custom_match = name_dict,
      warn = TRUE
    )
  )
