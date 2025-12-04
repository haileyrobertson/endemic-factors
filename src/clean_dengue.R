#' This function cleans the raw Open Dengue data
#'
#' @title clean_dengue
#' @param df_raw Raw Open Dengue data frame
#' @param
#' @returns clean_df Cleaned dengue data frame
#' @export

clean_dengue <- function(df_raw, adm0, adm1) {
  # fix country names
  name_dict <- c(
    "FSM" = "FSM",
    "SAINT MARTIN" = "MAF",
    "KOSOVO" = "XKX",
    "SOMALILAND" = "SOM"
  )

  # change everything in open dengue that is "other" to something more specific
  region_dict <- c(
    "ABW" = "Americas",
    "AIA" = "Americas",
    "ASM" = "Oceania",
    "ATG" = "Americas",
    "BES" = "Americas",
    "BLM" = "Americas",
    "BMU" = "Americas",
    "BRB" = "Americas",
    "COK" = "Oceania",
    "CPV" = "Africa",
    "CUW" = "Americas",
    "CYM" = "Americas",
    "DMA" = "Americas",
    "FSM" = "Oceania",
    "GLP" = "Americas",
    "GRD" = "Americas",
    "GUF" = "Americas",
    "GUM" = "Oceania",
    "HKG" = "Asia",
    "KIR" = "Oceania",
    "KNA" = "Americas",
    "LCA" = "Americas",
    "MAC" = "Asia",
    "MAF" = "Americas",
    "MDV" = "Asia",
    "MHL" = "Oceania",
    "MNP" = "Oceania",
    "MSR" = "Americas",
    "MTQ" = "Americas",
    "MUS" = "Africa",
    "MYT" = "Africa",
    "NIU" = "Oceania",
    "NRU" = "Oceania",
    "PCN" = "Oceania",
    "PLW" = "Oceania",
    "PYF" = "Oceania",
    "REU" = "Africa",
    "SGP" = "Asia",
    "STP" = "Africa",
    "SXM" = "Americas",
    "TCA" = "Americas",
    "TKL" = "Oceania",
    "TON" = "Oceania",
    "TUV" = "Oceania",
    "VCT" = "Americas",
    "VGB" = "Americas",
    "VIR" = "Americas",
    "WLF" = "Oceania",
    "WSM" = "Oceania"
  )

  # clean ISO3 codes and dates
  clean_df <- df_raw %>%
    mutate(
      adm_0_iso3 = countrycode(adm_0_name,
        origin = "country.name",
        destination = "iso3c",
        custom_match = name_dict,
        origin_regex = TRUE,
        warn = TRUE
      ),
      across(all_of(c("calendar_start_date", "calendar_end_date")), ymd),
      year = year(calendar_start_date),
      period_length = calendar_end_date - calendar_start_date + 1,
      temporal_coverage = case_when(
        period_length >= 360 & period_length <= 370 ~ "annual",
        period_length >= 27 & period_length <= 31 ~ "monthly",
        period_length >= 4 & period_length <= 10 ~ "weekly",
        TRUE ~ "other"
      )
    ) %>%
    # join adm and dengue data
    left_join(adm0, by = c("adm_0_iso3" = "adm0_a3")) %>%
    left_join(adm1, by = c("adm_0_iso3" = "adm0_a3", "adm_1_name" = "name")) %>%
    # fix regions last after join w rnaturalearth
    mutate(
      region_un = ifelse(
        is.na(region_un),
        region_dict[adm_0_iso3],
        region_un
      )
    )

  return(clean_df)
}
