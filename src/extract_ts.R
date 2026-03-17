extract_ts <- function(data, locations) {
  df <- data %>%
    # join by country as first op
    # NOTE: This might break if multiple locations share a country
    # Maybe do semi_join? look into
    dplyr::inner_join(locations, by = "iso_a0") %>%
    # then match by admin level
    dplyr::filter(
      (level == "adm0" & iso_a0 == location) |
        (level == "adm1" & adm_1_name == location) |
        (level == "adm2" & adm_2_name == location)
    ) %>%
    # format dates
    dplyr::mutate(
      calendar_start_date = as.Date(calendar_start_date),
      calendar_end_date   = as.Date(calendar_end_date)
    ) %>%
    # drop missing dates
    dplyr::filter(!is.na(calendar_start_date)) %>%
    # drop yearly resolution to avoid duplication (only really matters at adm0 level)
    # TODO: figure out a rule for when to do this (bc sometimes its the best covg)
    # dplyr::filter(t_res != "year") %>%
    dplyr::select(
      iso_a0,
      adm_0_name,
      adm_1_name,
      adm_2_name,
      gaul_level,
      fao_gaul_code,
      t_res,
      calendar_start_date,
      calendar_end_date,
      year,
      dengue_total,
      case_definition_standardised
    )

  return(df)
}
