df_raw <- raw_dengue
names(df_raw) <- tolower(names(df_raw))

normalize <- function(x) {
  stringi::stri_trans_general(x, "latin-ascii") |>
    tolower() |>
    trimws()
}

df_raw <- df_raw |>
  mutate(
    adm_0_name = normalize(adm_0_name),
    adm_1_name = normalize(adm_1_name),
    adm_2_name = normalize(adm_2_name),
    full_name = normalize(full_name),
    case_definition_standardised = normalize(case_definition_standardised),
    s_res = normalize(s_res),
    t_res = normalize(t_res)
  )

df_raw <- df_raw |>
  mutate(
    gaul_level = case_when(
      !is.na(adm_0_name) & is.na(adm_1_name) & is.na(adm_2_name) ~ "adm0",
      !is.na(adm_0_name) & !is.na(adm_1_name) & is.na(adm_2_name) ~ "adm1",
      !is.na(adm_0_name) & !is.na(adm_2_name) ~ "adm2",
      TRUE ~ NA_character_
    )
  )

shp <- st_read("data/fao_gaul/g2015_2014_2/g2015_2014_2.shp")

df_adm0 <- df_raw |>
  dplyr::filter(gaul_level == "adm0") |>
  dplyr::left_join(
    shp |>
      sf::st_drop_geometry() |>
      dplyr::select(ADM0_CODE, ADM0_NAME) |>
      dplyr::distinct(),
    by = c("fao_gaul_code" = "ADM0_CODE")
  )

df_adm1 <- df_raw |>
  dplyr::filter(gaul_level == "adm1") |>
  dplyr::left_join(
    shp |>
      sf::st_drop_geometry() |>
      dplyr::select(ADM1_CODE, ADM0_NAME, ADM1_NAME) |>
      dplyr::distinct(),
    by = c("fao_gaul_code" = "ADM1_CODE")
  )

df_adm2 <- df_raw |>
  dplyr::filter(gaul_level == "adm2") |>
  dplyr::left_join(
    shp |>
      sf::st_drop_geometry() |>
      dplyr::select(ADM2_CODE, ADM0_NAME, ADM1_NAME, ADM2_NAME) |>
      dplyr::distinct(),
    by = c("fao_gaul_code" = "ADM2_CODE")
  )


adm0_df <- shp |>
  sf::st_drop_geometry() |>
  dplyr::distinct(ADM0_CODE, ADM0_NAME)

adm1_df <- shp |>
  sf::st_drop_geometry() |>
  dplyr::distinct(
    ADM0_CODE, ADM0_NAME,
    ADM1_CODE, ADM1_NAME
  )

adm2_df <- shp |>
  sf::st_drop_geometry() |>
  dplyr::distinct(
    ADM0_CODE, ADM0_NAME,
    ADM1_CODE, ADM1_NAME,
    ADM2_CODE, ADM2_NAME
  )

df_raw <- dplyr::bind_rows(df_adm0, df_adm1, df_adm2)

df_raw <- df_raw |>
  dplyr::mutate(
    matched = case_when(
      gaul_level == "adm0" ~ !is.na(ADM0_NAME),
      gaul_level == "adm1" ~ !is.na(ADM1_NAME),
      gaul_level == "adm2" ~ !is.na(ADM2_NAME),
      TRUE ~ FALSE
    )
  )

unmatched <- df_raw |>
  dplyr::filter(!matched) |>
  distinct(iso_a0, adm_0_name, adm_1_name, adm_2_name, fao_gaul_code)

df_raw <- df_raw |>
  mutate(
    adm0_name_shp = normalize(ADM0_NAME),
    adm1_name_shp = normalize(ADM1_NAME),
    adm2_name_shp = normalize(ADM2_NAME)
  )

# mismatches <- df_raw |>
#   dplyr::filter(
#     !is.na(adm_1_name),
#     !is.na(adm1_name_shp),
#     adm_1_name != adm1_name_shp
#   ) |>
#   distinct(adm_0_name, fao_gaul_code, adm1_name_shp, adm_1_name, adm_2_name, adm2_name_shp)

# gaul_recode <- unmatched |>
#   dplyr::distinct(fao_gaul_code) |>
#   dplyr::arrange(fao_gaul_code) |>
#   dplyr::rename(open_dengue_code = fao_gaul_code)

# readr::write_csv(
#   gaul_recode,
#   "gaul_recode.csv"
# )

x_shp <- shp |> dplyr::filter(tolower(ADM0_NAME) == "saudi arabia")
View(x_shp)
####
