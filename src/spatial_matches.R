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

df_fix <- df_raw |>
  mutate(
    adm_2_name = case_when(
      iso_a0 == "COL" & adm_1_name %in% c(
        "barranquilla",
        "cartagena",
        "santa marta",
        "bogota"
      ) ~ adm_1_name,
      iso_a0 == "NPL" & adm_1_name %in% c(
        "bagmati",
        "madhesh",
        "province 1",
        "sudurpaschim",
        "karnali",
        "gandaki",
        "lumbini"
      ) ~ adm_1_name,
      iso_a0 == "SAU" & !is.na(adm_1_name) ~ adm_1_name,
      iso_a0 == "THA" & adm_1_name == "bungkan" ~ adm_1_name,
      TRUE ~ adm_2_name
    )
  )

df_fix <- df_fix |>
  mutate(
    adm_1_name = case_when(
      iso_a0 == "COL" & adm_2_name == "barranquilla" ~ "atlantico",
      iso_a0 == "COL" & adm_2_name == "cartagena" ~ "bolivar",
      iso_a0 == "COL" & adm_2_name == "santa marta" ~ "magdalena",
      iso_a0 == "COL" & adm_2_name == "bogota" ~ "bogota",
      iso_a0 == "NPL" & adm_2_name == "bagmati" ~ "central",
      iso_a0 == "NPL" & adm_2_name == "madhesh" ~ "central",
      iso_a0 == "NPL" & adm_2_name == "province 1" ~ "eastern",
      iso_a0 == "NPL" & adm_2_name == "sudurpaschim" ~ "far western",
      iso_a0 == "NPL" & adm_2_name == "karnali" ~ "mid western",
      iso_a0 == "NPL" & adm_2_name == "gandaki" ~ "western",
      iso_a0 == "NPL" & adm_2_name == "lumbini" ~ "western",
      iso_a0 == "THA" & adm_2_name == "bungkan" ~ "bung kan",
      TRUE ~ adm_1_name
    )
  )

gaul_recode <- readr::read_csv("data/fao_gaul/gaul_recode.csv")

df_fix <- df_fix |>
  dplyr::left_join(
    gaul_recode |>
      dplyr::select(adm_0_name, adm_1_name, adm_2_name, gaul_2015_code),
    by = c("adm_0_name", "adm_1_name", "adm_2_name")
  ) |>
  dplyr::mutate(
    fao_gaul_code = dplyr::coalesce(gaul_2015_code, fao_gaul_code)
  ) |>
  dplyr::select(-gaul_2015_code)

df_fix <- df_fix |>
  mutate(
    gaul_level = case_when(
      !is.na(adm_0_name) & is.na(adm_1_name) & is.na(adm_2_name) ~ "adm0",
      !is.na(adm_0_name) & !is.na(adm_1_name) & is.na(adm_2_name) ~ "adm1",
      !is.na(adm_0_name) & !is.na(adm_2_name) ~ "adm2",
      TRUE ~ NA_character_
    )
  )



shp <- st_read("data/fao_gaul/g2015_2014_2/g2015_2014_2.shp")

all_gaul_codes <- unique(c(
  shp$ADM0_CODE,
  shp$ADM1_CODE,
  shp$ADM2_CODE
))

df_fix <- df_fix |>
  mutate(matched = fao_gaul_code %in% all_gaul_codes)

unmatched <- df_fix |>
  dplyr::filter(!matched)

# mismatches <- df_fix |>
#   dplyr::filter(
#     !is.na(adm_1_name),
#     !is.na(adm1_name_shp),
#     adm_1_name != adm1_name_shp
#   ) |>
#   distinct(adm_0_name, fao_gaul_code, adm1_name_shp, adm_1_name, adm_2_name, adm2_name_shp)


# x_shp <- shp |> dplyr::filter(tolower(ADM0_NAME) == "indonesia")
# View(x_shp)

####
