library(tidyr)
library(dplyr)
library(rnaturalearth)
library(countrycode)
library(lubridate)
library(ggplot2)
library(ggnewscale)
library(sf)

# open dengue most recent
url <- "https://raw.githubusercontent.com/OpenDengue/master-repo/refs/heads/main/data/raw_data/masterDB_V1.2.csv"
df_raw <- read.csv(url)

# fix country names
name_dict <- c(
  "FSM" = "FSM",
  "Saint Martin" = "MAF"
)

# rnaturalearth adm0 and adm1 (countries + states)
adm0 <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  select(adm0_a3 = iso_a3, adm_0_geometry = geometry)

adm1 <- rnaturalearth::ne_states(returnclass = "sf") %>%
  select(adm0_a3, name, adm_1_geometry = geometry)

# clean ISO3 codes and dates
df <- df_raw %>%
  mutate(
    adm_0_iso3 = countrycode(adm_0_name,
      origin = "country.name",
      destination = "iso3c",
      custom_match = name_dict,
      warn = TRUE
    ),
    across(all_of(c("calendar_start_date", "calendar_end_date")), ymd)
  ) %>%
  mutate(
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
  left_join(adm1, by = c("adm_0_iso3" = "adm0_a3", "adm_1_name" = "name"))


# summarize number of years of data per adm1 and temporal coverage
coverage_summary <- df %>%
  st_as_sf() %>%
  group_by(adm_0_iso3, adm_1_name, adm_1_geometry) %>%
  summarise(years_reported = n_distinct(year), .groups = "drop") %>%
  st_as_sf() %>%
  mutate(
    years_bin = cut(
      years_reported,
      breaks = c(-Inf, 0, 2, 5, 10, 19, Inf),
      labels = c("0 years", "1-2 years", "3-5 years", "6-10 years", "11-19 years", "20+ years"),
      right = TRUE
    )
  )

# plot global coverage
# uncomment lines to zoom to americas (and comment out global)
ggplot() +
  geom_sf(data = adm1, fill = "grey95", color = "grey70", size = 0.2) +
  geom_sf(
    data = coverage_summary,
    aes(fill = years_bin),
    color = "grey70",
    size = 0.1
  ) +
  geom_sf(data = adm0, fill = NA, color = "grey60", size = 0.4) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  # coord_sf(xlim = c(-180, -20), ylim = c(-60, 85), expand = FALSE) +
  scale_fill_brewer(palette = "YlGnBu", name = "Years reported") +
  theme_minimal() +
  labs(
    title = "Number of years with dengue data reported",
    fill = "Years reported"
  )
# ggsave("output/figures/adm1_opendengue_coverage_americas.png", width = 10, height = 6)
ggsave("output/figures/adm1_opendengue_coverage.png", width = 10, height = 6)
