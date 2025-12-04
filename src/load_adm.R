#' This function loads the rnaturalearth adm0 and adm1 data
#'
#' @title load_adm
#' @param
#' @param
#' @returns adm0, adm1 R natural earth adm0 and adm1 spatial data frames
#' @export

load_adm <- function() {
  # rnaturalearth adm0 and adm1 (countries + states)
  adm0 <- rnaturalearth::ne_countries(returnclass = "sf") %>%
    select(adm0_a3 = iso_a3, region_un, adm_0_geometry = geometry)

  adm1 <- rnaturalearth::ne_states(returnclass = "sf") %>%
    select(adm0_a3, name, adm_1_geometry = geometry)

  return(list(adm0 = adm0, adm1 = adm1))
}
