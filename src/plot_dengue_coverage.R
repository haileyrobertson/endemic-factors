#' Plot ADM1 dengue data coverage and save figures
#'
#' @title plot_dengue_coverage
#' @param clean_df Cleaned dengue data frame
#' @param adm0 R natural earth adm0 spatial data frame
#' @param adm1 R natural earth adm1 spatial data frame
#' @param output_dir Directory where output figures will be saved
#' @return Character vector of file paths (for {targets} file tracking)
#' @export
#'
plot_dengue_coverage <- function(clean_df, adm0, adm1, output_dir) {
  coverage_summary <- clean_df %>%
    st_as_sf() %>%
    group_by(adm_0_iso3, adm_1_name, adm_1_geometry) %>%
    summarise(years_reported = n_distinct(year), .groups = "drop") %>%
    st_as_sf() %>%
    mutate(
      years_bin = cut(
        years_reported,
        breaks = c(-Inf, 0, 2, 5, 10, 19, Inf),
        labels = c(
          "0 years", "1-2 years", "3-5 years",
          "6-10 years", "11-19 years", "20+ years"
        ),
        right = TRUE
      )
    )

  plot_covg <- function(xlim_vals, ylim_vals) {
    selected_cols <- paletteer::paletteer_d("MoMAColors::Ernst")[c(1, 3, 4, 6, 8)]

    ggplot() +
      geom_sf(data = adm1, fill = "grey95", color = "grey70", size = 0.2) +
      geom_sf(
        data = coverage_summary,
        aes(fill = years_bin),
        color = "grey70",
        size = 0.1
      ) +
      geom_sf(data = adm0, fill = NA, color = "grey60", size = 0.4) +
      coord_sf(xlim = xlim_vals, ylim = ylim_vals, expand = FALSE) +
      scale_fill_manual(values = selected_cols, name = "Years reported") +
      theme_minimal() +
      labs(
        title = "Number of years with dengue data reported",
        fill = "Years reported"
      )
  }

  fig_dir <- file.path(output_dir, "figures")

  americas <- file.path(fig_dir, "adm1_opendengue_coverage_americas.png")
  global <- file.path(fig_dir, "adm1_opendengue_coverage_global.png")

  # Set the extent for the areas we want
  ext_americas <- plot_covg(xlim_vals = c(-180, -20), ylim_vals = c(-60, 85))
  ext_global <- plot_covg(xlim_vals = c(-180, 180), ylim_vals = c(-60, 85))

  ggsave(americas, plot = ext_americas, width = 10, height = 6)
  ggsave(global, plot = ext_global, width = 10, height = 6)

  return(c(americas, global))
}
