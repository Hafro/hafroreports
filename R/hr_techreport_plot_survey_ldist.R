dat_ldist_by_year <- function(
  pcon,
  sampling_type
) {
  # NSE variables
  species <- year <- sex <- length <- mfdb_gear_code <- count <- n <- NULL

  dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% .env$sampling_type) |>
    dplyr::left_join(
      dplyr::tbl(pcon, "ldist") |>
        pax::pax_ldist_scale_round() |>
        pax::pax_ldist_scale_abund(),
      by = "sample_id"
    ) |>
    dplyr::group_by(species, year, sex, length, mfdb_gear_code) |>
    dplyr::summarise(n = sum(count, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(
      year,
      mfdb_gear_code,
      species,
      length,
      sex,
      n
    )
}

# Was survey_ldist_joy_plot
hr_techreport_plot_survey_ldist_joy <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- NULL

  plots <- lapply(c(30, 35), function(sampling_type) {
    dat_ldist_by_year(pcon, sampling_type) |>
      dplyr::filter(
        year >= local(year_start),
        year <= local(year_end - ifelse(sampling_type == 35, 1, 0))
      ) |>
      pax::pax_ldist_joy_plot() +
      ggplot2::xlim(c(0, 80)) +
      ggplot2::xlab(NULL) +
      ggplot2::theme(
        plot.tag = ggplot2::element_text(size = ggplot2::rel(1)),
        plot.margin = ggplot2::margin(5.5, 5.5, 0, 5.5),
        plot.tag.position = "bottom"
      )
  })
  patchwork::wrap_plots(plots)
}

# Was agfs_ldist_plot
hr_techreport_plot_survey_ldist <- function(
  pcon,
  sampling_type
) {
  dat_ldist_by_year(pcon, sampling_type) |>
    pax::pax_ldist_plot()
}
