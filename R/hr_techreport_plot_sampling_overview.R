#sampling_by_month_plot <-
#  tidypax:::sampling_overview_plot(mar, species_nr = species_code, tyr = tyr)
hr_techreport_plot_sampling_overview <- function(
  pcon,
  assessment_year
) {
  # NSE variables
  year <- NULL
  month <- NULL
  mfdb_gear_code <- NULL
  sampling_type <- NULL
  sample_id <- NULL
  n <- NULL
  p <- NULL
  species <- NULL
  landings <- NULL
  lnd <- NULL
  p.lnd <- NULL
  sampling_type_desc <- NULL
  pp <- NULL
  element_blank <- NULL
  element_line <- NULL
  lat <- NULL
  lon <- NULL
  year <- NULL
  mfdb_gear_code <- NULL

  dplyr::tbl(pcon, "station") |>
    dplyr::group_by(year, month, mfdb_gear_code, sampling_type) |>
    dplyr::summarise(n = dplyr::n_distinct(sample_id, na.rm = TRUE)) |>
    dplyr::group_by(mfdb_gear_code, year) |>
    #    dplyr::arrange(month) |>
    dplyr::mutate(p = (n) / sum(n)) |>
    dplyr::group_by(mfdb_gear_code, year, month) |>
    dplyr::mutate(n = sum(n), pp = sum(p)) |>
    dplyr::full_join(
      dplyr::tbl(pcon, "landings") |>
        # Landings by gear
        dplyr::group_by(species, year, month, mfdb_gear_code) |>
        dplyr::summarise(lnd = sum(catch, na.rm = TRUE)) |>
        # Window working out proportion of landings per month
        dplyr::group_by(species, year, mfdb_gear_code) |>
        dplyr::mutate(p.lnd = ifelse(sum(lnd) == 0, 0, (lnd) / sum(lnd))),
      #        dplyr::arrange(species, year, month),
      by = c("year", "month", "mfdb_gear_code")
    ) |>
    dplyr::ungroup() |>
    pax::pax_describe_sampling_type() |>
    pax::pax_describe_mfdb_gear_code() |>
    dplyr::collect(n = Inf) |>

    ggplot2::ggplot(ggplot2::aes(month, p.lnd)) +
    ggplot2::geom_bar(
      ggplot2::aes(y = p, fill = sampling_type_desc),
      stat = 'identity'
    ) +
    ggplot2::geom_text(ggplot2::aes(y = pp + 0.05, label = lnd)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(year ~ mfdb_gear_code_desc) +
    pax::pax_scale_fill_crayola() +
    ggplot2::scale_x_continuous(breaks = c(seq(2, 12, by = 2))) +
    ggplot2::labs(x = 'Month', y = 'Percent samples/landings', fill = '') +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      legend.position = 'top'
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey80", size = 0.25),
      panel.grid.minor = ggplot2::element_line(colour = "grey80", size = 0.25),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}
