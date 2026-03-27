hr_techreport_plot_landings_country <- function(
  pcon,
  ylab = 'Landings (in kt)',
  xlab = 'Year',
  breaks = seq(0, 1e5, by = 10),
  year_start = 1000,
  year_end = 9999
) {
  dat <- dplyr::tbl(pcon, "landings") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    dplyr::mutate(
      country = ifelse(
        country != 'Iceland' | is.na(country),
        'Other',
        'Iceland'
      )
    ) |>
    dplyr::group_by(year, country) |>
    dplyr::summarize(catch = sum(catch, na.rm = TRUE) / 1e3) |>
    dplyr::arrange(dplyr::desc(country))

  ggplot2::ggplot(dat, ggplot2::aes(year, catch, fill = country)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::theme_bw() +
    ggplot2::labs(y = ylab, x = xlab, fill = '') +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = c(0.15, 0.75)
    ) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    pax::pax_scale_fill_crayola()
}
