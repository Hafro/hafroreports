#' Plot landings by country
#'
#' Queries the \code{landings} table and creates a stacked bar chart of
#' annual landings in thousands of tonnes, split between Icelandic and
#' foreign catches. Country labels are localised to the current language.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param ylab Character. Y-axis label. Defaults to the localised label for
#'   landings in thousands of tonnes.
#' @param xlab Character. X-axis label. Defaults to the localised label for
#'   year.
#' @param breaks Numeric vector of x-axis break points. Default is every
#'   10 units from 0 to 100 000.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_landings_country <- function(
  pcon,
  ylab = hr_label("landings_kt"),
  xlab = hr_label('year'),
  breaks = seq(0, 1e5, by = 10),
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- country <- catch <- NULL
  dat <- dplyr::tbl(pcon, "landings") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    dplyr::mutate(
      country = ifelse(
        country != 'Iceland' | is.na(country),
        local(hr_label("other_country")),
        local(hr_label('iceland'))
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
    hr_theme_crayola_fill()
}
