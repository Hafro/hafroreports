#sampling_by_month_plot <-
#  tidypax:::sampling_overview_plot(mar, species_nr = species_code, tyr = tyr)
#' Plot sampling coverage by month, gear, and sampling type
#'
#' Creates a faceted tile/bar plot comparing the monthly distribution of
#' biological samples to the monthly distribution of landings, grouped by
#' gear and year. Sampling type is shown as stacked bar fill; the number of
#' samples per month is annotated above each bar.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param mfdb_gear_codes Character vector of MFDB gear codes to include.
#'   Default is \code{c("LLN", "DSE", "BMT")}.
#' @param sampling_types Integer vector of sampling type codes. Default is
#'   \code{c(1, 2, 3, 4, 8)}.
#' @param gear_group Named list mapping gear group labels to MFDB gear codes,
#'   used to group stations and landings by gear. Default groups are
#'   \code{BMT}, \code{LLN}, and \code{DSE}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_sampling_overview <- function(
  pcon,
  mfdb_gear_codes = c('LLN', 'DSE', 'BMT'),
  sampling_types = c(1, 2, 3, 4, 8),
  gear_group = list(
    BMT = "BMT",
    LLN = 'LLN',
    DSE = "DSE"
  ),
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- NULL
  month <- NULL
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
  gear_name <- NULL
  catch <- NULL

  dat <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
      sampling_type %in% sampling_types
    ) |>
    # Only include samples that have a length measurement
    dplyr::semi_join(dplyr::tbl(pcon, "ldist"), by = "sample_id") |>
    pax::pax_add_gear_group(gear_group = gear_group) |>
    dplyr::filter(!is.na(gear_name)) |>
    dplyr::group_by(year, month, gear_name, sampling_type) |>
    dplyr::summarise(n = dplyr::n_distinct(sample_id, na.rm = TRUE)) |>
    dplyr::group_by(gear_name, year) |>
    dplyr::mutate(p = (n) / sum(n)) |>
    dplyr::group_by(gear_name, year, month) |>
    dplyr::mutate(n = sum(n), pp = sum(p)) |>
    dplyr::full_join(
      dplyr::tbl(pcon, "landings") |>
        dplyr::filter(
          year >= year_start,
          year <= year_end,
        ) |>
        # Landings by gear
        pax::pax_add_gear_group(gear_group = gear_group) |>
        dplyr::filter(!is.na(gear_name)) |>
        dplyr::group_by(species, year, month, gear_name) |>
        dplyr::summarise(lnd = sum(catch, na.rm = TRUE)) |>
        # Window working out proportion of landings per month
        dplyr::group_by(species, year, gear_name) |>
        dplyr::mutate(p.lnd = ifelse(sum(lnd) == 0, 0, (lnd) / sum(lnd))),
      by = c("year", "month", "gear_name")
    ) |>
    pax::pax_describe_sampling_type() |>
    pax::pax_describe_mfdb_gear_code() |>
    dplyr::arrange(year, month, gear_name) |>
    dplyr::collect(n = Inf)

  ggplot2::ggplot(dat, ggplot2::aes(month, p.lnd)) +
    ggplot2::geom_bar(
      ggplot2::aes(y = p, fill = sampling_type_desc),
      stat = 'identity'
    ) +
    ggplot2::geom_text(ggplot2::aes(y = pp + 0.05, label = n)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(year ~ mfdb_gear_code_desc) +
    hr_theme_crayola_fill() +
    ggplot2::scale_x_continuous(breaks = c(seq(2, 12, by = 2))) +
    ggplot2::labs(
      x = hr_label('month'),
      y = hr_label("pct_samples_landings"),
      fill = ''
    ) +
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
