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
#' Plot survey length distributions as joy plots for both surveys
#'
#' Creates a side-by-side ridgeline (joy) plot of length distributions from
#' the spring (sampling type 30) and autumn (sampling type 35) groundfish
#' surveys. Each survey is shown in a separate panel.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#'   The autumn survey is excluded for the final year.
#' @return A \code{patchwork} plot object with two panels.
#' @export
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
#' Plot survey length distribution by year
#'
#' Queries the \code{station} and \code{ldist} tables for the specified
#' sampling type and renders a length-frequency plot with year on the y-axis.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param sampling_type Integer or integer vector of sampling type codes
#'   (e.g. \code{30} for the spring survey, \code{35} for the autumn survey).
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_survey_ldist <- function(
  pcon,
  sampling_type
) {
  dat_ldist_by_year(pcon, sampling_type) |>
    pax::pax_ldist_plot()
}
