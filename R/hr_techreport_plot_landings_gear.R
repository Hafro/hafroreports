#' Plot Icelandic landings by gear (total and stacked)
#'
#' Queries the \code{landings} table of a pax database, groups by gear
#' (BMT, DSE, LLN, Other), and creates a two-panel plot showing total
#' Icelandic landings (in thousands of tonnes) and their proportional
#' composition by gear over time.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{ggplot2} / \code{patchwork} plot object.
#' @export
hr_techreport_plot_landings_gear <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- gear_name <- catch <- country <- NULL

  dplyr::tbl(pcon, "landings") |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
    ) |>
    pax::pax_landings_by_gear() |>
    dplyr::ungroup() |>
    dplyr::filter(
      gear_name %in% c('BMT', 'DSE', 'LLN', 'Other'),
      catch > 0,
      country == 'Iceland'
    ) |>
    dplyr::group_by(
      year,
      gear_name
    ) |>
    dplyr::summarize(val = sum(catch) / 1e3) |>
    dplyr::rename(group = gear_name) |>
    two_panel_plot()
}
