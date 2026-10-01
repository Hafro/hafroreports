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
#' @param gear_group Gear groups, as for \code{pax::pax_landings_by_gear()}.
#'   Default \code{NULL} uses its default groups.
#' @export
hr_techreport_plot_landings_gear <- function(
  pcon,
  year_start = 1000,
  year_end = 9999,
  gear_group = NULL
) {
  # NSE variables
  year <- gear_name <- catch <- country <- NULL

  dplyr::tbl(pcon, "landings") |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
    ) |>
    (\(x) if (is.null(gear_group)) pax::pax_landings_by_gear(x) else pax::pax_landings_by_gear(x, gear_group = gear_group))() |>
    dplyr::ungroup() |>
    dplyr::filter(
      catch > 0,
      country == 'Iceland'
    ) |>
    dplyr::group_by(
      year,
      gear_name
    ) |>
    # Landings are in kg, plot in thousand tonnes
    dplyr::summarize(val = sum(catch) / 1e6) |>
    dplyr::rename(group = gear_name) |>
    two_panel_plot()
}
