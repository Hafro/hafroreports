# Was catch_by_area_plot
#' Plot catch by geographic region (total and stacked)
#'
#' Queries the \code{logbook} table of a pax database and creates a two-panel
#' plot showing total catch and proportional composition by region
#' (W, NW, NE, SE, SW, and other) over time. Region names are localised to
#' the current language setting.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{ggplot2} / \code{patchwork} plot object.
#' @export
hr_techreport_plot_catch_region <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- mfdb_gear_code <- region <- catch <- ocean_depth_class <- NULL
  coalesce <- NULL

  dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_add_ocean_depth_class(breaks = c(0, 100, 200, 300)) |>
    pax::pax_add_regions(
      regions = list(
        W = 101,
        NW = 102,
        NE = c(103, 104, 105),
        SE = c(107, 106),
        SW = 108
      ) |>
        stats::setNames(sapply(c("W", "NW", "NE", "SE", "SW"), hr_label))
    ) |>
    dplyr::mutate(region = coalesce(region, local(hr_label('other')))) |>
    dplyr::group_by(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::summarise(val = sum(catch, na.rm = TRUE) / 1e6) |>
    dplyr::rename(group = region) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    two_panel_plot(
      cols = c(
        "#999999",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
      )
    )
}
