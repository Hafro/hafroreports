# Was depth_plot
#' Plot catch by depth class (total and stacked)
#'
#' Queries the \code{logbook} table of a pax database and creates a two-panel
#' plot showing total catch (in thousands of tonnes) and proportional
#' composition by depth class (0–100 m, 100–200 m, 200–300 m, >300 m) over
#' time.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param depth_class Positive numeric vector of depth breaks.
#' @param year_start Integer. First year to include. Default is \code{1000}
#'   (no lower limit).
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{ggplot2} / \code{patchwork} plot object.
#' @export
hr_techreport_plot_catchdepth <- function(
  pcon,
  depth_class = c(0, 100, 200, 300),
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- ocean_depth_class <- catch <- NULL
  lang <- getOption("hr.lang", "en")

  dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(
      year >= year_start,
      year <= year_end
    ) |>
    pax::pax_add_ocean_depth_class(breaks = depth_class) |>
    dplyr::group_by(year, ocean_depth_class) |>
    dplyr::summarise(val = sum(catch, na.rm = TRUE) / 1e6) |>
    dplyr::rename(group = ocean_depth_class) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    two_panel_plot(
      fill = hr_label("total_catch_by_depth"),
      cols = c("#C7E9B4", "#7FCDBB", "#41B6C4", "#225EA8", 'darkblue')
    )
}
