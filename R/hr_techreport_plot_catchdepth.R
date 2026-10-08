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
#' @param mfdb_gear_code Gear codes of the logbook records to include, e.g.
#'   \code{c("BMT", "DSE")}. Default \code{NULL}, all gears.
#' @return A \code{ggplot2} / \code{patchwork} plot object.
#' @export
hr_techreport_plot_catchdepth <- function(
  pcon,
  depth_class = c(0, 100, 200, 300),
  year_start = 1000,
  year_end = 9999,
  mfdb_gear_code = NULL
) {
  # NSE variables
  year <- ocean_depth_class <- catch <- group <- NULL
  lang <- getOption("hr.lang", "en")
  gears <- mfdb_gear_code

  logbook <- dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(
      year >= year_start,
      year <= year_end
    )
  if (!is.null(gears)) {
    logbook <- dplyr::filter(logbook, mfdb_gear_code %in% local(gears))
  }
  logbook |>
    pax::pax_add_ocean_depth_class(breaks = depth_class) |>
    dplyr::group_by(year, ocean_depth_class) |>
    dplyr::summarise(val = sum(catch, na.rm = TRUE) / 1e6) |>
    dplyr::rename(group = ocean_depth_class) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    # Order depth classes by depth ("100+" would otherwise sort before "20-40")
    dplyr::mutate(
      group = factor(group, levels = unique(group)[order(as.numeric(sub("[^0-9].*$", "", unique(group))))])
    ) |>
    two_panel_plot(
      fill = hr_label("total_catch_by_depth"),
      # One colour per depth class, interpolated over the same ramp
      cols = grDevices::colorRampPalette(
        c("#C7E9B4", "#7FCDBB", "#41B6C4", "#225EA8", 'darkblue')
      )(length(depth_class) + 1)
    )
}
