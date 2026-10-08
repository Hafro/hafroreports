# Was catch_by_area_plot
#' Plot catch by geographic region (total and stacked)
#'
#' Queries the \code{logbook} table of a pax database and creates a two-panel
#' plot showing total catch and proportional composition by region
#' (W, NW, NE, SE, SW, and other) over time. Region names are localised to
#' the current language setting.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param depth_class Positive numeric vector of depth breaks.
#' @param regions Named list mapping region labels to integer MFDB area codes.
#'   Default regions are W (101), NW (102), NE (103–105), SE (106–107),
#'   SW (108). The names are the labels, translated by \code{hr_label()}
#'   if they are known keys (e.g. \code{"NW"} is \code{"NV"} in Icelandic).
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @param keep_order If \code{TRUE}, the regions are stacked and listed in
#'   the order of \code{regions}, other last. Default \code{FALSE},
#'   alphabetical order of the labels.
#' @return A \code{ggplot2} / \code{patchwork} plot object.
#' @export
hr_techreport_plot_catch_region <- function(
  pcon,
  depth_class = c(0, 100, 200, 300),
  regions = list(
        W = 101,
        NW = 102,
        NE = c(103, 104, 105),
        SE = c(107, 106),
        SW = 108
      ),
  year_start = 1000,
  year_end = 9999,
  keep_order = FALSE
) {
  # NSE variables
  year <- mfdb_gear_code <- region <- catch <- ocean_depth_class <- group <- NULL
  coalesce <- NULL
  labels <- region_labels(regions)

  dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_add_ocean_depth_class(breaks = depth_class) |>
    pax::pax_add_regions(regions = stats::setNames(regions, labels)) |>
    dplyr::mutate(region = coalesce(region, local(hr_label('other')))) |>
    dplyr::group_by(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::summarise(val = sum(catch, na.rm = TRUE) / 1e6) |>
    dplyr::rename(group = region) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    region_order(labels, keep_order) |>
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

# Labels of the regions: their names, translated if they are hr_locale keys
# ("Other" as "other")
region_labels <- function(regions) {
  keys <- ifelse(tolower(names(regions)) == "other", "other", names(regions))
  vapply(
    keys,
    function(k) {
      if (k %in% hafroreports::hr_locale$key) hr_label(k) else k
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Order the region groups as regions (other last) if keep_order
region_order <- function(dat, labels, keep_order) {
  # NSE variables
  group <- NULL
  if (!isTRUE(keep_order)) {
    return(dat)
  }
  levels <- unique(c(labels, hr_label("other"), dat$group))
  dplyr::mutate(dat, group = factor(group, levels = levels[levels %in% group]))
}
