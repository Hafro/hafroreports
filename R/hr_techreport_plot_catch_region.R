# Was catch_by_area_plot
hr_techreport_plot_catch_region <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
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
