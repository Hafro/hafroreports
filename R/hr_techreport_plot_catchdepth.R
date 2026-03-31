# Was depth_plot
hr_techreport_plot_catchdepth <- function(
  pcon,
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
    pax::pax_add_ocean_depth_class(breaks = c(0, 100, 200, 300)) |>
    dplyr::group_by(year, ocean_depth_class) |>
    dplyr::summarise(val = sum(catch, na.rm = TRUE) / 1e6) |>
    dplyr::rename(group = ocean_depth_class) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    two_panel_plot(
      fill = "Total catch \n by depth (m)",
      cols = c("#C7E9B4", "#7FCDBB", "#41B6C4", "#225EA8", 'darkblue')
    )
}
