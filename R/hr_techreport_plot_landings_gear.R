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
