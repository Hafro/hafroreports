hr_techreport_plot_landings_gear <- function(
  pcon,
  mfdb_gear_codes = c('LLN', 'DSE', 'BMT'),
  year_start = 1000
) {
  dplyr::tbl(pcon, "landings") |>
    dplyr::filter(
      year >= year_start,
      mfdb_gear_code %in% mfdb_gear_codes
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
