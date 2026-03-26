hr_techreport_table_boatsummary <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
  dplyr::tbl(pcon, "landings") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_landings_by_gear() |>
    dplyr::ungroup() |>
    dplyr::filter(
      gear_name %in% c('BMT', 'DSE', 'LLN', 'Other'),
      year >= year_start,
      catch > 0,
      country == 'Iceland'
    ) |>
    dplyr::mutate(catch = round(catch / 1e3)) |>
    pax::pax_landings_boat_summary() |>
    tbl_formater()
}
