hr_techreport_plot_comm_joy_ldist <- function(
  pcon,
  length_min = 0,
  length_max = 1e6,
  year_start = 1000,
  year_end = 9999,
  mfdb_gear_codes = c('BMT', 'DSE', 'LLN', 'GIL'),
  sampling_types = c(1, 2, 4, 8),
  max_height = 50,
  split_by_sex = FALSE
) {
  dplyr::tbl(pcon, "station") |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
      mfdb_gear_code %in% mfdb_gear_codes,
      !(mfdb_gear_code == 'DSE' & year < 1984),
      !(mfdb_gear_code == 'GIL' & year == 2021),
      sampling_type %in% sampling_types,
    ) |>
    pax::pax_ldist_by_year() |>
    dplyr::filter(
      length > length_min,
      length < length_max,
    ) |>
    pax::pax_ldist_joy_plot(
      max_height = max_height,
      split_by_sex = split_by_sex
    )
}
