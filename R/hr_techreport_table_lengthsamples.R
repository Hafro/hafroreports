hr_techreport_table_lengthsamples <- function(
  pcon,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8),
  year_start = 1000,
  year_end = 9999,
  include_cols = c("lengths", "otol")
) {
  sampling <- dplyr::tbl(pcon, "sampling") |>
    dplyr::inner_join(dplyr::tbl(pcon, "measurement")) |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_sampling_detail(
      mfdb_gear_code = mfdb_gear_code,
      sampling_type = sampling_type
    )

  if (!("lengths" %in% include_cols)) {
    sampling <- dplyr::select(sampling, -ends_with('lengths'))
  }
  if (!("otol" %in% include_cols)) {
    sampling <- dplyr::select(sampling, -ends_with('otol'))
  }

  sampling |>
    dplyr::rename_with(~ gsub('__n$', '__Number of samples', .x)) |>
    dplyr::rename_with(~ gsub('__n_lengths$', '__Number of lengths', .x)) |>
    dplyr::rename_with(~ gsub('__n_otol$', '__Number of otoliths', .x)) |>
    dplyr::ungroup() |>
    dplyr::relocate(year) |>
    dplyr::arrange(year) |>
    dplyr::rename(Year = year) |>
    dplyr::collect() |>
    hafroreports:::tbl_formater()
}
