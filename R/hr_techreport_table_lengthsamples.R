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
    dplyr::ungroup() |>
    dplyr::relocate(year) |>
    dplyr::arrange(year) |>
    dplyr::collect() |>
    dplyr::rename_with(
      ~ gsub('__n$', paste0("__", hr_label("number_of_samples")), .x)
    ) |>
    dplyr::rename_with(
      ~ gsub('__n_lengths$', paste0("__", hr_label("number_of_lengths")), .x)
    ) |>
    dplyr::rename_with(
      ~ gsub('__n_otol$', paste0("__", hr_label("number_of_otoliths")), .x)
    ) |>
    dplyr::rename_with(~ hr_label("year"), year) |>
    hafroreports:::tbl_formater()
}
