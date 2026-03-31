#' Format length and otolith sampling summary table
#'
#' Queries the \code{sampling} and \code{measurement} tables of a pax
#' database and produces a GT table summarising the number of samples,
#' length measurements, and/or otolith readings per gear and year.
#' Column headers are localised to the current language.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param mfdb_gear_code Character vector of MFDB gear codes to include.
#'   Default is \code{c("BMT", "LLN", "DSE")}.
#' @param sampling_type Integer vector of sampling type codes. Default is
#'   \code{c(1, 2, 3, 4, 8)}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @param include_cols Character vector specifying which sample count columns
#'   to include. Valid values are \code{"lengths"} and \code{"otol"}.
#'   Default is \code{c("lengths", "otol")}.
#' @return A \code{gt} table object.
#' @export
hr_techreport_table_lengthsamples <- function(
  pcon,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8),
  year_start = 1000,
  year_end = 9999,
  include_cols = c("lengths", "otol")
) {
  # NSE variables
  year <- NULL

  sampling <- dplyr::tbl(pcon, "sampling") |>
    dplyr::inner_join(dplyr::tbl(pcon, "measurement")) |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_sampling_detail(
      mfdb_gear_code = mfdb_gear_code,
      sampling_type = sampling_type
    )

  if (!("lengths" %in% include_cols)) {
    sampling <- dplyr::select(sampling, -dplyr::ends_with('lengths'))
  }
  if (!("otol" %in% include_cols)) {
    sampling <- dplyr::select(sampling, -dplyr::ends_with('otol'))
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
    tbl_formater()
}
