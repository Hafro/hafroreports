hr_update_hist <- function(
  historical_file,
  assessment_year,
  ...
) {
  readr::read_csv2(historical_file) |>
    dplyr::filter(assessment_year < .env$assessment_year) |>
    dplyr::bind_rows(as.data.frame(c(
      list(assessment_year = assessment_year),
      list(...)
    )))
}
