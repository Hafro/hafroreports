#' Plot commercial length-frequency distribution as a joy plot
#'
#' Queries the \code{station} table of a pax database, filters by gear code
#' and sampling type, and produces a ridgeline (joy) plot of length
#' distributions by year.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param length_min Numeric. Minimum length to include (exclusive).
#'   Default is \code{0}.
#' @param length_max Numeric. Maximum length to include (exclusive).
#'   Default is \code{1e6}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @param mfdb_gear_codes Character vector of MFDB gear codes to include.
#'   Default is \code{c("BMT", "DSE", "LLN", "GIL")}.
#' @param sampling_types Integer vector of sampling type codes. Default is
#'   \code{c(1, 2, 4, 8)}.
#' @param max_height Numeric. Scaling factor for ridge height.
#'   Default is \code{50}.
#' @param split_by_sex Logical. If \code{TRUE}, produces separate ridges
#'   for each sex. Default is \code{FALSE}.
#' @return A \code{ggplot2} plot object.
#' @export
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
  # NSE variables
  year <- mfdb_gear_code <- sampling_type <- NULL

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
    ) +
    ggplot2::labs(y = hr_label("year"), x = hr_label("length"))
}
