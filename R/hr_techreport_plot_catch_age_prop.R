#' Plot catch proportions by year class (total and stacked)
#'
#' Creates a two-panel plot showing total catch weight (in thousands of
#' tonnes) and its proportional composition by year class over time.
#'
#' @param input_data A data frame with columns \code{year}, \code{age},
#'   \code{catch} (numbers), and \code{catch_weight} (mean weight in grams),
#'   as produced by \code{\link{hr_input_data_had}} or similar.
#' @param year_start Integer. First year to display. Default is \code{1970}.
#' @param year_end Integer. Last year to display. Default is \code{9999}.
#' @param age_start Integer. Minimum age to include. Default is \code{0}.
#' @param age_end Integer. Maximum age to include. Default is \code{9999}.
#' @return A \code{ggplot2} / \code{patchwork} plot object.
#' @export
hr_techreport_plot_catch_age_prop <- function(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
) {
  # NSE variables
  year <- age <- catch_weight <- catch <- yc <- val <- NULL

  dat <- input_data |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
      !is.na(age),
      age >= age_start,
      age <= age_end
    ) |>
    dplyr::mutate(
      val = catch_weight * catch / 1e6,
      yc = as.ordered(year - age)
    ) |>
    dplyr::select(year, group = yc, val)

  two_panel_plot(
    dat,
    cols = rep(
      c(
        "#A6CEE3",
        "#1F78B4",
        "#B2DF8A",
        "#33A02C",
        "#FB9A99",
        "#E31A1C",
        "#FDBF6F",
        "#FF7F00",
        "#CAB2D6",
        "#6A3D9A",
        "#FFFF99",
        "#B15928"
      ),
      100
    )
  )
}
