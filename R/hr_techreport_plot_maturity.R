# Was igfs_mat_plot
#' Plot maturity-at-age by year class
#'
#' Creates a faceted segment plot showing the annual proportion mature at each
#' age (coloured by year class) relative to the long-term mean maturity for
#' that age. Each facet corresponds to one age group.
#'
#' @param input_data A data frame with columns \code{year}, \code{age}, and
#'   \code{maturity} (proportion mature), as produced by
#'   \code{\link{hr_input_data_had}}.
#' @param year_start Integer. First year to display. Default is \code{1970}.
#' @param year_end Integer. Last year to display. Default is \code{9999}.
#' @param age_start Integer. Minimum age. Default is \code{0}.
#' @param age_end Integer. Maximum age. Default is \code{9999}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_maturity <- function(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
) {
  # NSE variables
  year <- age <- maturity <- yc <- age_lab <- mw <- NULL
  dat <- input_data |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
      !is.na(age),
      age >= age_start,
      age <= age_end
    ) |>
    dplyr::mutate(yc = year - age) |>
    dplyr::mutate(
      age_lab = ordered(
        max(age) - age,
        levels = 0:max(age - 1),
        labels = (max(age)):1
      )
    ) |>
    dplyr::group_by(age) |>
    dplyr::mutate(mw = mean(maturity, na.rm = TRUE))

  ggplot2::ggplot(dat) +
    ggplot2::geom_segment(
      ggplot2::aes(year, maturity, xend = year, yend = mw, col = as.factor(yc)),
      size = 3
    ) +
    hr_theme_crayola_col() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_wrap(~age, scale = 'free_y', strip.position = 'top') +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = 'Maturity (%)', x = 'Year')
}
