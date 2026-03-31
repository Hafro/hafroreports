#' Plot catch-at-age bar chart coloured by year class
#'
#' Creates a stacked bar chart showing catch numbers at each age over time,
#' with bars coloured by year class. Ages are displayed in descending order
#' as facet strips on the right-hand side.
#'
#' @param input_data A data frame with columns \code{year}, \code{age},
#'   and \code{catch} (catch numbers at age), as produced by
#'   \code{\link{hr_input_data_had}} or similar.
#' @param year_start Integer. First year to display. Default is \code{1970}.
#' @param year_end Integer. Last year to display. Default is \code{9999}
#'   (no upper limit).
#' @param age_start Integer. Minimum age to display. Default is \code{0}.
#' @param age_end Integer. Maximum age to display. Default is \code{9999}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_catch_age_bubble <- function(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
) {
  # NSE variables
  year <- age <- yc <- age_lab <- catch <- ind <- NULL
  catage <- NULL

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
    dplyr::select(year, yc, age, age_lab, catage = catch) |>
    dplyr::mutate(ind = ifelse(catage == 0, NA, catage))

  dat |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      stat = 'identity',
      ggplot2::aes(year, ind, fill = as.factor(yc))
    ) +
    hr_theme_crayola_fill() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_wrap(
      ~age_lab,
      ncol = 1,
      strip.position = 'right',
      scale = 'free_y'
    ) +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = hr_label("catch_at_age"), x = hr_label("year")) +
    ggplot2::scale_x_continuous(
      breaks = seq(year_start, year_end, by = 4),
      minor_breaks = seq(year_start, year_end, by = 2)
    )
}
