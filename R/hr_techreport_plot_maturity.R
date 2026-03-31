# Was igfs_mat_plot
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
