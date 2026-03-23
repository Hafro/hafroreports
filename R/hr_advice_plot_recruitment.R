hr_advice_plot_recruitment <- function(
  assessment_results,
  assessment_year
) {
  lang <- getOption("hr.lang", "en")

  assessment_results |>
    dplyr::filter(key == 'recruitment', assessment_year == assessment_year) |>
    ggplot2::ggplot(ggplot2::aes(year, median / 1000)) +
    ggiraph::geom_bar_interactive(
      stat = 'identity',
      fill = 'deepskyblue',
      ggplot2::aes(
        tooltip = paste(
          eval(rlang::sym(paste('label', lang, sep = '.'))),
          ':',
          round(median / 1e3),
          'mill.',
          '\n',
          hr_label("year"),
          ':',
          year
        ),
        data_id = year
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = low / 1000, ymax = high / 1000),
      size = 0.25
    ) +
    hr_astand_theme() +
    ggplot2::labs(
      y = hr_label("million_tonnes", bold = TRUE),
      title = hr_label("recruitment_age", 1, bold = TRUE)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 600, 100),
      expand = c(0, 0),
      limits = c(0, 600)
    ) +
    hr_astand_x_scale(5)
}
