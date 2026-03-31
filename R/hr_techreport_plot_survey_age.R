# Was survey_at_age_bubble_plot
hr_techreport_plot_survey_age_bubble <- function(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
) {
  # NSE variables
  year <- age <- yc <- age_lab <- smb <- smh <- name <- ind <- NULL
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
    dplyr::select(year, yc, age, age_lab, smb, smh) |>
    tidyr::pivot_longer(-c(year, yc, age, age_lab), values_to = 'ind') |>
    dplyr::mutate(
      name = ifelse(
        name == 'smb',
        local(hr_label("spring_survey")),
        local(hr_label("autumn_survey"))
      )
    )

  ggplot2::ggplot(dat) +
    ggplot2::geom_col(
      ggplot2::aes(year, ind, fill = as.factor(yc)),
      position = 'dodge'
    ) +
    hr_theme_crayola_fill() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_grid(age_lab ~ name, scale = 'free_y') +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = hr_label("survey_at_age"), x = hr_label("year")) +
    ggplot2::scale_x_continuous(
      breaks = seq(year_start, year_end, by = 4),
      minor_breaks = seq(year_start, year_end, by = 2)
    )
}
