hr_techreport_plot_catch_wgt_growth <- function(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
) {
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
    dplyr::mutate(
      age_lab = ordered(
        max(age) - age,
        levels = 1:max(age),
        labels = (max(age) - 1):0
      )
    ) |>
    dplyr::select(
      year,
      yc,
      age,
      age_lab,
      ` Stock weights` = stock_weight,
      `Catch weights` = catch_weight
    ) |>
    tidyr::pivot_longer(
      -c(year:age_lab),
      names_to = 'survey',
      values_to = 'w'
    ) |>
    dplyr::filter(round(w) != 4000, survey != ' Stock weights') |>
    dplyr::group_by(survey, age) |>
    dplyr::arrange(age) |>
    dplyr::mutate(mw = mean(w, na.rm = TRUE))

  ggplot2::ggplot(dat) +
    ggplot2::geom_segment(
      ggplot2::aes(year, w, xend = year, yend = mw, col = as.factor(yc)),
      size = 3
    ) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = mw)) +
    ggplot2::geom_label(
      ggplot2::aes(label = age),
      data = dat |>
        dplyr::select(age, age_lab) |>
        dplyr::distinct(age, age_lab) |>
        dplyr::mutate(survey = " Stock weights"),
      x = -Inf,
      y = Inf,
      vjust = 1.2,
      hjust = -0.5
    ) +
    pax::pax_scale_col_crayola() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_wrap(~age, scale = 'free_y', drop = TRUE, dir = 'v') +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    ) +
    ggplot2::labs(y = 'Mean weight (g)', x = 'Year') +
    ggplot2::scale_x_continuous(
      breaks = seq(year_start, year_end, by = 4),
      minor_breaks = seq(year_start, year_end, by = 2)
    ) +
    ggplot2::expand_limits(x = 1983.5)
}
