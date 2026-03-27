# Was weight_model_plot_left
hr_techreport_plot_wm_left <- function(input_data) {
  input_data |>
    dplyr::filter(year > 1984) |>
    dplyr::mutate(
      stock_weight = ifelse(stock_weight == 4000, NA, stock_weight),
      yc = year - age,
      period = ifelse(
        year < 2000,
        '1985-1999',
        ifelse(year > 2012, '2013+', '2000-2012')
      )
    ) |>
    #dplyr::select(yc,year,age,stock_weight) |>
    dplyr::group_by(yc) |>
    dplyr::arrange(age) |>
    dplyr::mutate(
      w1 = lag(stock_weight),
      marker = ifelse(year == tyr, as.character(tyr), '')
    ) |>
    dplyr::arrange(year) |>
    # modelr::add_predictions(wm) |>
    ggplot2::ggplot(ggplot2::aes(w1, stock_weight)) +
    ggplot2::geom_point(ggplot2::aes(col = marker)) +
    #  ggplot2::geom_line(ggplot2::aes(y=exp(pred+log(w1))))+
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_smooth(method = 'lm') +
    ggplot2::labs(
      y = bquote(sW[a + 1][y + 1]),
      x = bquote(sW[a][y]),
      col = ''
    ) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::scale_color_manual(values = c('black', 'red'))
}

# Was weight_model_plot_center
hr_techreport_plot_wm_center <- function(input_data) {
  input_data |>
    dplyr::filter(
      year > 1984,
      stock_weight != catch_weight,
      catch_weight > 0
    ) |>
    dplyr::mutate(
      stock_weight = ifelse(round(stock_weight) == 4000, NA, stock_weight),
      catch_weight = ifelse(round(catch_weight) == 4000, NA, catch_weight),
      yc = year - age,
      marker = ifelse(year == tyr - 1, as.character(tyr), ''),
      period = ifelse(
        year < 2000,
        '1985-1999',
        ifelse(year > 2012, '2013+', '2000-2012')
      )
    ) |>

    ggplot2::ggplot(ggplot2::aes(stock_weight, catch_weight)) +
    ggplot2::geom_point(ggplot2::aes(col = marker)) +
    ggplot2::geom_smooth(method = 'lm') +
    ggplot2::scale_color_manual(values = c('black', 'red')) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(y = 'Catch weights (g)', x = 'Stock weights (g)')
}

# Was weight_model_plot_right
hr_techreport_plot_wm_right <- function(input_data) {
  wm <-
    input_data |>
    dplyr::mutate(
      stock_ = ifelse(stock_weight == 4000, NA, stock_weight),
      yc = year - age
    ) |>
    dplyr::select(yc, year, age, stock_weight) |>
    dplyr::group_by(yc) |>
    dplyr::arrange(age) |>
    dplyr::mutate(w1 = lag(stock_weight)) |>
    dplyr::filter(
      age > 1,
      (year > 1984 & age < 8) | (year > 2013 & age < 10)
    ) |>
    stats::na.omit() |>
    stats::lm(log(stock_weight / w1) ~ log(w1) + as.factor(year - 1), data = .)

  wm |>
    broom::tidy(conf.int = TRUE) |>
    dplyr::mutate(source = 'Stock weights')
  dplyr::filter(grepl('year', term)) |>
    dplyr::mutate(
      year = gsub('as.factor(year - 1)', '', term, fixed = TRUE) |>
        as.numeric(),
      period = ifelse(
        year < 2000,
        '1985-1999',
        ifelse(year > 2012, '2013+', '2000-2012')
      )
    ) |>
    dplyr::filter(year > 1984) |>
    ggplot2::ggplot(ggplot2::aes(year, exp(estimate))) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = exp(conf.low),
      ymax = exp(conf.high)
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0.9, lty = 2) +
    #ggplot2::geom_segment(ggplot2::aes(xend = year),yend=0)  +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(x = 'Year', y = bquote(delta[y]))
}
