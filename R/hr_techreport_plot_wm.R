# Was weight_model_plot_left
#' Plot stock weight growth model (left panel)
#'
#' Plots weight at age \eqn{a+1} in year \eqn{y+1} against weight at age
#' \eqn{a} in year \eqn{y} on log scales, with a linear model smoother.
#' Colours distinguish the current assessment year from historical data.
#'
#' @param input_data A data frame with columns \code{year}, \code{age}, and
#'   \code{stock_weight} (grams), as produced by \code{\link{hr_input_data_had}}.
#' @param assessment_year Year report is providing an assessment for
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_wm_left <- function(
  input_data,
  assessment_year
) {
  # NSE variables
  year <- age <- stock_weight <- yc <- period <- w1 <- marker <- NULL

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
      w1 = dplyr::lag(stock_weight),
      marker = ifelse(year == assessment_year, as.character(assessment_year), '')
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
#' Plot catch weight vs. stock weight relationship (centre panel)
#'
#' Plots catch mean weight against stock mean weight on linear scales with
#' a linear model smoother, coloured by time period. Used to visualise
#' the systematic relationship between the two weight series.
#'
#' @param input_data A data frame with columns \code{year}, \code{age},
#'   \code{stock_weight} (grams), and \code{catch_weight} (grams), as
#'   produced by \code{\link{hr_input_data_had}}.
#' @param assessment_year Year report is providing an assessment for
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_wm_center <- function(
  input_data,
  assessment_year
) {
  # NSE variables
  year <- age <- stock_weight <- catch_weight <- yc <- period <- marker <- NULL

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
      marker = ifelse(year == assessment_year - 1, as.character(assessment_year), ''),
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
#' Plot year-effect estimates from stock weight growth model (right panel)
#'
#' Fits the linear weight-growth model and extracts the year-effect
#' coefficients (\eqn{\delta_y}). Plots these year effects with 95\%
#' confidence intervals and a horizontal reference line at 0.9.
#'
#' @param input_data A data frame with columns \code{year}, \code{age}, and
#'   \code{stock_weight} (grams), as produced by \code{\link{hr_input_data_had}}.
#' @param assessment_year Year report is providing an assessment for
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_wm_right <- function(
  input_data,
  assessment_year
) {
  # NSE variables
  year <- age <- stock_weight <- w1 <- yc <- term <- estimate <- conf.low <- conf.high <- NULL

  wm <-
    input_data |>
    dplyr::mutate(
      stock_ = ifelse(stock_weight == 4000, NA, stock_weight),
      yc = year - age
    ) |>
    dplyr::select(yc, year, age, stock_weight) |>
    dplyr::group_by(yc) |>
    dplyr::arrange(age) |>
    dplyr::mutate(w1 = dplyr::lag(stock_weight)) |>
    dplyr::filter(
      age > 1,
      (year > 1984 & age < 8) | (year > 2013 & age < 10)
    ) |>
    stats::na.omit() |>
    stats::lm(log(stock_weight / w1) ~ log(w1) + as.factor(year - 1), data = _)

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
