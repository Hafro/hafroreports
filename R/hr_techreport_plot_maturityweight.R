# Was maturity_model_plot
#' Plot maturity as a function of stock weight
#'
#' Plots observed proportion mature against stock weight (on a log scale),
#' coloured by time period, with a fitted model overlay. Two model types
#' are available: a single curve fitted to data from 2013 onwards
#' (\code{"pred"}), or period-specific curves (\code{"period"}).
#'
#' @param input_data A data frame with columns \code{year}, \code{age},
#'   \code{stock_weight} (grams), and \code{maturity} (proportion), as
#'   produced by \code{\link{hr_input_data_combine}}.
#' @param mat_model Character. Maturity model type: \code{"pred"} (default)
#'   fits a single GLM to the most recent period; \code{"period"} fits
#'   separate curves for three historical time periods.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_maturityweight <- function(
  input_data,
  mat_model = c("pred", "period")
) {
  # NSE variables
  year <- age <- stock_weight <- catch_weight <- yc <- period <- maturity <- pred <- NULL
  mat_model <- match.arg(mat_model)

  dat <- input_data |>
    dplyr::filter(year > 1984, age %in% 3:10) |>
    dplyr::mutate(
      stock_weight = ifelse(round(stock_weight) == 4000, NA, stock_weight),
      catch_weight = ifelse(round(catch_weight) == 4000, NA, catch_weight),
      yc = year - age,
      period = ifelse(
        year < 2000,
        '1985-1999',
        ifelse(year > 2012, '2013+', '2000-2012')
      )
    )

  if (mat_model == "period") {
    mat_model_period <-
      input_data |>
      dplyr::filter(year > 1984, age %in% 3:20) |>
      dplyr::mutate(
        stock_weight = ifelse(round(stock_weight) == 4000, NA, stock_weight),
        catch_weight = ifelse(round(catch_weight) == 4000, NA, catch_weight),
        maturity = ifelse(maturity >= 1, 1, maturity),
        yc = year - age,
        period = ifelse(
          year < 2000,
          '1985-1999',
          ifelse(year > 2012, '2013+', '2000-2012')
        )
      ) |>
      stats::glm(
        maturity ~ log(stock_weight) * period,
        data = _,
        family = stats::quasi(variance = "mu(1-mu)", link = "logit")
      )

    dat <- dat |>
      modelr::add_predictions(mat_model_period, var = 'pred') |>
      dplyr::mutate(pred = psych::logistic(pred))
    mod_geom <- ggplot2::geom_line(ggplot2::aes(y = pred, lty = period))
  }

  if (mat_model == "pred") {
    mat_model <-
      input_data |>
      dplyr::filter(year > 2012, age %in% 3:20) |>
      dplyr::mutate(
        stock_weight = ifelse(round(stock_weight) == 4000, NA, stock_weight),
        catch_weight = ifelse(round(catch_weight) == 4000, NA, catch_weight),
        maturity = ifelse(maturity >= 1, 1, maturity),
        yc = year - age,
        period = ifelse(
          year < 2000,
          '1985-1999',
          ifelse(year > 2012, '2013+', '2000-2012')
        )
      ) |>
      stats::glm(
        maturity ~ log(stock_weight),
        data = _,
        family = stats::quasi(variance = "mu(1-mu)", link = "logit")
      )

    dat <- dat |>
      modelr::add_predictions(mat_model, var = 'pred') |>
      dplyr::mutate(pred = psych::logistic(pred))
    mod_geom <- ggplot2::geom_line(ggplot2::aes(y = pred), col = 'black')
  }

  ggplot2::ggplot(dat, ggplot2::aes(stock_weight, maturity, col = period)) +
    ggplot2::geom_point() +
    mod_geom +
    ggplot2::scale_x_log10(breaks = c(200, 500, 1000, 2000, 5000)) +
    ggplot2::labs(
      col = 'Period',
      x = 'Stock weights',
      y = 'Proportion mature'
    ) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = c(0.8, 0.15)
    )
}
