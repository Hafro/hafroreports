# Was model_resid_plot & model_pres_resid_plot
#' Plot SAM model residuals by year, age, and fleet
#'
#' Creates a bubble plot of SAM model residuals from a fitted SAM object.
#' Bubble size represents residual magnitude and colour indicates sign
#' (red = negative, blue = positive). Residuals are faceted by fleet.
#'
#' @param res A fitted SAM model object, as returned by
#'   \code{\link[stockassessment]{sam.fit}}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_samres_resid <- function(
  res
) {
  # NSE variables
  year <- age <- residual <- fleet <- NULL
  res |>
    SAMutils::format_sam_pres() |>
    ggplot2::ggplot(ggplot2::aes(
      year,
      age,
      size = residual,
      col = as.factor(sign(residual))
    )) +
    ggplot2::geom_point(pch = 20) +
    ggplot2::facet_wrap(~fleet, ncol = 1) +
    ggplot2::scale_size_area(max_size = 10) +
    ggplot2::labs(col = 'Sign', size = 'Size') +
    ggplot2::scale_colour_manual(values = c('red', 'blue'))
}

#' Plot SAM combined survey fit
#'
#' Computes observed and predicted survey biomass indices (stock-weight
#' expanded, in thousands of tonnes) from SAM model residuals, and plots
#' points (observed) with overlaid lines (predicted) for each fleet.
#'
#' @param res A fitted SAM model object, as returned by
#'   \code{\link[stockassessment]{sam.fit}}.
#' @param model_data A data frame with columns \code{year}, \code{age}, and
#'   \code{stock_weight} used to expand numbers to biomass.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_samres_combfit <- function(
  res,
  model_data
) {
  # NSE variables
  year <- age <- fleet <- residual <- stock_weight <- observation <- mean <- obs <- pred <- NULL
  res |>
    SAMutils::format_sam_res() |>
    dplyr::filter(year > 1980, residual != 0, fleet != 'Residual catch') |>
    dplyr::mutate(
      age = ifelse(fleet == 'spring', age, pmin(12, age + 1)),
      year = ifelse(fleet == 'spring', year, year + 1)
    ) |>
    dplyr::left_join(
      model_data |>
        dplyr::select(year, age, stock_weight)
    ) |>
    dplyr::group_by(year, fleet) |>
    dplyr::summarise(
      obs = sum(stock_weight * exp(observation), na.rm = TRUE) / 1e6,
      pred = sum(stock_weight * exp(mean), na.rm = TRUE) / 1e6
    ) |>
    ggplot2::ggplot(ggplot2::aes(year, obs, col = fleet)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = pred)) +
    ggplot2::scale_colour_manual(values = c('red', 'darkblue')) +
    ggplot2::labs(col = '', y = 'Survey index', x = 'Year') +
    ggplot2::theme(
      legend.position = c(0.2, 0.8),
      legend.background = ggplot2::element_blank()
    )
}
