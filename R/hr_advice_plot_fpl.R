#' Plot harvest rate time series for advice sheet
#'
#' Creates an interactive ggplot of the harvest rate (HR) time series with
#' a confidence ribbon, and overlays reference point lines for the management
#' (\code{HR_mgt}), precautionary (\code{HR_pa}), and MSY (\code{HR_msy})
#' harvest rates.
#'
#' @param data_assessment A long-format data frame as returned by
#'   \code{\link{hr_advice_data_assessment}}, filtered or containing a
#'   \code{key} column and \code{assessment_year} column.
#' @param assessment_year Integer. The assessment year to display.
#' @param ref_points A named list or data frame with elements \code{HR_mgt},
#'   \code{HR_pa}, and \code{HR_msy} giving the harvest rate reference points.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_fpl <- function(
  data_assessment,
  assessment_year,
  ref_points
) {
  # NSE variables
  key <- year <- median <- low <- high <- NULL
  lang <- getOption("hr.lang", "en")
  data_assessment |>
    dplyr::filter(key %in% c('HR'), assessment_year == .env$assessment_year) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = median)) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = paste(
          eval(rlang::sym(paste('label', lang, sep = '.'))),
          ':',
          round(median, 3),
          '\n',
          hr_label("year"),
          ':',
          year
        ),
        data_id = year
      ),
      size = .10,
      hover_nearest = TRUE,
      col = 'white',
      alpha = 0
    ) +
    ggiraph::geom_line_interactive(size = 0.5, col = 'tomato') +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = low, ymax = high),
      fill = 'tomato',
      alpha = 0.4
    ) +
    hr_astand_theme(legend.position = c(0.75, 0.90)) +
    ggplot2::labs(y = '', title = hr_label("harvest_rate", bold = TRUE)) +
    ggplot2::geom_hline(
      yintercept = ref_points$HR_mgt,
      linetype = "dashed",
      size = 0.4
    ) +
    #geom_hline(yintercept=ref_points$HR_lim, linetype="solid", size=0.4) +
    ggplot2::geom_hline(
      yintercept = ref_points$HR_pa,
      linetype = "dashed",
      size = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = 1985,
      y = ref_points$HR_mgt + 0.04,
      label = hr_label("HRmgt"),
      size = 2.5,
      parse = TRUE
    ) +
    ggplot2::annotate(
      "text",
      x = 1995,
      y = ref_points$HR_msy + 0.04,
      label = hr_label("HRmsy"),
      size = 2.5,
      parse = TRUE
    ) +
    #ggplot2::annotate("text", x=2013, y=ref_points$HR_lim+0.04, label= HRlim(), size=2.5, parse=TRUE) +
    ggplot2::annotate(
      "text",
      x = 1990,
      y = ref_points$HR_pa + 0.04,
      label = hr_label("HRpa"),
      size = 2.5,
      parse = TRUE
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, .2),
      expand = c(0, 0),
      limits = c(0, 1)
    ) +
    hr_astand_x_scale(5, 0)
}
