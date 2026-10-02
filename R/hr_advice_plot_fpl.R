#' Plot fishing pressure for advice sheet
#'
#' Line plot with confidence band of the harvest rate (\code{"HR"}) or fishing
#' mortality (\code{"F"}) in the current assessment, with dashed lines for the
#' management, MSY and precautionary reference points the stock has
#' (\code{HR_mgt}, \code{HR_msy}, \code{HR_pa} or \code{F_mgt}, \code{F_msy},
#' \code{F_pa} in \code{ref_points}), and for category 3 stocks the MSY
#' proxy harvest rate (\code{HR_msy_proxy}).
#'
#' @param data_assessment Long-format assessment data as returned by
#'   \code{\link{hr_advice_data_assessment}}.
#' @param assessment_year Integer. The assessment year to plot.
#' @param ref_points Named list of reference points.
#' @param fishing_pressure \code{"HR"} (harvest rate) or \code{"F"}. Default
#'   \code{"HR"}.
#' @param title Plot title. Default: harvest rate, or fishing mortality.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_fpl <- function(
  data_assessment,
  assessment_year,
  ref_points,
  fishing_pressure = c("HR", "F"),
  title = NULL
) {
  # NSE variables
  key <- year <- median <- low <- high <- value <- label_year <- label <- NULL
  lang <- getOption("hr.lang", "en")
  fishing_pressure <- match.arg(fishing_pressure)

  refs <- advice_ref_lines(ref_points, fishing_pressure)
  d <- data_assessment |>
    dplyr::filter(key == fishing_pressure, assessment_year == .env$assessment_year)
  # Reference point labels above their lines, spaced apart from the right so
  # close values (e.g. HR_mgt = 0.2, HR_msy = 0.21) don't overlap
  refs$label_year <- assessment_year - 4 - 9 * (seq_len(nrow(refs)) - 1)

  ggplot2::ggplot(d, ggplot2::aes(x = year, y = median)) +
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
    ggiraph::geom_line_interactive(linewidth = 0.5, col = 'tomato') +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = low, ymax = high),
      fill = 'tomato',
      alpha = 0.4
    ) +
    hr_astand_theme(legend.position = c(0.75, 0.90)) +
    ggplot2::labs(
      y = '',
      title = if (is.null(title)) {
        hr_label(if (fishing_pressure == "HR") "harvest_rate" else "fbar", bold = TRUE)
      } else {
        title
      }
    ) +
    ggplot2::geom_hline(
      data = refs,
      ggplot2::aes(yintercept = value),
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = refs,
      ggplot2::aes(x = label_year, y = value * 1.07, label = label),
      inherit.aes = FALSE,
      size = 2.5,
      parse = TRUE
    ) +
    hr_advice_y_scale() +
    hr_astand_x_scale(5, 0)
}

#' Reference lines for fishing pressure
#'
#' @param ref_points Named list of reference points.
#' @param fishing_pressure \code{"HR"} or \code{"F"}.
#' @return A tibble with the reference points the stock has: \code{value} and
#'   a plotmath \code{label}.
#' @noRd
advice_ref_lines <- function(ref_points, fishing_pressure) {
  keys <- paste0(fishing_pressure, c("_mgt", "_msy", "_pa", "_msy_proxy"))
  labels <- paste0(fishing_pressure, c("mgt", "msy", "pa", "msy_proxy"))
  value <- vapply(keys, function(k) {
    v <- ref_points[[k]]
    if (is.null(v) || length(v) != 1) NA_real_ else as.numeric(v)
  }, numeric(1))
  out <- tibble::tibble(
    value = value,
    label = vapply(labels, hr_label, character(1))
  )
  out <- out[!is.na(out$value), ]
  # Same value (e.g. HR_msy = HR_mgt): one line, one label
  out[!duplicated(out$value), ]
}
