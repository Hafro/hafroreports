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
#' @param show_lim If \code{TRUE}, also draw the limit reference point
#'   (\code{F_lim} or \code{HR_lim}), as for relative (F/F_MSY) stocks.
#'   Default \code{FALSE}.
#' @param points If \code{TRUE}, also draw the yearly values as points, so
#'   single years of a series with gaps show. Default \code{FALSE}.
#' @param y_label Y axis label. Default none.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_fpl <- function(
  data_assessment,
  assessment_year,
  ref_points,
  fishing_pressure = c("HR", "F"),
  title = NULL,
  show_lim = FALSE,
  points = FALSE,
  y_label = ''
) {
  # NSE variables
  key <- year <- median <- low <- high <- value <- label_year <- label <- NULL
  lang <- getOption("hr.lang", "en")
  fishing_pressure <- match.arg(fishing_pressure)

  refs <- advice_ref_lines(ref_points, fishing_pressure, show_lim = show_lim)
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
    (if (isTRUE(points)) ggplot2::geom_point(col = 'tomato', size = 1)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = low, ymax = high),
      fill = 'tomato',
      alpha = 0.4
    ) +
    hr_astand_theme(legend.position = c(0.75, 0.90)) +
    ggplot2::labs(
      y = y_label,
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
#' @param show_lim Also the limit reference point (\code{F_lim}).
#' @return A tibble with the reference points the stock has: \code{value} and
#'   a plotmath \code{label}.
#' @noRd
advice_ref_lines <- function(ref_points, fishing_pressure, show_lim = FALSE) {
  suffix <- c("mgt", "msy", "pa", "msy_proxy", if (isTRUE(show_lim)) "lim")
  keys <- paste0(fishing_pressure, "_", suffix)
  labels <- paste0(fishing_pressure, suffix)
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

#' Plot the fishing pressure proxy for a category 3 advice sheet
#'
#' \code{\link{hr_advice_plot_fpl}} for the length-based fishing pressure
#' proxy of the rfb rule (L_F=M / L_mean, held as \code{"F"} in the
#' assessment history), with the F_MSY proxy line and the title
#' \code{hr_label("fproxy")}. The proxy is close to 1, so the y axis can be
#' set to a range around it instead of starting at 0.
#'
#' @inheritParams hr_advice_plot_fpl
#' @param ref_points Named list of reference points with \code{F_msy_proxy}
#'   (the only line drawn).
#' @param year_start,year_end First and last year of the series shown.
#' @param y_limits \code{NULL} (default) for the standard axis from 0;
#'   otherwise \code{c(lower, upper)}, the least range of the y axis (either
#'   may be \code{NA}): the axis is widened to the values and the F_MSY
#'   proxy, rounded out to 0.1, with breaks every 0.1.
#' @param y_pad Also widen the axis by this much below and above the values
#'   and the F_MSY proxy (rounded out to 0.1), e.g. 0.1. Default 0; with
#'   \code{y_limits = NULL} and \code{y_pad > 0} the axis is from the data.
#' @param points Also draw the yearly values as points, so single years of a
#'   series with gaps show: \code{FALSE} (default), \code{TRUE} (size 0.8) or
#'   the point size.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_fproxy <- function(
  data_assessment,
  assessment_year,
  ref_points,
  year_start = -Inf,
  year_end = Inf,
  y_limits = NULL,
  y_pad = 0,
  points = FALSE
) {
  # NSE variables
  year <- key <- median <- .data <- NULL

  d <- data_assessment |>
    dplyr::filter(year >= .env$year_start, year <= .env$year_end)
  p <- hr_advice_plot_fpl(
    d,
    assessment_year = assessment_year,
    ref_points = list(F_msy_proxy = ref_points$F_msy_proxy),
    fishing_pressure = "F",
    title = hr_label("fproxy", bold = TRUE)
  )
  if (!isFALSE(points)) {
    p <- p +
      ggplot2::geom_point(
        data = d |>
          dplyr::filter(
            key == "F",
            .data$assessment_year == .env$assessment_year,
            !is.na(median)
          ),
        ggplot2::aes(x = year, y = median),
        colour = "tomato",
        size = if (isTRUE(points)) 0.8 else points,
        inherit.aes = FALSE
      )
  }
  if (!is.null(y_limits) || y_pad > 0) {
    values <- d$median[d$key == "F" & d$assessment_year == assessment_year]
    limits <- fproxy_axis_limits(
      c(values, ref_points$F_msy_proxy),
      y_limits = y_limits,
      y_pad = y_pad
    )
    p <- suppressMessages(
      p +
        ggplot2::scale_y_continuous(
          limits = limits,
          breaks = seq(limits[1], limits[2], 0.1),
          expand = c(0, 0)
        )
    )
  }
  p
}

#' Axis limits around a fishing pressure proxy
#'
#' The range of \code{values}, rounded out to 0.1 and widened by
#' \code{y_pad}, and at least \code{y_limits}.
#'
#' @param values Values to show (with the reference point).
#' @param y_limits \code{c(lower, upper)} the axis covers at least, or
#'   \code{NULL}; \code{NA} for none.
#' @param y_pad Widen by this much beyond the values.
#' @return Numeric limits \code{c(lower, upper)}.
#' @noRd
fproxy_axis_limits <- function(values, y_limits = NULL, y_pad = 0) {
  # Rounded on a grid of 10 per unit (multiply, round, divide by 10 rather
  # than by steps of 0.1, so the limits are exact multiples of 0.1)
  lower <- floor(min(values, na.rm = TRUE) * 10 - y_pad * 10) / 10
  upper <- ceiling(max(values, na.rm = TRUE) * 10 + y_pad * 10) / 10
  if (!is.null(y_limits)) {
    lower <- min(y_limits[1], lower, na.rm = TRUE)
    upper <- max(y_limits[2], upper, na.rm = TRUE)
  }
  c(lower, upper)
}
