#' Plot a survey biomass index for advice sheet
#'
#' Line plot with confidence band of a survey biomass index in the current
#' assessment (thousand tonnes), with a dashed line for \code{I_trigger}. For
#' index-based (category 3) stocks, whose assessment history holds the
#' survey index as \code{SSB} (as the tidypax-based advice sheets did).
#'
#' @param data_assessment Long-format assessment data as returned by
#'   \code{\link{hr_advice_data_assessment}}, index in tonnes.
#' @param assessment_year Integer. The assessment year to plot.
#' @param ref_points Named list of reference points with \code{I_trigger} in
#'   tonnes, or \code{NULL} for no line.
#' @param key Key of the index in \code{data_assessment}. Default
#'   \code{"SSB"}.
#' @param title Plot title. Default: biomass index.
#' @param index_ab If \code{TRUE}, draw the mean index of the last two years
#'   to \code{assessment_year} (index A) and of the three years before (index
#'   B) as red lines, as in the rfb rule (r = A / B). Default \code{FALSE}.
#' @param index_ab_span How the index A and B lines span the years:
#'   \code{"years"} (default) from the first to the last year of each
#'   period; \code{"periods"} to the half years between the periods, so the
#'   lines meet (index B from \code{assessment_year - 4.5} to
#'   \code{assessment_year - 1.5}, index A from there to
#'   \code{assessment_year}), as the old tidypax \code{three_over_two()}
#'   figures.
#' @param index_ab_colour Colour of the index A and B lines. Default
#'   \code{"red3"}.
#' @param ref_label Plotmath label of the \code{I_trigger} line. Default
#'   \code{hr_label("Itrigger")}; e.g. \code{hr_label("Itrigger_Ilim")} where
#'   I_trigger = I_lim (the shrimp stocks).
#' @param ref_label_year Year (x position) of the label. Default 2005.
#' @param divisor Divisor of the index on the axis. Default 1000 (tonnes to
#'   thousand tonnes); e.g. 1 for an abundance index in millions.
#' @param y_label Axis label. Default thousand tonnes.
#' @param point_years Years drawn as points with error bars instead of the
#'   line and band, e.g. the years of a biennial survey after an annual one
#'   (the Nephrops UWTV index: annual 2016-2021, then 2023, 2025). Default
#'   none.
#' @param year_start First year shown. Default all.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_index <- function(
  data_assessment,
  assessment_year,
  ref_points = NULL,
  key = "SSB",
  title = NULL,
  index_ab = FALSE,
  index_ab_span = c("years", "periods"),
  index_ab_colour = "red3",
  ref_label = NULL,
  ref_label_year = 2005,
  divisor = 1000,
  y_label = hr_label("thousand_tonnes", bold = TRUE),
  point_years = NULL,
  year_start = NULL
) {
  # NSE variables
  year <- median <- low <- high <- label <- .data <- group <- avg <- NULL
  lang <- getOption("hr.lang", "en")
  index_ab_span <- match.arg(index_ab_span)

  d <- data_assessment |>
    dplyr::filter(
      .data$key == .env$key,
      assessment_year == .env$assessment_year
    ) |>
    dplyr::mutate(
      label = as.character(.data[[paste('label', lang, sep = '.')]])
    )

  if (!is.null(year_start)) {
    d <- dplyr::filter(d, year >= year_start)
  }
  d_line <- dplyr::filter(d, !(year %in% point_years))
  d_point <- dplyr::filter(d, year %in% point_years)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = year, y = median / divisor)) +
    ggplot2::geom_ribbon(
      data = d_line,
      ggplot2::aes(ymin = pmax(0, low / divisor), ymax = high / divisor),
      fill = "darkgreen",
      alpha = 0.4
    ) +
    ggplot2::geom_line(data = d_line, color = "darkgreen", linewidth = 0.5) +
    (if (nrow(d_point) > 0) {
      ggplot2::geom_pointrange(
        data = d_point,
        ggplot2::aes(ymin = pmax(0, low / divisor), ymax = high / divisor),
        colour = "darkgreen",
        size = 0.3
      )
    }) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = paste(
          label,
          ':',
          round(median),
          if (divisor == 1000) 't' else '',
          '\n',
          hr_label("year"),
          ':',
          year
        )
      ),
      size = 10,
      hover_nearest = TRUE,
      col = 'white',
      alpha = 0
    )
  itrigger <- ref_points$I_trigger
  if (!is.null(itrigger)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = itrigger / divisor,
        linetype = "dashed",
        linewidth = 0.4
      ) +
      ggplot2::annotate(
        "text",
        x = ref_label_year,
        y = itrigger / divisor * 2,
        label = if (is.null(ref_label)) hr_label("Itrigger") else ref_label,
        size = 2.5,
        parse = TRUE
      )
  }
  if (isTRUE(index_ab) && index_ab_span == "periods") {
    p <- p +
      ggplot2::geom_line(
        data = advice_index_ab_periods(d, assessment_year),
        ggplot2::aes(x = year, y = avg / divisor, group = group),
        colour = index_ab_colour,
        linewidth = 0.75,
        inherit.aes = FALSE
      )
  } else if (isTRUE(index_ab)) {
    avg <- d |>
      dplyr::filter(
        year <= .env$assessment_year,
        year > .env$assessment_year - 5
      ) |>
      dplyr::mutate(group = ifelse(year > .env$assessment_year - 2, "A", "B")) |>
      dplyr::group_by(group) |>
      dplyr::mutate(avg = mean(median)) |>
      dplyr::ungroup()
    p <- p +
      ggplot2::geom_line(
        data = avg,
        ggplot2::aes(x = year, y = avg / divisor, group = group),
        colour = index_ab_colour,
        linewidth = 0.75,
        inherit.aes = FALSE
      )
  }
  p +
    ggplot2::labs(
      title = if (is.null(title)) hr_label("biomass_index", bold = TRUE) else title,
      y = y_label
    ) +
    hr_advice_y_scale() +
    hr_astand_theme(legend.position = "none") +
    hr_astand_x_scale(5, 0)
}

#' Index A and B lines spanning their periods
#'
#' The mean index of the last two years to \code{assessment_year} (index A)
#' and of the three years before (index B), at the years of each period with
#' the first and last moved half a year out to where the periods meet
#' (index B from \code{assessment_year - 4.5} to \code{assessment_year -
#' 1.5}, index A from there to \code{assessment_year}), as the old tidypax
#' \code{three_over_two()}.
#'
#' @param d The index of one assessment (columns \code{year},
#'   \code{median}).
#' @param assessment_year The assessment year.
#' @return A tibble with columns \code{year}, \code{group} (\code{"A"},
#'   \code{"B"}) and \code{avg}.
#' @noRd
advice_index_ab_periods <- function(d, assessment_year) {
  # NSE variables
  year <- median <- group <- avg <- NULL

  tyr <- assessment_year
  d |>
    dplyr::filter(year > tyr - 5, year <= tyr) |>
    dplyr::mutate(group = ifelse(year > tyr - 2, "A", "B")) |>
    dplyr::group_by(group) |>
    dplyr::mutate(avg = mean(median, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      year = dplyr::case_when(
        year == tyr - 2 ~ tyr - 2 + 0.5,
        year == tyr - 1 ~ tyr - 1 - 0.5,
        year == tyr - 4 ~ tyr - 4 - 0.5,
        TRUE ~ year
      )
    ) |>
    dplyr::select(year, group, avg)
}
