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
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_index <- function(
  data_assessment,
  assessment_year,
  ref_points = NULL,
  key = "SSB",
  title = NULL
) {
  # NSE variables
  year <- median <- low <- high <- label <- .data <- NULL
  lang <- getOption("hr.lang", "en")

  d <- data_assessment |>
    dplyr::filter(
      .data$key == .env$key,
      assessment_year == .env$assessment_year
    ) |>
    dplyr::mutate(
      label = as.character(.data[[paste('label', lang, sep = '.')]])
    )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = year, y = median / 1000)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmax(0, low / 1000), ymax = high / 1000),
      fill = "darkgreen",
      alpha = 0.4
    ) +
    ggplot2::geom_line(color = "darkgreen", linewidth = 0.5) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = paste(
          label,
          ':',
          round(median),
          't',
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
        yintercept = itrigger / 1000,
        linetype = "dashed",
        linewidth = 0.4
      ) +
      ggplot2::annotate(
        "text",
        x = 2005,
        y = itrigger / 1000 * 2,
        label = hr_label("Itrigger"),
        size = 2.5,
        parse = TRUE
      )
  }
  p +
    ggplot2::labs(
      title = if (is.null(title)) hr_label("biomass_index", bold = TRUE) else title,
      y = hr_label("thousand_tonnes", bold = TRUE)
    ) +
    hr_advice_y_scale() +
    hr_astand_theme(legend.position = "none") +
    hr_astand_x_scale(5, 0)
}
