#' Plot spawning stock biomass for advice sheet
#'
#' Line plot with confidence bands of SSB and, if the stock has one, the
#' reference biomass in the current assessment (thousand tonnes), with lines
#' for MGT Btrigger and Blim. Series without values (e.g. the reference
#' biomass of a stock with F-based advice) are left out.
#'
#' @param data_assessment Long-format assessment data as returned by
#'   \code{\link{hr_advice_data_assessment}}.
#' @param assessment_year Integer. The assessment year to plot.
#' @param ref_points Named list of reference points (biomass in thousand
#'   tonnes): \code{MGT_btrigger} (or \code{MSY_btrigger} if there is no
#'   management plan), \code{B_lim}, \code{B_pa}.
#' @param refbio_label Text added to the reference biomass legend entry, e.g.
#'   \code{"(B4+)"}. Default \code{NULL}.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_ssb <- function(
  data_assessment,
  assessment_year,
  ref_points,
  refbio_label = NULL
) {
  # NSE variables
  key <- year <- median <- label <- low <- high <- .data <- NULL
  lang <- getOption("hr.lang", "en")

  d <- data_assessment |>
    dplyr::filter(
      key %in% c('SSB', 'refbio'),
      assessment_year == .env$assessment_year
    ) |>
    dplyr::group_by(key) |>
    dplyr::filter(any(!is.na(median))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label = as.character(.data[[paste('label', lang, sep = '.')]])
    )
  # Colours keyed by the labels in the data
  labels <- dplyr::distinct(d, key, label)
  colours <- c(SSB = "darkgreen", refbio = "black")[labels$key]
  names(colours) <- labels$label
  legend_labels <- stats::setNames(
    ifelse(
      labels$key == "refbio" & !is.null(refbio_label),
      paste(labels$label, refbio_label),
      labels$label
    ),
    labels$label
  )

  # MGT Btrigger if there is a management plan, else MSY Btrigger
  if (!is.null(ref_points$MGT_btrigger)) {
    btrigger <- ref_points$MGT_btrigger
    btrigger_label <- "Btrigger"
  } else {
    btrigger <- ref_points$MSY_btrigger
    btrigger_label <- "MSYBtrigger"
  }

  ggplot2::ggplot(d, ggplot2::aes(x = year, y = median / 1000)) +
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
    ) +
    ggplot2::geom_line(ggplot2::aes(color = label), linewidth = 0.5) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = low / 1000, ymax = high / 1000, fill = label),
      alpha = 0.4
    ) +
    ggplot2::scale_color_manual(values = colours, labels = legend_labels) +
    ggplot2::scale_fill_manual(values = colours, labels = legend_labels) +
    ggplot2::geom_hline(
      yintercept = btrigger,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = ref_points$B_lim,
      linetype = "solid",
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = 2008,
      y = btrigger * 1.2,
      label = hr_label(btrigger_label),
      size = 2.5,
      parse = TRUE
    ) +
    ggplot2::annotate(
      "text",
      # Clear of the Btrigger label when they are at the same level
      x = if (isTRUE(all.equal(btrigger, ref_points$B_pa))) 2017 else 2012,
      y = ref_points$B_pa * 1.2,
      label = hr_label("Bpa"),
      size = 2.5,
      parse = TRUE
    ) +
    ggplot2::annotate(
      "text",
      x = 2008,
      y = ref_points$B_lim * 1.2,
      label = hr_label("Blim"),
      size = 2.5,
      parse = TRUE
    ) +
    ggplot2::labs(
      title = hr_label("biomass", bold = TRUE),
      y = hr_label("thousand_tonnes", bold = TRUE)
    ) +
    hr_advice_y_scale() +
    hr_astand_theme(
      legend.position = if (nrow(labels) > 1) c(0.275, 0.9) else "none"
    ) +
    hr_astand_x_scale(5, 1)
}
