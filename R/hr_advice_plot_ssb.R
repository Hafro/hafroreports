hr_advice_plot_ssb <- function(
  data_assessment,
  assessment_year,
  ref_points
) {
  lang <- getOption("hr.lang", "en")

  # Create a named color vector
  color_map <- stats::setNames(
    c("darkgreen", "black"),
    c(hr_label("ssb"), hr_label("ref biomass"))
  )

  data_assessment |>
    dplyr::filter(
      key %in% c('SSB', 'refbio'),
      assessment_year == .env$assessment_year
    ) |>
    dplyr::mutate(
      label = eval(as.symbol(paste('label', lang, sep = '.')))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = median / 1000)) +
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
    ggplot2::geom_line(ggplot2::aes(color = label), size = 0.5) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = low / 1000, ymax = high / 1000, fill = label),
      alpha = 0.4
    ) +
    ggplot2::scale_color_manual(values = color_map) +
    ggplot2::scale_fill_manual(values = color_map) +
    ggplot2::geom_hline(
      yintercept = ref_points$MGT_btrigger,
      linetype = "dashed",
      size = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = ref_points$B_lim,
      linetype = "solid",
      size = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = 2008,
      y = ref_points$MGT_btrigger * 1.2,
      label = hr_label("Btrigger"),
      size = 2.5,
      parse = TRUE
    ) +
    ggplot2::annotate(
      "text",
      x = 2012,
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
    ggplot2::scale_y_continuous(
      breaks = seq(0, 250, 50),
      expand = c(0, 0),
      limits = c(0, 275)
    ) +
    hr_astand_theme(legend.position = c(0.275, 0.9)) +
    hr_astand_x_scale(5, 1)
}
