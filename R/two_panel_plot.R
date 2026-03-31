#' Two panel plot
#'
#' @param dat input data frame
#' @param prop.text Proportion text
#' @param total.text total text
#' @param y type of data label
#' @param x Time label
#' @param fill Fill label
#' @param cols fill colours
#'
#' @return ggplot object
two_panel_plot <- function(
  dat,
  prop.text = hr_label("prop_of_measure"),
  total.text = '%s (kt)',
  y = hr_label("catches"),
  x = hr_label("year"),
  fill = '',
  cols = c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    'black'
  ),
  split_by_gear = FALSE
) {
  # NSE variables
  year <- val <- group <- mfdb_gear_code <- NULL
  p1 <-
    dat |>
    ggplot2::ggplot(ggplot2::aes(year, val, fill = group)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(angle = 90),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = 'top'
    ) +
    ggplot2::labs(y = sprintf(total.text, y), fill = fill) +
    ggplot2::scale_fill_manual(values = cols)

  p2 <-
    dat |>
    ggplot2::ggplot(ggplot2::aes(year, val, fill = group)) +
    ggplot2::geom_bar(stat = 'identity', position = 'fill') +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(angle = 90),
      legend.position = " "
    ) +
    ggplot2::labs(y = sprintf(prop.text, y), x = x) +
    ggplot2::scale_fill_manual(values = cols)

  if (split_by_gear) {
    p1 <- p1 + ggplot2::facet_wrap(~mfdb_gear_code, nrow = 1)
    p2 <- p2 + ggplot2::facet_wrap(~mfdb_gear_code, nrow = 1)
  }

  patchwork::wrap_plots(
    p1,
    p2,
    ncol = 1
  )
}
