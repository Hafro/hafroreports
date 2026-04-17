#' The Ástand theme
#' @name hr_astand_theme
#' @description Ástand functions
#' @param ... input to ggplot theme
#' @return ggplot2 theme for the Ástand
hr_astand_theme <- function(...) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(size = 9, hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 6),
      legend.key.size = ggplot2::unit(0.25, 'cm'),
      legend.key = ggplot2::element_rect(colour = NA),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(colour = "black"),
      axis.text.y = ggplot2::element_text(colour = "black"),
      panel.grid.major = ggplot2::element_line(colour = "grey80", size = 0.25),
      panel.grid.minor = ggplot2::element_line(colour = "grey80", size = 0.25),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.text.align = 0,
      ...
    )
}

#' Astand x scale
#' @name hr_astand_x_scale
#' @param x break point
#' @param tyr terminal year
#' @param ... Further input to ggplot2::scale_x_continuous()
hr_astand_x_scale <- function(x, tyr = lubridate::year(Sys.Date()), ...) {
  if ((tyr %% 2) == 0) {
    ggplot2::scale_x_continuous(
      breaks = seq(1900, 2100, x),
      expand = c(0, 0.5),
      ...
    )
  } else {
    ggplot2::scale_x_continuous(
      breaks = seq(1901, 2101, x),
      expand = c(0, 0.5),
      ...
    )
  }
}

#' @title Print number with a thousand separator
#' @name hr_red_dot_number
#' @param number the number you want to convert
hr_red_dot_number <- function(number) {
  lang <- getOption("hr.lang", "en")

  prettyNum(
    number,
    big.mark = " ",
    decimal.mark = if (lang == "is") "," else ".",
    digits = 10
  )
}
