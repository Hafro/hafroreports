hr_techreport_plot_numboats <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
  lang <- getOption("hr.lang", "en")

  tbl <- dplyr::tbl(pcon, "landings") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_landings_significantboats_summary()

  limits <- tbl |>
    dplyr::arrange() |>
    dplyr::summarize(year_min = min(year), year_max = max(year)) |>
    dplyr::collect()
  limits <- as.integer(c(limits[1, "year_min"], limits[1, "year_max"]))

  # Collect every 5 years from table
  breaks <- tbl |>
    dplyr::distinct(year) |>
    dplyr::filter(year %% 5 == 0) |>
    dplyr::pull(year)
  breaks <- c(breaks, max(breaks) + 5) # Add back on topmost year

  p1 <-
    ggplot2::ggplot(tbl, ggplot2::aes(catch, n, label = year)) +
    ggplot2::geom_path(colour = 4, linetype = 1, alpha = 0.5) +
    ggplot2::geom_text(
      hjust = 0,
      nudge_x = 0.05,
      check_overlap = TRUE,
      size = 3
    ) +
    ggplot2::theme_light() +
    ggplot2::expand_limits(y = 0, x = 0) +
    #  ylim(c(0,250))+
    #  xlim(4000,12500)+
    ggplot2::labs(
      y = "",
      x = hr_label("catch_tonnes"),
      color = hr_label("year")
    ) +
    ggplot2::theme(
      legend.position = c(0.9, 0.3),
      legend.title = ggplot2::element_text(size = 5),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.5))
    )

  p2 <- ggplot2::ggplot(tbl, ggplot2::aes(year, n)) +
    ggplot2::geom_line(col = 4) +
    ggplot2::scale_x_continuous(limits = limits, breaks = breaks) +
    #  ylim(0,250)+
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      x = hr_label("year"),
      y = c(
        en = "Number of vessles accounting for 95% of catch",
        is = "Fjöldi báta sem veiða 95 % af heildarafla"
      )[[lang]]
    ) +
    ggplot2::theme_light()

  patchwork::wrap_plots(p2, p1)
}
