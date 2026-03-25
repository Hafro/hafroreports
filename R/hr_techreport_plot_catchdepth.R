hr_techreport_plot_catchdepth <- function(
  pcon,
  year_start = max(year_start, year_end - 22)
) {
  lang <- getOption("hr.lang", "en")

  out <- pax::pax_map_base() |>
    pax::pax_map_layer_depth(dplyr::tbl(pcon, "ocean_depth")) |>
    pax::pax_map_layer_catch(
      hr_catch_by_location(pcon, year_start),
      alpha = 1,
      na.fill = -50,
      breaks = c(0, 1, 2, seq(3, 20, by = 3), 40, 60)
    )
  if (lang == "is") {
    out <- out + ggplot2::labs(fill = 'Afli (t/nm2)')
  }
  return(out)
}
