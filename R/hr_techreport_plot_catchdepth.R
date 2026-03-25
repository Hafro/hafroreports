hr_techreport_plot_catchdepth <- function(
  pcon,
  year_start = max(year_start, year_end - 22)
) {
  lang <- getOption("hr.lang", "en")

  catch_by_location <- dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(year <= year_start) |>
    dplyr::group_by(year, lat = round(lat, 1), lon = round(lon, 1)) |>
    dplyr::summarise(
      catch = sum(1e-3 * catch / tow_area, na.rm = TRUE),
      tow_time = sum(tow_time / tow_area, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  out <- pax::pax_map_base() |>
    pax::pax_map_layer_depth(dplyr::tbl(pcon, "ocean_depth")) |>
    pax::pax_map_layer_catch(
      catch_by_location,
      alpha = 1,
      na.fill = -50,
      breaks = c(0, 1, 2, seq(3, 20, by = 3), 40, 60)
    )
  if (lang == "is") {
    out <- out + ggplot2::labs(fill = 'Afli (t/nm2)')
  }
  return(out)
}
