hr_techreport_plot_catchspatial <- function(
  pcon,
  years,
  low_res = TRUE
) {
  # NSE variables
  year <- lat <- lon <- catch <- tow_area <- tow_time <- NULL
  lang <- getOption("hr.lang", "en")

  catch_by_location <- dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(year %in% years) |>
    dplyr::group_by(year, lat = round(lat, 1), lon = round(lon, 1)) |>
    dplyr::summarise(
      catch = sum(1e-3 * catch / tow_area, na.rm = TRUE),
      tow_time = sum(tow_time / tow_area, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  out <- pax::pax_map_base(low_res = low_res) |>
    pax::pax_map_layer_depth(dplyr::tbl(pcon, "ocean_depth")) |>
    pax::pax_map_layer_catch(
      catch_by_location,
      alpha = 1,
      na.fill = -50,
      breaks = c(0, 1, 2, seq(3, 20, by = 3), 40, 60)
    ) +
    hr_theme_crayola_fill() +
    ggplot2::theme(legend.position = c(0.8, 0.2))
  if (lang == "is") {
    out <- out + ggplot2::labs(fill = 'Afli (t/nm2)')
  }
  return(out)
}
