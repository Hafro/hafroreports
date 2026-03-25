hr_techreport_plot_sampling_position <- function(
  pcon,
  mfdb_gear_codes = c('LLN', 'DSE', 'BMT'),
  assessment_year
) {
  sampling_pos <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    dplyr::mutate(lon = round(lon, 1), lat = round(lat, 1)) |>
    dplyr::select(lat, lon, year, mfdb_gear_code) |>
    dplyr::distinct() |>
    dplyr::collect()

  catch_by_gear <-
    dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(
      year == local(assessment_year - 1),
      mfdb_gear_code %in% mfdb_gear_codes
    ) |>
    dplyr::mutate(lon = round(lon, 1), lat = round(lat, 1)) |>
    dplyr::group_by(year, mfdb_gear_code, lon, lat) |>
    dplyr::summarise(
      catch = sum(1e-3 * catch / tow_area, na.rm = TRUE),
      tow_time = sum(tow_time / tow_area, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  pax::pax_map_base() |>
    pax::pax_map_layer_depth(dplyr::tbl(pcon, "ocean_depth")) |>
    pax::pax_map_layer_catch(
      catch_by_gear,
      annotation = 'year', # TODO: gear?
      na.fill = -50
    ) +
    ggplot2::geom_point(ggplot2::aes(lon, lat), data = sampling_pos, pch = 4)
}
