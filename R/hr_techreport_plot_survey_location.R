# Was survey_location_plot
#' Map survey station locations with catch density
#'
#' Creates a map of survey station locations (crosses) with the catch density
#' (kg/nm, proportional circles) for the spring (SMB) and autumn (SMH)
#' groundfish surveys in the assessment year. Zero-catch stations are shown
#' as grey crosses; non-zero stations have red circles proportional to
#' biomass per nautical mile of tow.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param assessment_year Integer. The SMB data from this year and the SMH
#'   data from the previous year are displayed.
#' @return A \code{ggplot2} plot object with two facets (SMB and SMH).
#' @export
hr_techreport_plot_survey_location <- function(
  pcon,
  assessment_year
) {
  # NSE variables
  sampling_type <- year <- lat <- lon <- bio <- zero_station <- survey <- NULL
  species <- sample_id <- count <- weight <- tow_length <- NULL
  begin_lat <- begin_lon <- NULL
  coalesce <- NULL

  dat <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(
      sampling_type == 30 &&
        year == .env$assessment_year |
        sampling_type == 35 && year == (.env$assessment_year - 1)
    ) |>
    dplyr::left_join(
      dplyr::tbl(pcon, "ldist") |>
        pax::pax_ldist_scale_round() |>
        pax::pax_ldist_scale_abund()
    ) |>
    pax::pax_ldist_add_weight() |>
    dplyr::mutate(lat = round(begin_lat, 1), lon = round(begin_lon, 1)) |>
    dplyr::group_by(sample_id, lat, lon, year, sampling_type, species) |>
    dplyr::summarize(
      bio = sum(
        abs(coalesce(count, 0) * weight) /
          abs(coalesce(tow_length, 4)),
        na.rm = TRUE
      ) /
        1e5
    ) |>
    dplyr::mutate(zero_station = ifelse(bio == 0, 'Zero catch', 'Non zero')) |>
    dplyr::ungroup()

  pax::pax_map_base() |>
    pax::pax_map_layer_depth(dplyr::tbl(pcon, "ocean_depth")) +
    ggplot2::geom_point(
      ggplot2::aes(lon, lat),
      pch = 4,
      data = dat,
      col = 'gray'
    ) +
    ggplot2::geom_point(
      ggplot2::aes(lon, lat, size = bio),
      col = 'red',
      data = dat,
      alpha = 0.4
    ) +
    ggplot2::scale_size_area() +
    ggplot2::labs(size = "kg/nm") +
    ggplot2::facet_wrap(~sampling_type, ncol = 2) +
    ggplot2::geom_label(
      x = -18.5,
      y = 65,
      ggplot2::aes(label = survey),
      data = tibble::tibble(sampling_type = c(30, 35), survey = c('SMB', 'SMH'))
    )
}
