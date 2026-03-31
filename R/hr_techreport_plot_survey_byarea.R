# Was survey_by_area_plot
#' Plot survey biomass by geographic region (total and stacked)
#'
#' Computes survey biomass indices for both the spring (SMB) and autumn (SMH)
#' groundfish surveys, broken down by geographic region, and produces a
#' two-panel plot per survey showing total biomass and regional proportions
#' over time.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param regions Named list mapping region labels to integer MFDB area codes.
#'   Default regions are W (101), NW (102), NE (103–105), SE (106–107),
#'   SW (108), and Other (all remaining).
#' @return A \code{ggplot2} / \code{patchwork} plot object split by survey.
#' @export
hr_techreport_plot_survey_byarea <- function(
  pcon,
  regions = list(
    W = 101,
    NW = 102,
    NE = c(103, 104, 105),
    SE = c(107, 106),
    SW = 108,
    Other = pax::pax_add_other()
  )
) {
  # NSE variables
  year <- region <- si_biomass <- mfdb_gear_code <- val <- group <- sampling_type <- NULL

  make_si <- function(survey) {
    stratification <- if (survey == "smb") {
      "new_strata_spring"
    } else {
      "new_strata_autumn"
    }
    sampling_type <- if (survey == "smb") 30 else 35
    tow_number <- if (survey == "smb") 0:35 else 0:75
    skip_years <- if (survey == "smb") c() else c(2011)

    dplyr::tbl(pcon, "station") |>
      dplyr::filter(
        sampling_type == .env$sampling_type,
        is.na(tow_number) | tow_number %in% .env$tow_number,
        !(year %in% .env$skip_years)
      ) |>
      pax::pax_si_by_length() |>
      pax::pax_si_scale_by_strata(stratification) |>
      pax::pax_add_regions(
        regions = regions |>
          stats::setNames(sapply(
            c("W", "NW", "NE", "SE", "SW", "other"),
            hr_label
          ))
      ) |>
      dplyr::group_by(year, sampling_type, region) |>
      dplyr::summarize(si_biomass = sum(si_biomass))
  }

  make_si("smb") |>
    dplyr::union(make_si("smh")) |>
    dplyr::rename(
      group = region
    ) |>
    dplyr::mutate(
      # NB: This isn't a gear code, it's the plot split
      mfdb_gear_code = ifelse(
        sampling_type == 30,
        local(hr_label("spring_survey")),
        local(hr_label("autumn_survey"))
      ),
      val = si_biomass / 1e3
    ) |>
    two_panel_plot(
      y = hr_label("survey_biomass"),
      total.text = '%s',
      cols = c(
        "#999999",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
      ),
      split_by_gear = TRUE
    )
}
