hr_techreport_plot_lbindex <- function(
  pcon,
  length_range,
  var = "si_biomass"
) {
  make_si <- function(survey, length_range) {
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
      pax::pax_si_strata_summary(length_range = length_range) |>
      pax::pax_si_year_summary() |>
      dplyr::mutate(
        var = !!substitute(x / 1e3, list(x = as.symbol(var))),
        var_cv = !!as.symbol(paste0(var, "_cv"))
      ) |>
      dplyr::select(year, var, var_cv) |>
      dplyr::collect()
  }

  make_si("smb", length_range) |>
    ggplot2::ggplot(ggplot2::aes(year, var)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = var * (1 - 1.96 * var_cv),
        ymax = var * (1 + 1.96 * var_cv)
      ),
      fill = 'grey'
    ) +
    ggplot2::geom_line() +
    ggplot2::theme_light() +
    ggplot2::labs(
      x = '',
      y = paste('Biomass', paste(length_range, collapse = ".."))
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_pointrange(
      data = make_si("smh", length_range),
      ggplot2::aes(
        year,
        var,
        ymax = var * (1 + 1.96 * var_cv),
        ymin = var * (1 - 1.96 * var_cv)
      )
    )
}
