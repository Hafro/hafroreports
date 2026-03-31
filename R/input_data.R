#' Extract length–weight data from a pax database
#'
#' Queries the \code{station} and \code{aldist} tables in a pax database
#' connection to obtain individual length and weight measurements. When
#' \code{prediction_length_range} is supplied, a GAM is fitted to the
#' observed data and predicted weights are returned instead of raw
#' observations.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param sampling_type Integer vector of sampling type codes to include.
#'   Default is \code{30} (spring groundfish survey).
#' @param prediction_length_range Numeric vector of lengths at which to
#'   predict weight using a GAM. If \code{NULL} (the default), the raw
#'   observed data are returned.
#' @return A tibble with columns \code{species}, \code{length}, and
#'   \code{weight}. If \code{prediction_length_range} is provided, each row
#'   corresponds to a predicted weight at the specified length.
#' @export
hr_input_data_lw <- function(
  pcon,
  sampling_type = 30,
  prediction_length_range = NULL
) {
  # NSE variables
  species <- length <- weight <- count <- NULL

  lw_dat <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% local(sampling_type)) |>
    dplyr::left_join(
      # NB: We really need unaggregated length/weight here, not aggregated by age.
      #     https://github.com/Hafro/pax/issues/17
      pax::pax_temptbl(
        pcon,
        dplyr::tbl(pcon, "aldist") |>
          dplyr::collect() |>
          tidyr::uncount(weights = count)
      ),
      by = c('sample_id')
    ) |>
    dplyr::filter(!is.na(length), weight > 0) |>
    dplyr::select(species, length, weight) |>
    dplyr::collect(n = Inf)

  if (!is.null(prediction_length_range)) {
    # NB: Using gam::s inside the formula results in inflated predictions
    s <- gam::s
    lw_dat <-
      modelr::add_predictions(
        tibble::tibble(
          species = lw_dat$species[[1]],
          length = prediction_length_range
        ),
        gam::gam(
          weight ~ s(log(length), df = 8),
          family = stats::Gamma(link = log),
          data = lw_dat
        ),
        var = 'weight'
      ) |>
      dplyr::mutate(weight = as.numeric(exp(weight)))
  }
  return(lw_dat)
}

#' Estimate maturity-at-length key from survey data
#'
#' Builds a maturity ogive by fitting a quasi-binomial GLM to maturity
#' observations from the \code{measurement} table, grouped by length class
#' and region. Observed proportions mature (by year, length group, age,
#' and region) are combined with model-predicted proportions (with
#' \code{year = NA} and \code{age = NA}) in the output, so downstream
#' code can distinguish measurements from estimates.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param lgroups Numeric vector of length group break points (lower bounds).
#'   Default is \code{seq(0, 200, 5)}.
#' @param regions Named list mapping region labels to integer MFDB area
#'   codes. If \code{NULL}, all stations are treated as one region
#'   (\code{"all"}). Default is \code{NULL}.
#' @param ignore_years Integer vector of years to exclude from model fitting.
#'   Default is \code{c()} (no years excluded).
#' @param sampling_type Integer vector of sampling type codes to include.
#'   Default is \code{30}.
#' @return A tibble with columns \code{year}, \code{lgroup}, \code{age},
#'   \code{region}, and \code{mat_p}. Rows with \code{year = NA} are
#'   model predictions.
#' @export
hr_input_data_maturity_key <- function(
  pcon,
  lgroups = seq(0, 200, 5),
  regions = NULL,
  ignore_years = c(),
  sampling_type = 30
) {
  # NSE variables
  measurement_type <- age <- maturity_stage <- mat <- year <- lgroup <- region <- mat_p <- NULL

  mat_length <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% local(sampling_type)) |>
    dplyr::inner_join(
      dplyr::tbl(pcon, "measurement") |>
        dplyr::filter(
          measurement_type == "OTOL",
          !is.na(age),
          !is.na(maturity_stage)
        ) |>
        dplyr::mutate(mat = ifelse(maturity_stage == 1, 0, 1))
    ) |>
    pax::pax_add_lgroups(lgroups = lgroups) |>
    pax::pax_add_regions(regions = regions)

  mat_model <-
    mat_length |>
    dplyr::group_by(year, lgroup, region) |>
    dplyr::summarise(mat_p = mean(mat)) |>
    dplyr::collect(n = Inf) |>
    na.omit() |>
    dplyr::filter(!(year %in% local(ignore_years))) |>
    stats::glm(
      mat_p ~ log(lgroup) * region,
      data = _,
      family = stats::quasi(variance = "mu(1-mu)", link = "logit")
    )

  mat_filler <- expand.grid(
    lgroup = lgroups,
    region = (if (is.null(regions)) 'all' else unique(names(regions)))
  ) |>
    dplyr::filter(lgroup > 0) |>
    modelr::add_predictions(mat_model, type = 'response', var = 'mat_p')

  # Combine measurements & estimates, with year/age = NA signifying the estimates
  return(dplyr::union_all(
    mat_length |>
      dplyr::group_by(year, lgroup, age, region) |>
      dplyr::summarise(mat_p = mean(mat)) |>
      dplyr::collect(),
    mat_filler |> dplyr::mutate(year = NA, age = NA)
  ))
}

## Generate the ALK from the survey
#' Compute survey index data (abundance and biomass at age)
#'
#' Derives age-structured survey abundance (thousands) and mean weight (g)
#' from a pax database by applying a length distribution, an age–length key,
#' optional strata scaling, and optional maturity weighting. The result is
#' the primary model input table used by the SAM and MUPPET assessment
#' workflows.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param lw_key Data frame with columns \code{species}, \code{length}, and
#'   \code{weight} for joining weight-at-length. If \code{NULL}, weights are
#'   derived directly from the \code{ldist} table.
#' @param maturity_key Output of \code{\link{hr_input_data_maturity_key}}, used
#'   to compute maturity-weighted biomass. If \code{NULL}, no maturity column
#'   is produced.
#' @param strata_name Character. Name of the stratification scheme to use for
#'   survey scaling (passed to \code{pax::pax_si_scale_by_strata}). If
#'   \code{NULL}, no strata scaling is applied.
#' @param sampling_type Integer vector of sampling type codes for station
#'   filtering. Default is \code{30}.
#' @param sam_use_10_11_first_2_years Logical. If \code{TRUE}, sampling types
#'   10 and 11 are additionally included for the first two years of data to
#'   improve age-1 estimates. Default is \code{FALSE}.
#' @param tow_number Integer vector of valid tow numbers (NA coerced to 0).
#'   Default is \code{0:35}.
#' @param tgroup Integer or \code{NULL}. Tow group for length-distribution
#'   scaling. Default is \code{NULL}.
#' @param regions Named list mapping region labels to integer MFDB area codes.
#'   Default is \code{list(all = 101:115)}.
#' @param lgroups Numeric vector of length group break points. Default is
#'   \code{seq(0, 200, 5)}.
#' @param gear_group Named list mapping gear group labels to MFDB gear codes.
#'   Default groups are \code{Other}, \code{BMT}, \code{LLN}, and \code{DSE}.
#' @param gear_id_filter Integer vector of gear IDs to include, or \code{NULL}
#'   for no filtering. Default is \code{NULL}.
#' @param scale_by_landings Logical. If \code{TRUE}, indices are additionally
#'   scaled to match total landings. Default is \code{FALSE}.
#' @return A grouped tibble with columns \code{year}, \code{age}, \code{n}
#'   (abundance in thousands), \code{mw} (mean weight in grams), and
#'   optionally \code{mat} (proportion mature).
#' @export
hr_input_data_si_index <- function(
  pcon,
  lw_key = NULL,
  maturity_key = NULL,
  strata_name = NULL,
  sampling_type = 30,
  sam_use_10_11_first_2_years = FALSE,
  tow_number = 0:35,
  tgroup = NULL,
  regions = list(all = 101:115),
  lgroups = seq(0, 200, 5),
  gear_group = list(
    Other = 'Var',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT'),
    LLN = 'LLN',
    DSE = c('PSE', 'DSE')
  ),
  gear_id_filter = NULL,
  scale_by_landings = FALSE
) {
  # NSE variables
  si_abund <- si_biomass <- mat_p <- mat_p_est <- year <- age <- NULL
  coalesce <- gear_id <- NULL

  ldist <- dplyr::tbl(pcon, "ldist")
  if (!is.null(lw_key)) {
    ldist <- dplyr::left_join(
      ldist,
      pax::pax_temptbl(pcon, lw_key),
      by = c("species", 'length')
    )
  } else {
    ldist <- pax::pax_ldist_add_weight(ldist)
  }

  alk <- dplyr::tbl(pcon, "station")

  if (isTRUE(sam_use_10_11_first_2_years)) {
    # NB: SAM is sensitive to the first 2 years in age 1, use sampling_types 10 & 11 to increase reported data
    start_year <- dplyr::tbl(pcon, "station") |>
      dplyr::summarise(year = min(year, na.rm = TRUE)) |>
      dplyr::pull(year)
    alk <- dplyr::filter(
      alk,
      sampling_type %in%
        local(sampling_type) |
        (year < local(start_year + 2) & (sampling_type %in% 10:11))
    )
  } else {
    alk <- dplyr::filter(alk, sampling_type %in% local(sampling_type))
  }

  alk <- alk |>
    dplyr::filter(
      coalesce(tow_number, 0) %in% local(tow_number),
      local(is.null(gear_id_filter)) | (gear_id %in% local(gear_id_filter))
    ) |>
    pax::pax_ldist_alk(
      lgroups = lgroups,
      tgroup = tgroup,
      regions = regions,
      gear_group = gear_group
    )

  at_age <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(
      sampling_type %in% local(sampling_type),
      coalesce(tow_number, 0) %in% local(tow_number)
    ) |>
    pax::pax_si_by_length(ldist = ldist)
  if (!is.null(strata_name)) {
    at_age <- pax::pax_si_scale_by_strata(at_age, strata_name)
  }
  if (!is.null(alk)) {
    at_age <- pax::pax_si_scale_by_alk(
      at_age,
      lgroups = lgroups,
      tgroup = tgroup,
      regions = regions,
      gear_group = gear_group,
      alk = alk
    )
  }
  if (isTRUE(scale_by_landings)) {
    at_age <- pax::pax_si_scale_by_landings(
      at_age,
      tgroup = tgroup,
      regions = regions,
      gear_group = gear_group
    )
  }

  if (!is.null(maturity_key)) {
    # Break apart measurements & filler, join both separately
    mat_measurements <- maturity_key |> dplyr::filter(!is.na(year))
    mat_filler <- maturity_key |>
      dplyr::filter(is.na(year)) |>
      dplyr::select(-year, -age) |>
      dplyr::rename(mat_p_est = mat_p)
    at_age <- at_age |>
      dplyr::left_join(
        pax::pax_temptbl(pcon, mat_measurements),
        by = c("year", "lgroup", "age", "region")
      ) |>
      dplyr::left_join(
        pax::pax_temptbl(pcon, mat_filler),
        by = c("lgroup", "region")
      )

    mat_c <- quote(sum(si_abund * coalesce(mat_p, mat_p_est)) / sum(si_abund))
  } else {
    mat_c <- NA
  }

  out <- at_age |>
    dplyr::group_by(year, age) |>
    dplyr::summarise(
      n = sum(si_abund) / 1000,
      mw = 1000 * sum(si_biomass) / sum(si_abund),
      mat = {{ mat_c }}
    )
  return(out)
}

#' Aggregate total landings by year from a pax database
#'
#' Queries the \code{landings} table and returns the sum of \code{catch}
#' for each year.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @return A lazy tibble (or tibble after collection) with columns \code{year}
#'   and \code{catch} (total catch in the units stored in the database).
#' @export
hr_input_data_landings <- function(pcon) {
  # NSE variables
  year <- catch <- NULL

  dplyr::tbl(pcon, "landings") |>
    dplyr::group_by(year) |>
    dplyr::summarize(catch = sum(catch))
}
