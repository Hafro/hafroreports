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
#' (and region, and optionally year). Observed proportions mature (by year,
#' length group, age, and region) are combined with model-predicted
#' proportions in the output. Predictions are marked by \code{age = NA}, and
#' have \code{year = NA} unless \code{by_year = TRUE}, so downstream code can
#' distinguish measurements from estimates.
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
#' @param by_year Logical. If \code{TRUE}, the model includes year as a factor
#'   (\code{mat_p ~ log(lgroup) + as.factor(year)}) and predictions are made
#'   per year. Otherwise the model is \code{mat_p ~ log(lgroup) * region}
#'   (\code{mat_p ~ log(lgroup)} with a single region). Default is
#'   \code{FALSE}.
#' @param sex Integer sex code to restrict the observations to (e.g. \code{2}
#'   for females), or \code{NULL} for all fish. Default is \code{NULL}.
#' @param mature_above,immature_below Length (cm). Before fitting, proportions
#'   mature in length groups above \code{mature_above} are set to 1 and below
#'   \code{immature_below} to 0. Default \code{NULL} (not used).
#' @param predict_lgroups Length groups to predict for. Default is all
#'   \code{lgroups} above 0.
#' @param predict_years Years to predict for when \code{by_year = TRUE}.
#'   Default \code{NULL} is all years in the data; years without data are
#'   dropped.
#' @param copy_years Named vector to fill years without data from another
#'   year's predictions, e.g. \code{c("1985" = 1987, "1986" = 1987)}.
#'   Only with \code{by_year = TRUE}. Default \code{NULL}.
#' @return A tibble with columns \code{year}, \code{lgroup}, \code{age},
#'   \code{region}, and \code{mat_p}. Rows with \code{age = NA} are
#'   model predictions.
#' @export
hr_input_data_maturity_key <- function(
  pcon,
  lgroups = seq(0, 200, 5),
  regions = NULL,
  ignore_years = c(),
  sampling_type = 30,
  by_year = FALSE,
  sex = NULL,
  mature_above = NULL,
  immature_below = NULL,
  predict_lgroups = lgroups[lgroups > 0],
  predict_years = NULL,
  copy_years = NULL
) {
  # NSE variables
  measurement_type <- age <- maturity_stage <- mat <- year <- lgroup <- region <- mat_p <- NULL

  measurements <- dplyr::tbl(pcon, "measurement") |>
    dplyr::filter(
      measurement_type == "OTOL",
      !is.na(age),
      !is.na(maturity_stage)
    )
  if (!is.null(sex)) {
    measurements <- dplyr::filter(measurements, sex %in% local(sex))
  }
  mat_length <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% local(sampling_type)) |>
    dplyr::inner_join(
      measurements |>
        dplyr::mutate(mat = ifelse(maturity_stage == 1, 0, 1))
    ) |>
    pax::pax_add_lgroups(lgroups = lgroups)
  mat_length <- if (is.null(regions)) {
    dplyr::mutate(mat_length, region = 'all')
  } else {
    pax::pax_add_regions(mat_length, regions = regions)
  }
  region_names <- if (is.null(regions)) 'all' else unique(names(regions))

  mat_dat <-
    mat_length |>
    dplyr::group_by(year, lgroup, region) |>
    dplyr::summarise(mat_p = mean(mat)) |>
    dplyr::collect(n = Inf) |>
    dplyr::ungroup()
  if (!is.null(mature_above)) {
    mat_dat <- dplyr::mutate(mat_dat, mat_p = ifelse(lgroup > mature_above, 1, mat_p))
  }
  if (!is.null(immature_below)) {
    mat_dat <- dplyr::mutate(mat_dat, mat_p = ifelse(lgroup < immature_below, 0, mat_p))
  }
  mat_dat <- mat_dat |>
    stats::na.omit() |>
    dplyr::filter(lgroup > 0, !(year %in% local(ignore_years)))

  formula <- if (isTRUE(by_year)) {
    mat_p ~ log(lgroup) + as.factor(year)
  } else if (length(region_names) > 1) {
    mat_p ~ log(lgroup) * region
  } else {
    mat_p ~ log(lgroup)
  }
  mat_model <- stats::glm(
    formula,
    data = mat_dat,
    family = stats::quasi(variance = "mu(1-mu)", link = "logit")
  )

  if (isTRUE(by_year)) {
    model_years <- sort(unique(mat_dat$year))
    if (!is.null(predict_years)) {
      model_years <- intersect(model_years, predict_years)
    }
    mat_filler <- tidyr::expand_grid(
      year = model_years,
      lgroup = predict_lgroups,
      region = region_names
    ) |>
      modelr::add_predictions(mat_model, type = 'response', var = 'mat_p')
    for (target in names(copy_years)) {
      mat_filler <- dplyr::bind_rows(
        mat_filler,
        mat_filler |>
          dplyr::filter(year == copy_years[[target]]) |>
          dplyr::mutate(year = as.numeric(target))
      )
    }
  } else {
    mat_filler <- tidyr::expand_grid(
      lgroup = predict_lgroups,
      region = region_names
    ) |>
      modelr::add_predictions(mat_model, type = 'response', var = 'mat_p') |>
      dplyr::mutate(year = NA_real_)
  }

  # Combine measurements & estimates, with age = NA signifying the estimates
  dplyr::bind_rows(
    mat_length |>
      dplyr::group_by(year, lgroup, age, region) |>
      dplyr::summarise(mat_p = mean(mat)) |>
      dplyr::collect() |>
      dplyr::ungroup(),
    mat_filler |> dplyr::mutate(age = NA_real_)
  )
}

#' Scale survey abundance to strata using a station list
#'
#' As \code{pax::pax_si_scale_by_strata()}, but assigns stations to strata
#' with a fixed station list (e.g. \code{biota.strata_stations}, as the
#' tidypax-based assessments did) instead of the h3 cell of the tow position.
#' Tows near a stratum boundary otherwise move between strata, which can
#' change a survey index by tens of percent in single years. Stratum areas
#' come from the pax strata table \code{strata_name}.
#'
#' @param tbl Output of \code{pax::pax_si_by_length()}.
#' @param strata_stations Data frame with columns \code{station} and
#'   \code{stratum}.
#' @param strata_name Name of the pax strata table holding stratum areas
#'   (\code{rall_area}, km^2).
#' @return \code{tbl} with \code{si_abund} and \code{si_biomass} scaled to
#'   the stratum area.
#' @export
hr_si_scale_by_strata_stations <- function(tbl, strata_stations, strata_name = NULL) {
  # NSE variables
  sample_id <- station <- gridcell <- species <- year <- length <- NULL
  tow_depth <- stratum <- sampling_type <- area <- rall_area <- NULL
  si_abund <- si_biomass <- NULL

  pcon <- dbplyr::remote_con(tbl)
  if ("area" %in% colnames(strata_stations)) {
    # Stratum areas given with the stations (square nautical miles)
    stations_area <- dplyr::distinct(strata_stations, station, stratum, area)
  } else {
    strata_area <- dplyr::tbl(pcon, strata_name) |>
      dplyr::select(stratum, rall_area) |>
      dplyr::collect() |>
      dplyr::mutate(
        # Convert km^2 (reitmapping units) to square nautical miles (tow area units)
        area = dplyr::coalesce(rall_area, 0) / 1.852^2
      ) |>
      dplyr::select(stratum, area)
    stations_area <- strata_stations |>
      dplyr::distinct(station, stratum) |>
      dplyr::left_join(strata_area, by = "stratum")
  }

  tbl |>
    dplyr::left_join(
      pax::pax_temptbl(pcon, stations_area),
      by = "station"
    ) |>
    dplyr::mutate(area = dplyr::coalesce(area, 0)) |>
    dplyr::group_by(
      sample_id,
      station,
      gridcell,
      species,
      year,
      length,
      tow_depth,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarize(
      si_abund = sum(si_abund, na.rm = TRUE),
      si_biomass = sum(si_biomass, na.rm = TRUE)
    ) |>
    dplyr::group_by(species, year, stratum, sampling_type, area) |>
    dplyr::mutate(
      # NB: Not summarise, i.e. window function
      si_abund = area * si_abund / dplyr::n_distinct(sample_id, na.rm = TRUE),
      si_biomass = area *
        si_biomass /
        dplyr::n_distinct(sample_id, na.rm = TRUE)
    )
}

#' Pool years for an age-length key
#'
#' Relabels every year in each group of \code{ygroup} as the group's first
#' year, so the years share one age-length key. Used instead of the
#' \code{ygroup} argument of pax, which coalesces the (text) group name with
#' the (numeric) year and fails in DuckDB.
#'
#' @param tbl A (lazy) table with a \code{year} column.
#' @param ygroup Named list of year vectors, e.g.
#'   \code{list(past = 1980:1994)}. \code{NULL} does nothing.
#' @return \code{tbl} with \code{year} relabelled.
#' @export
hr_pool_years <- function(tbl, ygroup) {
  # NSE variables
  year <- NULL
  for (g in ygroup) {
    first_year <- min(g)
    tbl <- dplyr::mutate(
      tbl,
      year = ifelse(year %in% local(g), local(first_year), year)
    )
  }
  tbl
}

## Generate the ALK from the survey
#' Compute survey index data (abundance and biomass at age)
#'
#' Derives age-structured survey abundance (thousands) and mean weight (g)
#' from a pax database by applying a length distribution, an age–length key,
#' optional strata scaling, and optional maturity weighting. The result is
#' the primary model input table used by the SAM and MUPPET assessment
#' workflows. Used for both surveys and commercial samples.
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
#'   10 and 11 are additionally included in the age-length key for the first
#'   two years of data to improve age-1 estimates. Default is \code{FALSE}.
#' @param tow_number Integer vector of valid tow numbers (NA coerced to 0), or
#'   \code{NULL} for no filtering. Use for surveys (e.g. \code{0:35} for the
#'   spring survey); for commercial samples \code{tow_number} is the haul
#'   number, which can exceed 35. Default is \code{NULL}.
#' @param tgroup Named list of months, e.g. \code{list(t1 = 1:6, t2 = 7:12)},
#'   or \code{NULL}. Default is \code{NULL}.
#' @param regions Named list mapping region labels to integer MFDB area codes.
#'   Default is \code{list(all = 101:115)}.
#' @param lgroups Numeric vector of length group break points. Default is
#'   \code{seq(0, 200, 5)}.
#' @param gear_group Named list mapping gear group labels to MFDB gear codes,
#'   or \code{NULL} for no gear grouping. Default is \code{NULL}.
#' @param gear_id_filter Integer vector of gear IDs to include, or \code{NULL}
#'   for no filtering. Default is \code{NULL}.
#' @param scale_by_landings Logical. If \code{TRUE}, indices are additionally
#'   scaled to match landings by gear group and time group. Default is
#'   \code{FALSE}.
#' @param haul_scalar Data frame with columns \code{sample_id} and
#'   \code{scalar}, to down-weight individual (e.g. very large) hauls.
#'   Default \code{NULL}.
#' @param strata_stations Data frame with columns \code{station} and
#'   \code{stratum}. If given, strata are assigned from it rather than from
#'   tow positions, see \code{\link{hr_si_scale_by_strata_stations}}.
#'   Default \code{NULL}.
#' @param maturity_measured Logical. If \code{FALSE}, only the modelled
#'   maturity at length from \code{maturity_key} is used, not the measured
#'   maturity at age. Default \code{TRUE}.
#' @param maturity_na Proportion mature for lengths with neither a measurement
#'   nor an estimate, or \code{NULL} to leave them out. Default \code{NULL}.
#' @param ygroup Named list of years to pool in the age-length key, e.g.
#'   \code{list(past = 1980:1994)}, see \code{\link{hr_pool_years}}. Default
#'   \code{NULL}.
#' @param gridcell_na Grid cell to give samples without a position. Without a
#'   position a sample gets no region, matches no age-length key cell and is
#'   dropped. Default \code{NULL}.
#' @param sample_gear_na Gear code to give samples with unknown gear, when
#'   raising them (not in the age-length key). Default \code{NULL}.
#' @param landings_gear_na,landings_month_na Gear code and month to give
#'   landings with unknown gear or month when \code{scale_by_landings = TRUE}.
#'   Landings without a month are otherwise left out of the scaling. Default
#'   \code{NULL}.
#' @param ygroup_alk Named list of years to pool when building the age-length
#'   key, when it differs from \code{ygroup} (the pools the index uses).
#'   Default \code{ygroup}.
#' @param alk_age_max Otoliths older than this are left out of the
#'   age-length key. Default \code{NULL}, all.
#' @param alk A precomputed age-length key (as \code{pax::pax_ldist_alk()}:
#'   columns \code{ygroup} for the key year, \code{lgroup}, \code{age},
#'   \code{agep} and the grouping columns \code{region}, \code{gear_name},
#'   \code{tgroup}, \code{species}), for keys this function can't build
#'   (blended, borrowed from other years, shifted ages). Default \code{NULL},
#'   the key is built from the samples' otoliths.
#' @param key_year Named vector mapping each year (names) to the year of the
#'   key it uses (values), e.g. a pooled or another survey's key. Years not
#'   in it are dropped. The key years can be numbers or labels (e.g.
#'   \code{"past"}), as in the key's \code{ygroup}. Not with \code{ygroup}.
#'   Default \code{NULL}.
#' @param landings_area_like SQL LIKE pattern of the ICES areas of the
#'   landings to scale to (\code{scale_by_landings = TRUE}), when the pax
#'   database holds landings from more areas. Default \code{NULL}, all.
#' @param plus_group Ages above this are summed into it. Default
#'   \code{NULL}.
#' @param mean_length If \code{TRUE}, also the mean length at age
#'   (\code{ml}). Default \code{FALSE}.
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
  tow_number = NULL,
  tgroup = NULL,
  regions = list(all = 101:115),
  lgroups = seq(0, 200, 5),
  gear_group = NULL,
  gear_id_filter = NULL,
  scale_by_landings = FALSE,
  haul_scalar = NULL,
  strata_stations = NULL,
  maturity_measured = TRUE,
  maturity_na = NULL,
  ygroup = NULL,
  gridcell_na = NULL,
  sample_gear_na = NULL,
  landings_gear_na = NULL,
  landings_month_na = NULL,
  ygroup_alk = ygroup,
  alk_age_max = NULL,
  alk = NULL,
  key_year = NULL,
  landings_area_like = NULL,
  plus_group = NULL,
  mean_length = FALSE
) {
  # NSE variables
  si_abund <- si_biomass <- mat_p <- mat_p_est <- year <- age <- NULL
  coalesce <- gear_id <- scalar <- year_orig <- mfdb_gear_code <- NULL
  gridcell <- month <- key_label <- ices_area <- sample_id <- NULL
  species <- count <- weight <- NULL

  if (!is.null(key_year) && !is.null(ygroup)) {
    stop("Give ygroup or key_year, not both")
  }

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
  station_filter <- function(tbl) {
    dplyr::filter(
      tbl,
      local(is.null(tow_number)) |
        coalesce(tow_number, 0) %in% local(c(tow_number, -1)),
      local(is.null(gear_id_filter)) | (gear_id %in% local(gear_id_filter))
    )
  }

  if (is.null(alk)) {
  aldist_tbl <- dplyr::tbl(pcon, "aldist")
  if (!is.null(alk_age_max)) {
    aldist_tbl <- dplyr::filter(aldist_tbl, is.na(age) | age <= local(alk_age_max))
  }
  aldist_tbl <- aldist_tbl |>
    dplyr::group_by(sample_id, species, length, age) |>
    dplyr::summarize(
      count = sum(count, na.rm = TRUE),
      weight = sum(weight * count, na.rm = TRUE) / sum(count, na.rm = TRUE)
    )
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
    hr_pool_years(ygroup_alk) |>
    station_filter() |>
    pax::pax_ldist_alk(
      lgroups = lgroups,
      tgroup = tgroup,
      regions = regions,
      gear_group = gear_group,
      aldist_tbl = aldist_tbl
    )
  }

  at_age <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% local(sampling_type)) |>
    station_filter()
  if (!is.null(gridcell_na)) {
    at_age <- dplyr::mutate(at_age, gridcell = coalesce(gridcell, local(gridcell_na)))
  }
  if (!is.null(sample_gear_na)) {
    at_age <- dplyr::mutate(
      at_age,
      mfdb_gear_code = coalesce(mfdb_gear_code, local(sample_gear_na))
    )
  }
  at_age <- pax::pax_si_by_length(at_age, ldist = ldist)

  if (!is.null(haul_scalar)) {
    at_age <- at_age |>
      dplyr::left_join(pax::pax_temptbl(pcon, haul_scalar), by = "sample_id") |>
      dplyr::mutate(
        si_abund = coalesce(scalar, 1) * si_abund,
        si_biomass = coalesce(scalar, 1) * si_biomass
      ) |>
      dplyr::select(-scalar)
  }
  if (!is.null(strata_stations)) {
    at_age <- hr_si_scale_by_strata_stations(at_age, strata_stations, strata_name)
  } else if (!is.null(strata_name)) {
    at_age <- pax::pax_si_scale_by_strata(at_age, strata_name)
  }
  if (!is.null(ygroup)) {
    at_age <- at_age |>
      dplyr::mutate(year_orig = year) |>
      hr_pool_years(ygroup)
  }
  if (!is.null(key_year)) {
    # Each year uses the key of its key year; years without one are dropped
    key_map <- data.frame(
      year = as.numeric(names(key_year)),
      key_label = unname(key_year)
    )
    at_age <- at_age |>
      dplyr::inner_join(pax::pax_temptbl(pcon, key_map), by = "year") |>
      dplyr::mutate(year_orig = year, year = key_label) |>
      dplyr::select(-key_label)
  }
  at_age <- pax::pax_si_scale_by_alk(
    at_age,
    lgroups = lgroups,
    tgroup = tgroup,
    regions = regions,
    gear_group = gear_group,
    alk = alk
  )
  if (!is.null(ygroup) || !is.null(key_year)) {
    at_age <- at_age |>
      dplyr::mutate(year = year_orig) |>
      dplyr::select(-year_orig)
  }
  if (isTRUE(scale_by_landings)) {
    landings_tbl <- dplyr::tbl(pcon, "landings")
    if (!is.null(landings_area_like)) {
      landings_tbl <- dplyr::filter(
        landings_tbl,
        ices_area %like% local(landings_area_like)
      )
    }
    if (!is.null(landings_gear_na)) {
      landings_tbl <- dplyr::mutate(
        landings_tbl,
        mfdb_gear_code = coalesce(mfdb_gear_code, local(landings_gear_na))
      )
    }
    if (!is.null(landings_month_na)) {
      landings_tbl <- dplyr::mutate(
        landings_tbl,
        month = coalesce(month, local(landings_month_na))
      )
    }
    at_age <- pax::pax_si_scale_by_landings(
      at_age,
      landings_tbl = landings_tbl,
      tgroup = tgroup,
      regions = regions,
      gear_group = gear_group
    )
  }

  if (!is.null(maturity_key)) {
    # Break apart measurements & estimates (age = NA), join both separately.
    # Estimates are by year if the key was fitted by year
    mat_measurements <- maturity_key |> dplyr::filter(!is.na(age))
    if (!isTRUE(maturity_measured)) {
      mat_measurements <- mat_measurements |> dplyr::filter(FALSE)
    }
    mat_filler <- maturity_key |>
      dplyr::filter(is.na(age)) |>
      dplyr::select(-age) |>
      dplyr::rename(mat_p_est = mat_p)
    filler_by <- if (all(is.na(mat_filler$year))) {
      mat_filler <- dplyr::select(mat_filler, -year)
      c("lgroup", "region")
    } else {
      c("year", "lgroup", "region")
    }
    at_age <- at_age |>
      dplyr::left_join(
        pax::pax_temptbl(pcon, mat_measurements),
        by = c("year", "lgroup", "age", "region")
      ) |>
      dplyr::left_join(
        pax::pax_temptbl(pcon, mat_filler),
        by = filler_by
      )

    mat_c <- if (is.null(maturity_na)) {
      quote(sum(si_abund * coalesce(mat_p, mat_p_est)) / sum(si_abund))
    } else {
      substitute(
        sum(si_abund * coalesce(mat_p, mat_p_est, x)) / sum(si_abund),
        list(x = maturity_na)
      )
    }
  } else {
    mat_c <- NA
  }

  if (!is.null(plus_group)) {
    at_age <- dplyr::mutate(
      at_age,
      age = ifelse(age > local(plus_group), local(plus_group), age)
    )
  }
  summaries <- list(
    n = quote(sum(si_abund) / 1000),
    mw = quote(1000 * sum(si_biomass) / sum(si_abund))
  )
  if (isTRUE(mean_length)) {
    summaries$ml <- quote(sum(si_abund * length) / sum(si_abund))
  }
  summaries$mat <- mat_c
  out <- at_age |>
    dplyr::group_by(year, age) |>
    dplyr::summarise(!!!summaries)
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
