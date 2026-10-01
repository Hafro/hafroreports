#' Fetch assessment results from ICES SAG
#'
#' Downloads the summary table and custom columns for a given stock from the
#' ICES Stock Assessment Graphs (SAG) database and reshapes them into the
#' standard assessment data frame used by this package.
#'
#' @param assessment_year Numeric. The assessment year to retrieve.
#' @param species Character. Species identifier to attach to the output
#'   (not used for filtering; purely informational).
#' @param ices_stock_key_label Character. The ICES stock key label
#'   (e.g. \code{"had.27.5a"}).
#' @param ices_median_refbio Character or \code{NULL}. Name of the custom
#'   SAG column to use as the reference biomass median. If \code{NULL} no
#'   reference biomass column is included.
#' @return A tibble with columns \code{year}, \code{species},
#'   \code{assessment_year}, \code{low_recruitment}, \code{median_recruitment},
#'   \code{high_recruitment}, \code{low_SSB}, \code{median_SSB},
#'   \code{high_SSB}, \code{median_refbio}, \code{landings}, \code{low_HR},
#'   \code{median_HR}, and \code{high_HR}. Harvest rate estimates for the
#'   assessment year itself are set to \code{NA}.
#' @export
hr_assessment_from_sag <- function(
  assessment_year,
  species,
  ices_stock_key_label,
  ices_median_refbio = NULL
) {
  # NSE variables
  StockKeyLabel <- Year <- low_recruitment <- recruitment <- high_recruitment <- NULL
  low_SSB <- SSB <- high_SSB <- landings <- low_F <- high_F <- NULL
  customUnit <- customColumnId <- customName <- customValue <- NULL
  year <- low_HR <- median_HR <- high_HR <- median_refbio <- NULL

  assessment_keys <-
    icesSAG::getListStocks(assessment_year) |>
    dplyr::filter(StockKeyLabel == ices_stock_key_label) |>
    dplyr::pull('AssessmentKey')

  out <- icesSAG::getSummaryTable(assessment_keys)
  if (!is.null(ices_median_refbio)) {
    # NB: Join only the reference biomass custom column, and only by year.
    #     Submissions can have other custom columns (e.g. "F" for saithe 2025)
    #     that would otherwise join on, or clash with, summary table columns
    refbio <- icesSAG::getCustomColumns(assessment_keys) |>
      dplyr::filter(customName == ices_median_refbio) |>
      dplyr::select(Year, median_refbio = customValue)
    out <- dplyr::left_join(out, refbio, by = 'Year')
  } else {
    out <- dplyr::mutate(out, median_refbio = NA_real_)
  }
  out |>
    dplyr::select(
      year = Year,
      low_recruitment = low_recruitment,
      median_recruitment = recruitment,
      high_recruitment = high_recruitment,
      low_SSB = low_SSB,
      median_SSB = SSB,
      high_SSB = high_SSB,
      median_refbio,
      landings = landings,
      low_HR = low_F,
      median_HR = as.symbol("F"),
      high_HR = high_F
    ) |>
    dplyr::mutate(
      species = .env$species,
      assessment_year = .env$assessment_year,
      low_HR = ifelse(year == assessment_year, NA_real_, low_HR),
      median_HR = ifelse(year == assessment_year, NA_real_, median_HR),
      high_HR = ifelse(year == assessment_year, NA_real_, high_HR)
    )
}

#' Assessment summary from a SAM fit
#'
#' Builds the current assessment's rows of the assessment history (the format
#' of \code{\link{hr_assessment_template}}) from a SAM fit, so the history
#' doesn't depend on ICES SAG. Recruitment, SSB and F (Fbar) come from the
#' fit, the reference biomass and harvest rate from \code{SAMutils::rby.sam()}
#' if \code{ref_bio_type} is given. F and the harvest rate are left empty in
#' the assessment year (no catch data), as are landings.
#'
#' @param sam_fit A SAM fit (\code{sam_fit$fit} from
#'   \code{SAMutils::full_sam_fit()}).
#' @param input_data_landings Landings by year with columns \code{year} and
#'   \code{catch} (kg), e.g. from \code{\link{hr_input_data_landings}}.
#' @param species Species code.
#' @param assessment_year Assessment year.
#' @param ref_bio_type \code{NULL} (no reference biomass, e.g. for F-based
#'   advice), \code{"length"} or \code{"age"} (passed to
#'   \code{SAMutils::rby.sam()}). Default \code{NULL}.
#' @return A tibble with the columns of \code{\link{hr_assessment_template}},
#'   landings in tonnes.
#' @export
hr_assessment_from_fit <- function(
  sam_fit,
  input_data_landings,
  species,
  assessment_year,
  ref_bio_type = NULL
) {
  # NSE variables
  variable <- median <- lower <- upper <- year <- catch <- landings <- low <- high <- NULL

  rby <- if (is.null(ref_bio_type)) {
    SAMutils::rby.sam(sam_fit, run_ref_bio = FALSE)
  } else {
    SAMutils::rby.sam(sam_fit, ref_bio_type = ref_bio_type)
  }
  out <- rby |>
    dplyr::filter(variable %in% c("rec", "ssb", "fbar", "ref_bio", "hr")) |>
    dplyr::mutate(
      variable = dplyr::recode(
        variable,
        rec = "recruitment",
        ssb = "SSB",
        fbar = "F",
        ref_bio = "refbio",
        hr = "HR"
      )
    ) |>
    dplyr::rename(low = lower, high = upper) |>
    tidyr::pivot_wider(
      names_from = variable,
      values_from = c(median, low, high)
    ) |>
    dplyr::left_join(
      input_data_landings |>
        dplyr::collect() |>
        dplyr::transmute(year, landings = catch / 1e3),
      by = "year"
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      species = as.integer(.env$species),
      assessment_year = as.integer(.env$assessment_year),
      landings = ifelse(year == .env$assessment_year, NA_real_, landings)
    )
  # Columns the fit doesn't provide (e.g. reference biomass)
  template <- hr_assessment_template()
  for (col in setdiff(names(template), names(out))) {
    out[[col]] <- NA_real_
  }
  out |>
    dplyr::mutate(
      # No catch data in the assessment year, so no F or harvest rate
      dplyr::across(
        dplyr::matches("_(HR|F)$"),
        ~ ifelse(year == .env$assessment_year, NA_real_, .x)
      )
    ) |>
    dplyr::select(dplyr::all_of(names(template)))
}

#' Create an empty assessment data template
#'
#' Returns a one-row tibble with all \code{NA} values and the column
#' structure expected by the assessment data functions in this package.
#' Useful as a starting point when building assessment data frames manually.
#'
#' @return A tibble with columns \code{year}, \code{species},
#'   \code{median_SSB}, \code{low_SSB}, \code{high_SSB}, \code{median_F},
#'   \code{low_F}, \code{high_F}, \code{median_recruitment},
#'   \code{low_recruitment}, \code{high_recruitment}, \code{landings},
#'   \code{median_refbio}, \code{low_refbio}, \code{high_refbio},
#'   \code{median_HR}, \code{low_HR}, \code{high_HR}, and
#'   \code{assessment_year}, all set to \code{NA}.
#' @export
hr_assessment_template <- function() {
  tibble::tibble(
    year = NA_integer_,
    species = NA_integer_,
    median_SSB = NA_real_,
    low_SSB = NA_real_,
    high_SSB = NA_real_,
    median_F = NA_real_,
    low_F = NA_real_,
    high_F = NA_real_,
    median_recruitment = NA_real_,
    low_recruitment = NA_real_,
    high_recruitment = NA_real_,
    landings = NA_real_,
    median_refbio = NA_real_,
    low_refbio = NA_real_,
    high_refbio = NA_real_,
    median_HR = NA_real_,
    low_HR = NA_real_,
    high_HR = NA_real_,
    assessment_year = NA_integer_
  )
}
