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
  low_SSB <- SSB <- high_SSB <- landings <- low_F <- F <- high_F <- NULL
  customUnit <- customColumnId <- customName <- customValue <- NULL
  year <- low_HR <- median_HR <- high_HR <- NULL

  assessment_keys <-
    icesSAG::getListStocks(assessment_year) |>
    dplyr::filter(StockKeyLabel == ices_stock_key_label) |>
    dplyr::pull('AssessmentKey')

  icesSAG::getSummaryTable(assessment_keys) |>
    dplyr::left_join(
      icesSAG::getCustomColumns(assessment_keys) |>
        dplyr::select(-c(customUnit, customColumnId)) |>
        tidyr::pivot_wider(names_from = customName, values_from = customValue)
    ) |>
    dplyr::select(
      year = Year,
      low_recruitment = low_recruitment,
      median_recruitment = recruitment,
      high_recruitment = high_recruitment,
      low_SSB = low_SSB,
      median_SSB = SSB,
      high_SSB = high_SSB,
      #low_refbio = CustomSeries3,
      median_refbio = as.symbol(ices_median_refbio),
      #high_refbio = CustomSeries4,
      landings = landings,
      low_HR = low_F,
      median_HR = F,
      high_HR = high_F
      # median_F = CustomSeries2
    ) |>
    dplyr::mutate(
      species = .env$species,
      assessment_year = .env$assessment_year,
      low_HR = ifelse(year == assessment_year, NA_real_, low_HR),
      median_HR = ifelse(year == assessment_year, NA_real_, median_HR),
      high_HR = ifelse(year == assessment_year, NA_real_, high_HR)
    )
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
