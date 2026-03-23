hr_assessment_from_sag <- function(
  assessment_year,
  species,
  ices_stock_key_label,
  ices_median_refbio = NULL
) {
  assessment_keys <-
    icesSAG::getListStocks(assessment_year) |>
    dplyr::filter(StockKeyLabel == ices_stock_key_label) |>
    dplyr::pull('AssessmentKey')

  icesSAG::getSummaryTable(assessment_keys) |>
    dplyr::left_join(
      icesSAG::getCustomColumns(assessment_keys) |>
        dplyr::filter(customName %in% local(c(ices_median_refbio)))
    ) |>
    dplyr::select(
      year = Year,
      species = species,
      low_recruitment = low_recruitment,
      median_recruitment = recruitment,
      high_recruitment = high_recruitment,
      low_SSB = low_SSB,
      median_SSB = SSB,
      high_SSB = high_SSB,
      #low_refbio = CustomSeries3,
      median_refbio = customValue,
      #high_refbio = CustomSeries4,
      landings = landings,
      low_HR = low_F,
      median_HR = F,
      high_HR = high_F
      # median_F = CustomSeries2
    ) |>
    dplyr::mutate(
      species = local(species),
      assessment_year = local(assessment_year),
      low_HR = ifelse(year == assessment_year, NA_real_, low_HR),
      median_HR = ifelse(year == assessment_year, NA_real_, median_HR),
      high_HR = ifelse(year == assessment_year, NA_real_, high_HR)
    )
}

hr_assessment_combine <- function(...) {
  # Start with a default set of columns
  out <- tibble::tibble(
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
  for (a in list(...)) {
    if (is.null(a)) {
      continue
    }
    out <- out |>
      # Remove any rows of a previous assessment in that year
      dplyr::filter(!(assessment_year %in% unique(a$assessment_year))) |>
      dplyr::bind_rows(a)
  }
  # Remove template once done
  return(out |> dplyr::filter(!is.na(year)))
}
