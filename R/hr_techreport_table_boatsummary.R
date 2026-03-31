#' Format vessel and catch summary table by gear
#'
#' Queries the \code{landings} table, groups by gear, and produces a GT table
#' summarising the number of vessels and catch (in thousands of tonnes) per
#' gear type per year. Column headers are localised to the current language.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{gt} table object.
#' @export
hr_techreport_table_boatsummary <- function(
  pcon,
  year_start = 1000,
  year_end = 9999
) {
  # NSE variables
  year <- gear_name <- catch <- country <- NULL
  lang <- getOption("hr.lang", "en")

  tlate_cols <- function(col_names) {
    col_names <- gsub(
      '^num_boats_',
      c(en = 'Nr. ', is = 'Fjöldi báta ')[[lang]],
      col_names
    )
    col_names <- gsub('^catch_', c(en = '', is = "Afli ")[[lang]], col_names)
    col_names[col_names == "Year"] <- hr_label("year")
    col_names[col_names == "total_catch"] <- hr_label("total_catch")
    return(col_names)
  }

  dplyr::tbl(pcon, "landings") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    pax::pax_landings_by_gear() |>
    dplyr::ungroup() |>
    dplyr::filter(
      gear_name %in% c('BMT', 'DSE', 'LLN', 'Other'),
      year >= year_start,
      catch > 0,
      country == 'Iceland'
    ) |>
    dplyr::mutate(catch = round(catch / 1e3)) |>
    pax::pax_landings_boat_summary() |>
    dplyr::rename_with(tlate_cols) |>
    tbl_formater()
}
