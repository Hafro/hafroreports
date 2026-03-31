#' Assemble TAC and landings history for advice sheet
#'
#' Joins historical advice, TAC, and landings data into a single table
#' suitable for displaying in the TAC history section of an advice sheet.
#' Landings are split into Icelandic and foreign components using the
#' \code{country} column.
#'
#' @param advice_hist A data frame with columns \code{assessment_year} and
#'   \code{advice} (recommended catch in tonnes) and \code{advice_period}
#'   (fishing year label).
#' @param tac_hist A data frame with columns \code{assessment_year} and
#'   \code{tac} (national TAC in tonnes).
#' @param landings_by_fishing_year_country A data frame with columns
#'   \code{fishing_year}, \code{country}, and \code{catch} (in kg).
#' @return A tibble with columns \code{advice_period}, \code{advice},
#'   \code{tac}, \code{icelandic} (Icelandic catch in thousands of tonnes),
#'   \code{foreign}, and \code{total}.
#' @export
hr_advice_data_tac <- function(
  advice_hist,
  tac_hist,
  landings_by_fishing_year_country
) {
  # NSE variables
  fishing_year <- country <- catch <- icelandic <- foreign <- total <- advice_period <- NULL
  advice <- tac <- NULL

  # TODO: 02-had say:, from mar::vessel() mutate(origin = case_when(status == 'Erlent' ~ 'foreign', TRUE ~ 'icelandic'))
  #       I think this is a poor-man's version of our contry column, from mar::landadur_afli():land
  # TODO: Vastly over-reported foreign landings, not filtering areas?
  landings <-
    landings_by_fishing_year_country |>
    dplyr::mutate(
      country = ifelse(country == "Iceland", "icelandic", "foreign")
    ) |>
    dplyr::group_by(
      fishing_year,
      country
    ) |>
    dplyr::summarize(
      catch = round(sum(catch, na.rm = TRUE) / 1000),
      .groups = "drop_last"
    ) |>
    tidyr::pivot_wider(
      id_cols = fishing_year,
      names_from = country,
      values_from = catch
    ) |>
    dplyr::select(-as.symbol("NA")) |>
    dplyr::mutate(
      total = icelandic + foreign
    ) |>
    dplyr::rename(
      advice_period = fishing_year
    )

  advice_hist |>
    dplyr::left_join(
      tac_hist,
      by = 'assessment_year'
    ) |>
    dplyr::left_join(
      landings,
      by = 'advice_period'
    ) |>
    dplyr::select(
      advice_period,
      advice,
      tac,
      icelandic,
      foreign,
      total
    )
}

#' Format TAC history table for advice sheet
#'
#' Renders a formatted \code{flextable} showing historical advice, TAC,
#' Icelandic landings, foreign landings, and total landings by fishing year.
#' Localised column headers, footnotes for data caveats, and harvest control
#' rule annotations are added automatically.
#'
#' @param data_tac A data frame as returned by \code{\link{hr_advice_data_tac}},
#'   with columns \code{advice_period}, \code{advice}, \code{tac},
#'   \code{icelandic}, \code{foreign}, and \code{total}.
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_table_tac <- function(data_tac) {
  lang <- getOption("hr.lang", "en")

  data_tac |>
    flextable::flextable() |>
    flextable::mk_par(
      j = "advice_period",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Fiskveiðiár" else 'Fishing year'
      )
    ) |>
    flextable::mk_par(
      j = "advice",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Tillaga" else 'Recommended TAC'
      )
    ) |>
    flextable::mk_par(
      j = "tac",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Aflamark" else "National TAC"
      )
    ) |>
    flextable::mk_par(
      j = "icelandic",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Afli Íslendinga" else "Catches Iceland"
      )
    ) |>
    flextable::mk_par(
      j = "foreign",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Afli annarra þjóða" else "Catches other nations"
      )
    ) |>
    flextable::mk_par(
      j = "total",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Afli alls" else "Total catch"
      )
    ) |>
    flextable::colformat_num(
      j = 2:6,
      big.mark = "  ",
      decimal.mark = ".",
      na_str = ""
    ) |>
    flextable::valign(valign = "top", part = "all") |>
    flextable::bg(bg = "#DEEAF6", part = "header") |>
    ### Total width of table in advice sheet is 9
    flextable::width(j = 1, width = 1.5) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::width(j = 3, width = 1.5) |>
    flextable::width(j = 4, width = 1.5) |>
    flextable::width(j = 5, width = 1.5) |>
    flextable::width(j = 6, width = 1.5) |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(
      #j = 2:6,
      align = "center",
      part = "all"
    ) |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all", border = officer::fp_border()) |>
    flextable::border(part = "all", border.right = officer::fp_border()) |>
    flextable::footnote(
      j = 5:6,
      i = 1,
      value = flextable::as_paragraph(
        if (lang == 'is') {
          'Afli annarra þjóð fyrir 2014 er aðeins skráður á almanaksári. Fyrir þann tíma tekur heildarafli á fiskveiðiári því ekki tillit til erlends afla nema að litlu leyti.'
        } else {
          "Landings of other nations before 2014 is only available by calendar year. Before that time total catches within the fishing year mostly excludes foreign landings."
        }
      ),
      ref_symbols = "1) ",
      part = "header"
    ) |>
    flextable::footnote(
      i = 32:36,
      j = 2,
      value = flextable::as_paragraph(
        if (lang == 'is') '40 % aflaregla' else "40 % harvest control rule"
      ),
      ref_symbols = "2) ",
      part = "body"
    ) |>
    flextable::footnote(
      i = 37:dim(data_tac)[1],
      j = 2,
      value = flextable::as_paragraph(
        if (lang == 'is') '35 % aflaregla' else "35 % harvest control rule"
      ),
      ref_symbols = "3) ",
      part = "body"
    ) |>
    flextable::footnote(
      i = 38,
      j = 3,
      value = flextable::as_paragraph(
        if (lang == 'is') {
          "Aflamark aukið um 8 000 t um mitt fiskveiðiár"
        } else {
          "TAC was increased by 8 000 t mid-fishing year"
        }
      ),
      ref_symbols = "4) ",
      part = "body"
    ) |>
    flextable::footnote(
      i = 39,
      j = 3,
      value = flextable::as_paragraph(
        if (lang == 'is') {
          "Aflamark minnkað um 8 000 t vegna aukningar á fyrra fiskveiðiári"
        } else {
          "TAC was decreased by 8 000 t because of the increase in the previous fishing year"
        }
      ),
      ref_symbols = "5) ",
      part = "body"
    ) |>
    flextable::padding(padding = 2, part = "footer") |>
    flextable::fontsize(size = 8, part = "footer") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}
