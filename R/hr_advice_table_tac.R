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
    # Landings with no country, if any
    dplyr::select(-dplyr::any_of("NA")) |>
    dplyr::mutate(
      # Missing landings of one part (e.g. no foreign landings) count as 0
      total = ifelse(
        is.na(icelandic) & is.na(foreign),
        NA_real_,
        dplyr::coalesce(icelandic, 0) + dplyr::coalesce(foreign, 0)
      )
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
#' Renders a formatted \code{flextable} showing historical advice, TAC and
#' landings by fishing year, with localised column headers. A note on foreign
#' landings before 2014 is added when those columns are shown, and
#' stock-specific notes can be attached to cells with \code{footnotes}.
#'
#' @param data_tac A data frame as returned by \code{\link{hr_advice_data_tac}},
#'   with columns \code{advice_period}, \code{advice}, \code{tac},
#'   \code{icelandic}, \code{foreign}, and \code{total}.
#' @param columns Columns to show. Default all six.
#' @param footnotes List of footnotes, each a list with \code{i} (rows, or a
#'   function of the number of rows), \code{j} (column name or number),
#'   \code{en} and \code{is} (text), e.g.
#'   \code{list(list(i = 32:36, j = "advice", en = "40 \% harvest control rule", is = "40 \% aflaregla"))}.
#'   Rows outside the table are ignored. Default \code{NULL}.
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_table_tac <- function(
  data_tac,
  columns = c("advice_period", "advice", "tac", "icelandic", "foreign", "total"),
  footnotes = NULL
) {
  lang <- getOption("hr.lang", "en")
  headers <- list(
    advice_period = c(en = 'Fishing year', is = "Fiskveiðiár"),
    advice = c(en = 'Recommended TAC', is = "Tillaga"),
    tac = c(en = "National TAC", is = "Aflamark"),
    icelandic = c(en = "Catches Iceland", is = "Afli Íslendinga"),
    foreign = c(en = "Catches other nations", is = "Afli annarra þjóða"),
    total = c(en = "Total catch", is = "Afli alls")
  )
  data_tac <- data_tac[, columns, drop = FALSE]
  n_rows <- nrow(data_tac)
  width <- 9 / length(columns) ### Total width of table in advice sheet is 9

  ft <- flextable::flextable(data_tac)
  for (col in columns) {
    ft <- flextable::mk_par(
      ft,
      j = col,
      part = "header",
      value = flextable::as_paragraph(headers[[col]][[lang]])
    )
  }
  ft <- ft |>
    flextable::colformat_num(
      j = setdiff(columns, "advice_period"),
      big.mark = "  ",
      decimal.mark = ".",
      na_str = ""
    ) |>
    flextable::valign(valign = "top", part = "all") |>
    flextable::bg(bg = "#DEEAF6", part = "header") |>
    flextable::width(width = width) |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(align = "center", part = "all") |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all", border = officer::fp_border()) |>
    flextable::border(part = "all", border.right = officer::fp_border())

  symbol <- 1
  foreign_cols <- intersect(c("foreign", "total"), columns)
  if ("foreign" %in% columns) {
    ft <- flextable::footnote(
      ft,
      j = foreign_cols,
      i = 1,
      value = flextable::as_paragraph(
        if (lang == 'is') {
          'Afli annarra þjóð fyrir 2014 er aðeins skráður á almanaksári. Fyrir þann tíma tekur heildarafli á fiskveiðiári því ekki tillit til erlends afla nema að litlu leyti.'
        } else {
          "Landings of other nations before 2014 is only available by calendar year. Before that time total catches within the fishing year mostly excludes foreign landings."
        }
      ),
      ref_symbols = paste0(symbol, ") "),
      part = "header"
    )
    symbol <- symbol + 1
  }
  for (fn in footnotes) {
    rows <- if (is.function(fn$i)) fn$i(n_rows) else fn$i
    rows <- rows[rows >= 1 & rows <= n_rows]
    if (!length(rows)) next
    ft <- flextable::footnote(
      ft,
      i = rows,
      j = fn$j,
      value = flextable::as_paragraph(fn[[lang]]),
      ref_symbols = paste0(symbol, ") "),
      part = "body"
    )
    symbol <- symbol + 1
  }
  ft |>
    flextable::padding(padding = 2, part = "footer") |>
    flextable::fontsize(size = 8, part = "footer") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}
