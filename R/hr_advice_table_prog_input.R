#' Format forecast assumptions table for advice sheet
#'
#' Renders a formatted \code{flextable} showing the assumptions for the
#' interim year and forecast: variable, value and notes. Row labels come from
#' the \code{variable.en} / \code{variable.is} columns of
#' \code{data_prog_input} if present, otherwise they are built from
#' \code{name} (\code{ssb}, \code{rec}, \code{catch}, \code{HR}, \code{fbar},
#' \code{refbio}) and \code{year}. Rows are shown in the order of
#' \code{data_prog_input}, or of \code{order}.
#'
#' @param data_prog_input A data frame with columns \code{name}, \code{year},
#'   \code{value}, \code{notes.en}, \code{notes.is}, and optionally
#'   \code{variable.en}, \code{variable.is}.
#' @param assessment_year Integer. The assessment year (unused, kept for
#'   compatibility).
#' @param recruitment_age Recruitment age for the built-in recruitment label.
#'   Default \code{NULL} (no age).
#' @param order Row indices to show, in order, e.g. \code{c(5, 3, 2, 4, 6, 1)}.
#'   Default \code{NULL} (data order).
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_table_prog_input <- function(
  data_prog_input,
  assessment_year,
  recruitment_age = NULL,
  order = NULL
) {
  # NSE variables
  name <- year <- value <- NULL
  lang <- getOption("hr.lang", "en")
  rec_is <- if (is.null(recruitment_age)) 'N\u00fdli\u00f0un' else sprintf('N\u00fdli\u00f0un %s %s', recruitment_age, if (recruitment_age == 1) '\u00e1rs' else '\u00e1ra')
  rec_en <- if (is.null(recruitment_age)) 'Recruitment' else sprintf('Recruitment age %s', recruitment_age)

  if (!all(c("variable.is", "variable.en") %in% colnames(data_prog_input))) {
    data_prog_input <- data_prog_input |>
      dplyr::mutate(
        variable.is = dplyr::case_when(
          name == 'ssb' ~ sprintf('Hrygningarstofn (%s)', year),
          name == 'rec' ~ sprintf('%s (%s)', rec_is, year),
          name == 'catch' ~ sprintf('Afli (%s)', year),
          name == 'HR' ~ sprintf('Vei\u00f0ihlutfall (%s)', year),
          name == 'fbar' ~ sprintf('Vei\u00f0id\u00e1nartala (%s)', year),
          name == 'refbio' ~ sprintf('Vi\u00f0mi\u00f0unarstofn (%s)', year)
        ),
        variable.en = dplyr::case_when(
          name == 'ssb' ~ sprintf('SSB (%s)', year),
          name == 'rec' ~ sprintf('%s (%s)', rec_en, year),
          name == 'catch' ~ sprintf('Catch (%s)', year),
          name == 'HR' ~ sprintf('Harvest rate (%s)', year),
          name == 'fbar' ~ sprintf('Fishing mortality (%s)', year),
          name == 'refbio' ~ sprintf('Reference biomass (%s)', year)
        )
      )
  }
  if (!is.null(order)) {
    data_prog_input <- dplyr::slice(data_prog_input, order)
  }
  # Tonnes for biomass and catch rows
  tonnes_rows <- which(data_prog_input$name %in% c('ssb', 'catch', 'refbio'))
  number_rows <- which(data_prog_input$name %in% c('rec'))

  data_prog_input |>
    dplyr::select(
      variable = as.symbol(paste0('variable.', lang)),
      value,
      notes = as.symbol(paste0('notes.', lang))
    ) |>
    dplyr::mutate(
      value = ifelse(
        value < 1,
        round(value, 2),
        hr_red_dot_number(round(value))
      )
    ) |>
    flextable::flextable() |>
    flextable::mk_par(
      j = "variable",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Breyta" else 'Variable'
      )
    ) |>
    flextable::mk_par(
      j = "value",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Gildi" else 'Value'
      )
    ) |>
    flextable::mk_par(
      j = "notes",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Athugasemdir" else 'Notes'
      )
    ) |>
    ftExtra::colformat_md() |>
    flextable::colformat_num(
      i = tonnes_rows,
      j = 2,
      big.mark = "\u200a\u200a",
      decimal.mark = ".",
      na_str = "",
      suffix = " t"
    ) |>
    flextable::colformat_num(
      i = number_rows,
      j = 2,
      big.mark = "\u200a\u200a",
      decimal.mark = ".",
      na_str = ""
    ) |>
    flextable::valign(j = 1:3, valign = "top", part = "body") |>
    flextable::bg(j = 1:3, bg = "#DEEAF6", part = "header") |>
    ### Total width of table in advice sheet is 9
    flextable::width(j = 1, width = 2.5) |>
    flextable::width(j = 2, width = 1.25) |>
    flextable::width(j = 3, width = 6) |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(j = 2, align = "center", part = "all") |>
    flextable::border_remove() |>
    flextable::border(part = "all", border = officer::fp_border()) |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}
