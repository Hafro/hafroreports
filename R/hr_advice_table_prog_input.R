#' Format prognosis input table for advice sheet
#'
#' Renders a formatted \code{flextable} summarising the key model inputs for
#' the prognosis (SSB, recruitment, catch, harvest rate, reference biomass)
#' with localised variable names, formatted values, and notes columns.
#' Numeric formatting applies thousand separators to biomass and tonnage rows.
#'
#' @param data_prog_input A data frame with columns \code{name} (one of
#'   \code{"ssb"}, \code{"rec"}, \code{"catch"}, \code{"HR"},
#'   \code{"refbio"}), \code{year}, \code{value}, \code{notes.en}, and
#'   \code{notes.is}.
#' @param assessment_year Integer. Included in variable labels as the
#'   reference year.
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_table_prog_input <- function(data_prog_input, assessment_year) {
  # NSE variables
  name <- year <- value <- NULL
  lang <- getOption("hr.lang", "en")

  data_prog_input |>
    dplyr::mutate(
      variable.is = dplyr::case_when(
        name == 'ssb' ~ sprintf('Hrygningarstofn (%s)', year),
        name == 'rec' ~ sprintf('Nýliðun 1 árs (%s)', year),
        name == 'catch' ~ sprintf('Afli (%s)', year),
        name == 'HR' ~ sprintf('Veiðihlutfall (%s)', year),
        name == 'refbio' ~ sprintf('Viðmiðunarstofn (%s)', year)
      ),
      variable.en = dplyr::case_when(
        name == 'ssb' ~ sprintf('SSB (%s)', year),
        name == 'rec' ~ sprintf('Recruitment age 1 (%s)', year),
        name == 'catch' ~ sprintf('Catch (%s)', year),
        name == 'HR' ~ sprintf('Harvest rate (%s)', year),
        name == 'refbio' ~ sprintf('Viðmiðunarstofn (%s)', year)
      ),
    ) |>
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
    dplyr::slice(5, 3, 2, 4, 6, 1) |>
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
      i = c(2, 5),
      j = 2,
      big.mark = "  ",
      decimal.mark = ".",
      na_str = "",
      suffix = " t"
    ) |>
    flextable::colformat_num(
      i = c(3, 4),
      j = 2,
      big.mark = "  ",
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
