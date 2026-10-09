#' Format the constant harvest rate (chr) advice calculation table
#'
#' Table of the category 3 constant harvest rate rule (ICES method 2.2): the
#' previous advice, the latest index, the MSY proxy harvest rate, the biomass
#' safeguard, the precautionary multiplier, the stability clause and the
#' advice. As \code{tidypax:::chr_prognosis_table()}, from the output of
#' \code{dlsrules::fproxy_rule()} instead of a file.
#'
#' @param chr_prognosis Data frame with columns \code{component} and
#'   \code{value} (the output of \code{dlsrules::fproxy_rule()}, component
#'   names in lower or original case).
#' @param assessment_year Integer. The assessment year, used in the row
#'   descriptions.
#' @param chr_prognosis_base Data frame with the row layout: \code{label},
#'   \code{chr_desc.en}, \code{chr_desc.is} and \code{component} (\code{NA}
#'   for heading rows). \code{\{tyr\}} in the descriptions is replaced by the
#'   assessment year. Default: the tidypax layout, bundled with the package.
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_table_chr <- function(
  chr_prognosis,
  assessment_year,
  chr_prognosis_base = readr::read_csv(
    system.file("extdata", "chr_prognosis_base.csv", package = "hafroreports"),
    show_col_types = FALSE
  )
) {
  advice_table_dls(
    chr_prognosis,
    assessment_year,
    base = chr_prognosis_base,
    desc_prefix = "chr_desc",
    advice_formula = list(
      en = "A~y+1~ = I~y~ \u00d7 HR~MSY\\ proxy~ \u00d7 b \u00d7 m, limited by stability clause",
      is = "A~y+1~ = I~y~ \u00d7 HR~MSY\\ proxy~ \u00d7 b \u00d7 m, takmarka\u00f0 me\u00f0 sveifluj\u00f6fnun"
    )
  )
}

#' Format the rfb (ratio, f, b) advice calculation table
#'
#' Table of the category 3 rfb rule (ICES method 2.1): the previous advice,
#' the index ratio, the fishing pressure proxy from mean catch length, the
#' biomass safeguard, the precautionary multiplier, the stability clause and
#' the advice. As \code{tidypax:::rfb_prognosis_table()}, from the output of
#' \code{dlsrules::rfb_rule()} instead of a file.
#'
#' @param rfb_prognosis Data frame with columns \code{component} and
#'   \code{value} (the output of \code{dlsrules::rfb_rule()}).
#' @param assessment_year Integer. The assessment year, used in the row
#'   descriptions.
#' @param biannual \code{TRUE} (default) if the advice is for two fishing
#'   years, as the rfb rule usually is.
#' @param rfb_prognosis_base Data frame with the row layout: \code{label},
#'   \code{rfb_desc.en}, \code{rfb_desc.is} and \code{component}. Default:
#'   the tidypax layout, bundled with the package.
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_table_rfb <- function(
  rfb_prognosis,
  assessment_year,
  biannual = TRUE,
  rfb_prognosis_base = readr::read_csv(
    system.file("extdata", "rfb_prognosis_base.csv", package = "hafroreports"),
    show_col_types = FALSE
  )
) {
  if (!biannual) {
    advice <- trimws(rfb_prognosis_base$component) == "catch_advice"
    rfb_prognosis_base$rfb_desc.is[advice] <- "R\u00e1\u00f0gj\u00f6f fyrir {tyr}/{tyr+1}"
    rfb_prognosis_base$rfb_desc.en[advice] <- "Catch advice for {tyr}/{tyr+1}"
  }
  advice_table_dls(
    rfb_prognosis,
    assessment_year,
    base = rfb_prognosis_base,
    desc_prefix = "rfb_desc",
    advice_formula = list(en = "A~y~ \u00d7 r \u00d7 1/f \u00d7 b \u00d7 m", is = "A~y~ \u00d7 r \u00d7 1/f \u00d7 b \u00d7 m")
  )
}

#' Advice calculation table of a dlsrules rule
#'
#' @param prognosis Data frame with \code{component} and \code{value}.
#' @param assessment_year Assessment year, for \code{\{tyr\}} in the
#'   descriptions.
#' @param base Row layout with \code{label}, \code{<desc_prefix>.en},
#'   \code{<desc_prefix>.is} and \code{component} (\code{NA} for headings).
#' @param desc_prefix Prefix of the description columns.
#' @param advice_formula Footnote on the advice calculation row, by language.
#' @noRd
advice_table_dls <- function(prognosis, assessment_year, base, desc_prefix, advice_formula) {
  # NSE variables
  component <- label <- chr_desc <- value <- NULL
  lang <- getOption("hr.lang", "en")
  tyr <- assessment_year

  values <- prognosis |>
    dplyr::select(component, value) |>
    dplyr::mutate(component = trimws(component))
  base <- base |>
    dplyr::mutate(component = trimws(component), order = dplyr::row_number())
  # Match case-insensitively (fproxy_rule() names, or lower case as the old
  # chr_prognosis.csv)
  base$value <- values$value[match(tolower(base$component), tolower(values$component))]

  tbl <- base |>
    dplyr::arrange(order) |>
    dplyr::select(
      label,
      chr_desc = rlang::sym(paste0(desc_prefix, '.', lang)),
      value
    ) |>
    dplyr::mutate(
      chr_desc = vapply(
        trimws(chr_desc),
        function(x) as.character(stringr::str_glue(x, tyr = tyr)),
        character(1)
      ),
      value = ifelse(
        value < 31,
        as.character(round(value, 3)),
        hr_red_dot_number(round(value))
      ),
      label = ifelse(is.na(label), '', paste0(label, ': ')),
      chr_desc = paste0(label, chr_desc)
    ) |>
    dplyr::select(chr_desc, value)
  n <- nrow(tbl)
  # Heading rows (no component) and the rows below them
  heading <- which(is.na(base$component))
  row_advice <- which(tolower(base$component) == "initial_catch_advice")
  row_clause <- which(tolower(base$component) == "stability_clause_applied")

  flextable::flextable(tbl) |>
    ftExtra::colformat_md() |>
    flextable::delete_part(part = "header") |>
    flextable::bg(bg = "#DEEAF6", part = "body", j = 1, i = seq_len(n)) |>
    flextable::bg(bg = "#DEEAF6", part = "body", j = 2, i = heading) |>
    ### Total width of table in advice sheet is 9
    flextable::width(j = 1, width = 5 * 1.5) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::align(j = 2, align = "center", part = "all") |>
    flextable::border_remove() |>
    flextable::border(
      j = 2,
      i = setdiff(seq_len(n), heading),
      border.left = officer::fp_border(),
      part = "body"
    ) |>
    flextable::border(part = "all", border.bottom = officer::fp_border()) |>
    flextable::border(i = seq_len(n), j = 1, border.left = officer::fp_border()) |>
    flextable::border(i = 1, j = 1:2, border.top = officer::fp_border()) |>
    flextable::border(i = seq_len(n), j = 2, border.right = officer::fp_border()) |>
    flextable::footnote(
      j = 1,
      i = row_advice,
      value = ftExtra::as_paragraph_md(advice_formula[[lang]]),
      ref_symbols = "1) ",
      part = "body"
    ) |>
    flextable::footnote(
      j = 1,
      i = row_clause,
      value = ftExtra::as_paragraph_md('min{max(0.7A~y~, A~y+1~), 1.2A~y~}'),
      ref_symbols = "2) ",
      part = "body"
    ) |>
    flextable::footnote(
      j = 1,
      i = n,
      value = flextable::as_paragraph(
        if (lang == 'is') {
          'T\u00f6lur \u00ed t\u00f6flu eru n\u00e1munda\u00f0ar. \u00datreikningar eru ger\u00f0ir me\u00f0 \u00f3n\u00e1mundu\u00f0um t\u00f6lum og \u00fev\u00ed g\u00e6tu reiknu\u00f0 gildi ekki stemmt'
        } else {
          "The figures in the table are rounded. Calculations were done with unrounded inputs, and compared values may not match exactly when calculated using the rounded figures in the table."
        }
      ),
      ref_symbols = "3) ",
      part = "body"
    ) |>
    flextable::padding(padding = 2, part = "footer") |>
    flextable::fontsize(size = 8, part = "footer") |>
    flextable::fontsize(size = 9, part = "body")
}
