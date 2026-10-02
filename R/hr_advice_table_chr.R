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
  # NSE variables
  component <- label <- chr_desc <- value <- NULL
  lang <- getOption("hr.lang", "en")
  tyr <- assessment_year

  values <- chr_prognosis |>
    dplyr::select(component, value) |>
    dplyr::mutate(component = trimws(component))
  base <- chr_prognosis_base |>
    dplyr::mutate(component = trimws(component), order = dplyr::row_number())
  # Match case-insensitively (fproxy_rule() names, or lower case as the old
  # chr_prognosis.csv)
  base$value <- values$value[match(tolower(base$component), tolower(values$component))]

  tbl <- base |>
    dplyr::arrange(order) |>
    dplyr::select(
      label,
      chr_desc = rlang::sym(paste0('chr_desc.', lang)),
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
      value = ftExtra::as_paragraph_md(
        if (lang == 'is') {
          'A~y+1~ = I~y~ × HR~MSY\\ proxy~ × b × m, takmarkað með sveiflujöfnun'
        } else {
          "A~y+1~ = I~y~ × HR~MSY\\ proxy~ × b × m, limited by stability clause"
        }
      ),
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
          'Tölur í töflu eru námundaðar. Útreikningar eru gerðir með ónámunduðum tölum og því gætu reiknuð gildi ekki stemmt'
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
