#' Format advice basis table
#'
#' Creates a formatted flextable displaying the basis for catch advice,
#' selecting columns matching the current language setting.
#'
#' @param basis_data A data frame with columns named using language suffixes
#'   (e.g., `desc.en`, `desc.is`) containing the basis text for advice.
#' @param markdown If \code{TRUE}, render markdown in the text (e.g.
#'   \code{F~MSY~} as a subscript) with \code{ftExtra::colformat_md()}.
#'   Default \code{FALSE}.
#' @return A \code{flextable} object with two columns styled for inclusion in
#'   an advice sheet.
#' @export
hr_advice_basis_table <- function(basis_data, markdown = FALSE) {
  lang <- getOption("hr.lang", "en")

  ft <- basis_data |>
    dplyr::select(dplyr::contains(lang)) |>
    flextable::flextable()
  if (isTRUE(markdown)) {
    ft <- ftExtra::colformat_md(ft)
  }
  ft |>
    flextable::valign(j = 1:2, valign = "top", part = "body") |>
    flextable::bg(j = 1, bg = "#DEEAF6", part = "body") |>
    flextable::delete_part(part = "header") |>
    flextable::theme_box() |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::line_spacing(space = 1.1, part = "body") |>
    flextable::width(j = 1, width = 2) |>
    flextable::width(j = 2, width = 7) |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}
