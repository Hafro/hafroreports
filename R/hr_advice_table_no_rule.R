#' Advice table of a stock without a catch rule
#'
#' The advice calculation table of an advice given without a rule (e.g. zero
#' catch with a sentinel fishery, or a category 5 roll-over): the previous
#' and the new advice and the change, as the old Nephrops sheets
#' (\code{prognosis_table_other()}).
#'
#' @param advice_last,advice The previous and the new advice (tonnes).
#' @param period_last,period Labels of the two advice periods, e.g.
#'   \code{2025} and \code{2026}.
#' @param change Text of the advice change. Default: the change in percent,
#'   or \code{"-"} when the previous advice is 0.
#' @return A \code{flextable} object styled for an advice sheet.
#' @export
hr_advice_table_no_rule <- function(
  advice_last,
  advice,
  period_last,
  period,
  change = NULL
) {
  lang <- getOption("hr.lang", "en")
  if (is.null(change)) {
    change <- if (advice_last == 0) {
      "-"
    } else {
      paste0(round(100 * (advice / advice_last - 1)), " %")
    }
  }
  label <- if (lang == "is") "Ráðgjöf" else "Catch advice for"
  d <- data.frame(a = advice_last, b = advice, c = change)
  names(d) <- c(
    paste(label, period_last),
    paste(label, period),
    if (lang == "is") "Breyting á ráðgjöf" else "Advice change"
  )
  flextable::flextable(d) |>
    flextable::set_table_properties(layout = "autofit", width = 1) |>
    flextable::bg(bg = "#DEEAF6", part = "header") |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(align = "center", part = "all") |>
    flextable::colformat_num(j = 1:2, big.mark = " ", decimal.mark = ".", na_str = "") |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all", border = officer::fp_border()) |>
    flextable::border(j = 2:3, border.left = officer::fp_border(), part = "all") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}
