#' Retrieve a localised label
#'
#' Looks up a display label from the \code{\link{hr_locale}} translation table
#' using the current language (set via \code{options(hr.lang = "en")} or
#' \code{"is"}). Falls back to the English value if the requested language is
#' missing, and to the raw key if no translation exists at all.
#'
#' The special key \code{"recruitment_age"} constructs a label of the form
#' \emph{"Recruitment (age N)"} / \emph{"Nýliðun (N árs/ára)"} using the
#' first \code{...} argument as the age.
#'
#' @param key A character string identifying the label to look up. Must match
#'   a row in \code{\link{hr_locale}}.
#' @param ... Additional arguments passed to label constructors. For the
#'   \code{"recruitment_age"} key, the first argument is the numeric age.
#' @param bold Logical. If \code{TRUE}, wraps the returned string in a
#'   \code{bold()} call suitable for use in \code{ggplot2} plot titles via
#'   \code{parse = TRUE}. Default is \code{FALSE}.
#' @return A character string (or language object when \code{bold = TRUE})
#'   containing the localised label.
#' @export
hr_label <- function(key, ..., bold = FALSE) {
  lang <- getOption("hr.lang", "en")

  if (key == "recruitment_age") {
    if (lang == "is") {
      out <- sprintf(
        "Nýliðun (%s %s)",
        as.character(..1),
        if (..1 == 1) "árs" else "ára"
      )
    } else {
      out <- sprintf("Recruitment (age %s)", as.character(..1))
    }
  } else {
    loc <- hafroreports::hr_locale
    rownames(loc) <- loc$key
    out <- as.character(loc[key, lang])
    if (is.na(out)) {
      out <- as.character(loc[key, "en"])
    }
    if (is.na(out)) {
      out <- key
    }
  }

  if (isTRUE(bold)) {
    out <- substitute(bold(x), list(x = out))
  }
  return(out)
}
