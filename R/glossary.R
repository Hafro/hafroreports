#' Generate a glossary section
#'
#' Writes a Markdown/Quarto glossary section to the console using
#' \code{cat}. The section title and term definitions are localised to
#' the current language setting (\code{options(hr.lang = "en")} or
#' \code{"is"}). Currently defines the acronym TAC.
#'
#' Intended to be called inside a Quarto code block with
#' \code{output: asis}:
#'
#' ```{r}
#' #| echo: false
#' #| output: asis
#'
#' hr_glossary_section()
#' ```
#'
#' @return Invisibly returns \code{NULL}; the glossary is written to the
#'   console via \code{cat}.
#' @export
hr_glossary_section <- function() {
  lang <- getOption("hr.lang", "en")

  header_text = list(
    en = "Glossary",
    is = "Orðalisti"
  )

  glossary = list(
    # NB: Text vector can include multiple entries for newlines
    TAC = list(
      en = c("Total Allowable Catch"),
      is = c("Leyfilegur heildarafli")
    )
  )

  cat(paste0("## ", header_text[[lang]]), "\n\n", sep = "")
  for (n in names(glossary)) {
    cat(n, "\n", sep = "")
    cat(paste(": ", glossary[[n]][[lang]], "\n"), sep = "")
  }
}
