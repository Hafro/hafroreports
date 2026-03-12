#' Generate a glossary section
#'
#' ```{r}
#' #| echo: false
#' #| output: asis
#'
#' hr_glossary_section(lang)
#' ```
hr_glossary_section <- function(lang = "en") {
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
