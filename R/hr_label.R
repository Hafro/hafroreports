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
