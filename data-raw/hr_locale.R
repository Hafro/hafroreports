# Writes data/hr_locale.rda from data-raw/hr_locale.tsv (key, en, is).
# Run from the package root: Rscript data-raw/hr_locale.R
#
# Read without NA strings: "NA" is the Icelandic label of the northeast
# region (Norðaustur), not a missing value
hr_locale <- utils::read.table(
  "data-raw/hr_locale.tsv",
  header = TRUE,
  quote = "\"",
  na.strings = character(0),
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)
stopifnot(
  !anyNA(hr_locale),
  identical(hr_locale[hr_locale$key == "NE", "is"], "NA")
)
save(hr_locale, file = "data/hr_locale.rda", compress = "xz", version = 2)
