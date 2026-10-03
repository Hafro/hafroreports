# Read without NA strings: "NA" is the Icelandic label of the northeast
# region (Norðaustur), not a missing value
hr_locale <- utils::read.table(
  "hr_locale.tsv",
  header = TRUE,
  quote = "\"",
  na.strings = character(0),
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)
