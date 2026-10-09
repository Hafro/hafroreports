# Reshape wide assessment data into long format

Pivots a wide assessment tibble (as returned by
[`hr_assessment_from_sag`](https://hafro.github.io/hafroreports/reference/hr_assessment_from_sag.md)
or assembled manually) into the long format used by the advice plotting
functions. Adds localised ordered factor labels for each assessment
variable in both English and Icelandic.

## Usage

``` r
hr_advice_data_assessment(assessment, labels = NULL)
```

## Arguments

- assessment:

  A wide-format tibble with columns `year`, `species`,
  `assessment_year`, and columns named using the pattern `<stat>_<key>`
  (e.g. `median_SSB`, `low_HR`).

- labels:

  Labels replacing the default ones, e.g. for an index-based
  (category 3) stock
  `list(en = c(recruitment = "Juvenile index"), is = c(recruitment = "Nýliðunarvísitala"))`,
  named by key. Default `NULL`.

## Value

A long-format tibble with columns `year`, `species`, `assessment_year`,
`key`, `low`, `median`, `high`, `label.is`, and `label.en`.
