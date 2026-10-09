# Prepare landings data for advice plots and tables

Summarises landings from a gear-grouped data frame into thousands of
tonnes per year per gear, and adds localised English and Icelandic gear
name factors in the display order used by advice sheet figures.

## Usage

``` r
hr_advice_data_landings(landings_by_gear)
```

## Arguments

- landings_by_gear:

  A data frame or lazy table with columns `year`, `gear_name` (e.g.
  `"BMT"`, `"NPT"` (Nephrops trawl), `"DSE"`, `"LLN"`, `"GIL"`, `"HLN"`,
  `"Other"`, as produced by
  [`pax::pax_landings_by_gear()`](https://rdrr.io/pkg/pax/man/pax_landings.html)
  with the stock's gear groups), and `catch` (kg). Unknown gear names
  are shown under their own name.

## Value

A tibble with columns `year`, `gear_name`, `tonnes` (landings in
thousands of tonnes), `gear.is` (ordered Icelandic gear factor), and
`gear.en` (ordered English gear factor).
