# Advice sheet banner

Advice sheet banner

## Usage

``` r
hr_advice_banner(
  year_end,
  tac,
  tac_last_year,
  note = "",
  publication_date,
  period = "fishing_year",
  publication_note = ""
)
```

## Arguments

- year_end:

  the assessment year

- tac:

  TAC

- tac_last_year:

  last years tac

- note:

  banner note, if needed

- publication_date:

  `BigD::fdt()`-parsable date string with the publication date

- period:

  Advisory period. Default is 'fishing_year', other options: 'annual',
  '2year', '3year', '5year'

- publication_note:

  publication note, if needed
