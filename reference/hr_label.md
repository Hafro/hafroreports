# Retrieve a localised label

Looks up a display label from the
[`hr_locale`](https://hafro.github.io/hafroreports/reference/hr_locale.md)
translation table using the current language (set via
`options(hr.lang = "en")` or `"is"`). Falls back to the English value if
the requested language is missing, and to the raw key if no translation
exists at all.

## Usage

``` r
hr_label(key, ..., bold = FALSE)
```

## Arguments

- key:

  A character string identifying the label to look up. Must match a row
  in
  [`hr_locale`](https://hafro.github.io/hafroreports/reference/hr_locale.md).

- ...:

  Additional arguments passed to label constructors. For the
  `"recruitment_age"` key, the first argument is the numeric age.

- bold:

  Logical. If `TRUE`, wraps the returned string in a `bold()` call
  suitable for use in `ggplot2` plot titles via `parse = TRUE`. Default
  is `FALSE`.

## Value

A character string (or language object when `bold = TRUE`) containing
the localised label.

## Details

The special key `"recruitment_age"` constructs a label of the form
*"Recruitment (age N)"* / *"Nýliðun (N árs/ára)"* using the first `...`
argument as the age.
