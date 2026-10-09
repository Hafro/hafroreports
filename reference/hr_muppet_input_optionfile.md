# Prepare a MUPPET option file for a given assessment year

Takes a template MUPPET option file (as a single character string) and
updates the key year and age settings using `rmuppet::line_replace`.
Returns a named list suitable for passing to
[`hr_muppet_run`](https://hafro.github.io/hafroreports/reference/hr_muppet_run.md)
as part of the input file set.

## Usage

``` r
hr_muppet_input_optionfile(
  opt_file,
  out_name,
  year_end,
  age_end = 10,
  plus_group = 1
)
```

## Arguments

- opt_file:

  Character string containing the full text of the MUPPET option file
  template. Lines are delimited by `"\n"`.

- out_name:

  Character. Name to use as the key in the returned list, typically the
  relative path to the option file within the MUPPET run directory (e.g.
  `"params/had.dat.opt"`).

- year_end:

  Integer. The last assessment year. Used to set the last data year,
  last optimisation year, last SMH year, and last SMB year.

- age_end:

  Integer. The last model age. Default is `10`.

- plus_group:

  Integer flag (0 or 1) indicating whether the last age is a plus group.
  Default is `1`.

## Value

A named list with one element: `out_name` mapped to the updated option
file text.
