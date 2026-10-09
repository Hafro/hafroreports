# Plot discard rates by gear for a given species

Uses the built-in
[`hr_discards`](https://hafro.github.io/hafroreports/reference/hr_discards.md)
dataset to produce a faceted point-and-errorbar plot of the discard rate
(percentage by weight) by gear and year. An overall total across all
gears is added as an additional facet. Error bars represent approximate
95\\ coefficient of variation.

## Usage

``` r
hr_techreport_plot_discards(species)
```

## Arguments

- species:

  Integer. Species code to filter on (1 = cod, 2 = haddock).

## Value

A `ggplot2` plot object faceted by gear.
