# Two panel plot

Two panel plot

## Usage

``` r
two_panel_plot(
  dat,
  prop.text = hr_label("prop_of_measure"),
  total.text = "%s (kt)",
  y = hr_label("catches"),
  x = hr_label("year"),
  fill = "",
  cols = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7", "black"),
  split_by_gear = FALSE
)
```

## Arguments

- dat:

  input data frame

- prop.text:

  Proportion text

- total.text:

  total text

- y:

  type of data label

- x:

  Time label

- fill:

  Fill label

- cols:

  fill colours

- split_by_gear:

  Boolean, Use mfdb_gear_code as a facet?

## Value

ggplot object
