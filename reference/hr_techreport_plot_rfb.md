# Tech report figures of the rfb rule

The four figures of the category 3 (rfb rule) tech reports, as the old
`dls_iplotter()`, `dls_Lplotter()`, `dls_MLplotter()` and
`dls_Fplotter()` that the stock repositories each kept a copy of:

- `hr_techreport_plot_rfb_index()`: the biomass index of the rule with
  its 95\\ index B (the three years before) in red, I_trigger (dashed)
  and I_loss (a point at the lowest index, or a line);

- `hr_techreport_plot_rfb_lc()`: the length distribution of the catch
  over all years, with L_c and L_F=M (red bars), 50\\ modal abundance
  (dashed) and L_inf (dashed);

- `hr_techreport_plot_rfb_ml()`: the length distribution of the catch in
  the last data year, with L_F=M and the mean length above L_c;

- `hr_techreport_plot_rfb_f()`: the fishing pressure proxy L_F=M /
  L_mean by year, with the F_MSY proxy (1).

Labels follow `getOption("hr.lang")` through
[`hr_label`](https://hafro.github.io/hafroreports/reference/hr_label.md).

## Usage

``` r
hr_techreport_rfb_theme(base_size = 3)

hr_techreport_plot_rfb_index(
  survey_index,
  index,
  rfb_prognosis,
  ref_points,
  assessment_year,
  iloss = c("point", "line"),
  iloss_year = NULL,
  iloss_label = "I[loss]",
  label_x = NULL,
  label_y = c(trigger = 1.2, iloss = 0.75),
  unit = 1000,
  ci = TRUE,
  points = FALSE,
  x_limits = NULL,
  title = NULL,
  base_size = 3
)

hr_techreport_plot_rfb_lc(
  ldist,
  ref_points,
  x_limits = c(0, 70),
  base_size = 3
)

hr_techreport_plot_rfb_ml(
  ldist,
  ref_points,
  data_year,
  rfb_prognosis = NULL,
  x_limits = c(0, 70),
  legend_position = c(0.8, 0.8),
  base_size = 3
)

hr_techreport_plot_rfb_f(
  ldist,
  ref_points,
  year_start = -Inf,
  year_end = Inf,
  x_limits = NULL,
  y_limits = NULL,
  base_size = 3
)
```

## Arguments

- base_size:

  Base text size (ggplot2 text size units).

- survey_index:

  Survey indices with columns `index`, `year`, `b` (tonnes) and `b_cv`.

- index:

  Name of the index of the rule in `survey_index`.

- rfb_prognosis:

  The rule's components (columns `component` and `value`), as
  `dlsrules::rfb_advice()`: `index_A` and `index_B` are drawn, and for
  `hr_techreport_plot_rfb_ml()` (if given) `mean_catch_length`.

- ref_points:

  Named list of reference points: `I_trigger`, `I_lim` (I_loss); `Lc`,
  `Linf`, `target_reference_length` (L_F=M, or `trl`) and `F_msy_proxy`
  (default 1).

- assessment_year:

  The assessment year: the index is drawn up to it.

- iloss:

  How I_loss is shown: `"point"` (default), a point at the lowest index
  (or `iloss_year`); or `"line"`, a line at `ref_points$I_lim` (when
  I_loss is not an index value, e.g. a mean of years).

- iloss_year:

  Year of the I_loss point. Default `ref_points$I_lim_year` (as
  `dlsrules::ref_points_iloss()`), or else the year of the lowest index
  up to `assessment_year`.

- iloss_label:

  Plotmath label of I_loss. Default `"I[loss]"`.

- label_x:

  Named numeric vector, the years at which the `trigger` and `iloss`
  labels are written. Default: the year of I_loss (`iloss_year`), less
  one year for the I_trigger label.

- label_y:

  Named numeric vector, the heights of the `trigger` and `iloss` labels
  as multiples of their values. Default
  `c(trigger = 1.2, iloss = 0.75)`.

- unit:

  Divisor of the index for the y axis: 1000 (default, thousand tonnes)
  or 1 (tonnes).

- ci:

  Draw the 95\\ `TRUE`.

- points:

  Also draw the yearly values as points. Default `FALSE`.

- x_limits:

  Lengths (cm) or years the x axis spans at least, or `NULL`.

- title:

  Plot title. Default `hr_label("rfb_biomass_index")`.

- ldist:

  Length distributions of the catch of the rule, with columns `year`,
  `length` (cm) and `n`.

- data_year:

  The catch year of the rule (the year before the assessment year).

- legend_position:

  Legend position inside the panel. Default `c(0.8, 0.8)`.

- year_start, year_end:

  First and last catch year of the series.

- y_limits:

  `NULL` (default) for the default axis; otherwise `c(lower, upper)`,
  the least range of the y axis, widened to the values (rounded out to
  0.1), with breaks every 0.1.

## Value

A `ggplot2` plot object.

## Functions

- `hr_techreport_rfb_theme()`: Theme of the rfb figures: axis text
  `2 * base_size`, bold axis titles and title `3 * base_size`.

- `hr_techreport_plot_rfb_index()`: The biomass index of the rule.

- `hr_techreport_plot_rfb_lc()`: The length distribution of the catch
  over all years with the length-based reference points.

- `hr_techreport_plot_rfb_ml()`: The length distribution of the catch in
  `data_year` with L_F=M (solid) and the mean length above L_c (dashed).
  The mean length is `mean_catch_length` of `rfb_prognosis` if given
  (the rule's value), otherwise computed from `ldist`.

- `hr_techreport_plot_rfb_f()`: The fishing pressure proxy L_F=M /
  L_mean by year (mean length above L_c of `ldist`, years `year_start`
  to `year_end`).
