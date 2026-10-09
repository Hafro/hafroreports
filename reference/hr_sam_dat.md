# Assemble SAM input data

Combines catch-at-age, weight, survey, maturity, and natural mortality
matrices into a SAM data object. Each component has a default value
computed by the corresponding `hr_sam_*` helper, but can be overridden
individually.

## Usage

``` r
hr_sam_dat(
  model_dat = NULL,
  minage = NULL,
  maxage = NULL,
  cn = hr_sam_cn(model_dat, minage, maxage),
  cw = hr_sam_cw(model_dat, minage, maxage),
  surveys = list(spring = hr_sam_smb(model_dat, minage, maxage)),
  sw = hr_sam_sw(model_dat, minage, maxage),
  mo = hr_sam_mo(model_dat, minage, maxage),
  lf = hr_sam_lf(model_dat, cn),
  pf = hr_sam_pf(model_dat, cn),
  pm = hr_sam_pm(model_dat, cn),
  nm = hr_sam_nm(model_dat, minage, maxage)
)
```

## Arguments

- model_dat:

  A data frame of model input data containing columns for year, age,
  catch, catch_weight, stock_weight, maturity, smb (spring survey), smh
  (autumn survey, if used), and natural mortality (M).

- minage:

  Minimum age to include in the assessment.

- maxage:

  Maximum age to include in the assessment.

- cn:

  Catch-at-age matrix; defaults to
  `hr_sam_cn(model_dat, minage, maxage)`.

- cw:

  Catch mean weight matrix; defaults to
  `hr_sam_cw(model_dat, minage, maxage)`.

- surveys:

  Named list of survey index matrices (the names are the SAM fleet
  names). Defaults to the spring survey only,
  `list(spring = hr_sam_smb(model_dat, minage, maxage))`; add e.g.
  `autumn = hr_sam_smh(...)` for more surveys.

- sw:

  Stock mean weight matrix; defaults to
  `hr_sam_sw(model_dat, minage, maxage)`.

- mo:

  Proportion mature matrix; defaults to
  `hr_sam_mo(model_dat, minage, maxage)`.

- lf:

  Landed fraction matrix; defaults to `hr_sam_lf(model_dat, cn)`.

- pf:

  Proportion of F before spawning matrix; defaults to
  `hr_sam_pf(model_dat, cn)` (0).

- pm:

  Proportion of M before spawning matrix; defaults to
  `hr_sam_pm(model_dat, cn)` (0).

- nm:

  Natural mortality matrix; defaults to
  `hr_sam_nm(model_dat, minage, maxage)`.

## Value

A SAM data object as returned by `setup.sam.data`.
