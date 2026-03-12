hr_sam_dat <- function(
  model_dat = NULL,
  minage = NULL,
  maxage = NULL,
  cn = hr_sam_cn(model_dat, minage, maxage),
  cw = hr_sam_cw(model_dat, minage, maxage),
  smb = hr_sam_smb(model_dat, minage, maxage),
  smh = hr_sam_smh(model_dat, minage, maxage),
  sw = hr_sam_sw(model_dat, minage, maxage),
  mo = hr_sam_mo(model_dat, minage, maxage),
  lf = hr_sam_lf(model_dat, cn),
  pf = hr_sam_pf(model_dat, cn),
  pm = hr_sam_pm(model_dat, cn),
  nm = hr_sam_nm(model_dat, minage, maxage)
) {
  dat <- stockassessment::setup.sam.data(
    surveys = list(spring = smb, autumn = smh),
    residual.fleet = cn,
    prop.mature = mo,
    stock.mean.weight = sw,
    catch.mean.weight = cw,
    dis.mean.weight = cw,
    land.mean.weight = cw,
    prop.f = pf,
    prop.m = pm,
    natural.mortality = nm,
    land.frac = lf
  )

  return(dat)
}


hr_sam_cn <- function(
  model_dat,
  minage,
  maxage
) {
  cn <-
    model_dat |>
    dplyr::mutate(
      catch = dplyr::case_when(
        is.na(catch) | catch == 0 ~ NA_real_,
        TRUE ~ catch
      )
    ) |>
    SAMutils::sam.input(
      "catch",
      age_range = as.numeric(minage:maxage),
      na.fill = NA_real_
    )

  ## blot out the incomplete catches in the interim year
  cn[dim(cn)[1], ] <- NA_real_
  return(cn)
}


hr_sam_cw <- function(
  model_dat,
  minage,
  maxage
) {
  cw <-
    model_dat |>
    dplyr::mutate(
      catch = dplyr::case_when(is.na(catch) ~ NA_real_, TRUE ~ catch)
    ) |>
    SAMutils::sam.input(
      "catch_weight",
      age_range = as.numeric(minage:maxage),
      tail_f = function(x, ...) dplyr::first(x),
      na.fill = 0
    )
  cw[dim(cw)[1], ] <- cw[dim(cw)[1] - 1, ]
  cw <- cw / 1000
  return(cw)
}

hr_sam_smb <- function(
  model_dat,
  minage,
  maxage
) {
  smb <-
    model_dat |>
    dplyr::filter(year > 1984) |>
    SAMutils::sam.input(
      "smb",
      age_range = as.numeric(minage:maxage),
      time_window = c(0.15, 0.2),
      na.fill = NA_real_
    ) |>
    (\(x) (ifelse(x < 0, NA_real_, x) * 1e3))()
  attributes(smb)$time <- c(0.15, 0.2)
  return(smb)
}


hr_sam_smh <- function(
  model_dat,
  minage,
  maxage
) {
  smh <-
    model_dat |>
    dplyr::filter(year > 1994) |>
    # Remove final year from data
    dplyr::filter(year < max(model_dat$year)) |>
    dplyr::mutate(smh = ifelse(age > 10, NA_real_, smh)) |>
    SAMutils::sam.input(
      "smh",
      age_range = as.numeric(minage:maxage),
      #time_window = c(0.15, 0.2),
      na.fill = NA_real_
    ) |>
    (\(x) (ifelse(x < 0, NA_real_, x) * 1e3))()
  attributes(smh)$time <- c(0.75, 0.8)
  smh['2011', ] <- NA_real_
  return(smh)
}

hr_sam_sw <- function(
  model_dat,
  minage,
  maxage
) {
  sw <-
    model_dat |>
    SAMutils::sam.input(
      "stock_weight",
      age_range = as.numeric(minage:maxage),
      tail_f = function(x, ...) dplyr::first(x),
      na.fill = 0.001
    )
  sw <- sw / 1000
  return(sw)
}


hr_sam_mo <- function(
  model_dat,
  minage,
  maxage
) {
  model_dat |>
    SAMutils::sam.input(
      "maturity",
      age_range = as.numeric(minage:maxage),
      tail_f = max,
      na.fill = 1
    )
}


hr_sam_lf <- function(
  model_dat,
  cn
) {
  lf <- array(1, dim = dim(cn))
  dimnames(lf) <- dimnames(cn)
  return(lf)
}


hr_sam_pf <- function(
  model_dat,
  cn
) {
  pf <- array(0.4, dim = dim(cn))
  dimnames(pf) <- dimnames(cn)
  return(pf)
}


hr_sam_pm <- function(
  model_dat,
  cn
) {
  pm <- array(0.3, dim = dim(cn))
  dimnames(pm) <- dimnames(cn)
  return(pm)
}


hr_sam_nm <- function(
  model_dat,
  minage,
  maxage
) {
  model_dat |>
    SAMutils::sam.input(
      "M",
      age_range = as.numeric(1:maxage),
      tail_f = mean,
      na.fill = 0.2
    )
}
