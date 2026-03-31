#' Assemble SAM input data for Icelandic haddock
#'
#' Combines catch-at-age, weight, survey, maturity, and natural mortality
#' matrices into a SAM data object. Each component has a default value
#' computed by the corresponding \code{hr_sam_*} helper, but can be
#' overridden individually.
#'
#' @param model_dat A data frame of model input data containing columns for
#'   year, age, catch, catch_weight, stock_weight, maturity, smb (spring
#'   survey), smh (autumn survey), and natural mortality (M).
#' @param minage Minimum age to include in the assessment.
#' @param maxage Maximum age to include in the assessment.
#' @param cn Catch-at-age matrix; defaults to \code{hr_sam_cn(model_dat, minage, maxage)}.
#' @param cw Catch mean weight matrix; defaults to \code{hr_sam_cw(model_dat, minage, maxage)}.
#' @param smb Spring (SMB) survey index matrix; defaults to \code{hr_sam_smb(model_dat, minage, maxage)}.
#' @param smh Autumn (SMH) survey index matrix; defaults to \code{hr_sam_smh(model_dat, minage, maxage)}.
#' @param sw Stock mean weight matrix; defaults to \code{hr_sam_sw(model_dat, minage, maxage)}.
#' @param mo Proportion mature matrix; defaults to \code{hr_sam_mo(model_dat, minage, maxage)}.
#' @param lf Proportion of fishing before spawning matrix; defaults to \code{hr_sam_lf(model_dat, cn)}.
#' @param pf Proportion of F before spawning matrix; defaults to \code{hr_sam_pf(model_dat, cn)}.
#' @param pm Proportion of M before spawning matrix; defaults to \code{hr_sam_pm(model_dat, cn)}.
#' @param nm Natural mortality matrix; defaults to \code{hr_sam_nm(model_dat, minage, maxage)}.
#' @return A SAM data object as returned by
#'   \code{\link[stockassessment]{setup.sam.data}}.
#' @export
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


#' Build SAM catch-at-age matrix
#'
#' Extracts the catch-at-age matrix from \code{model_dat}, replacing zero and
#' missing catches with \code{NA}. The final (interim) year row is set to
#' \code{NA} to avoid using incomplete data.
#'
#' @param model_dat A data frame with columns \code{year}, \code{age}, and
#'   \code{catch}.
#' @param minage Minimum age to include.
#' @param maxage Maximum age to include.
#' @return A numeric matrix with years as rows and ages as columns.
#' @export
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


#' Build SAM catch mean weight matrix
#'
#' Extracts the catch mean weight-at-age matrix from \code{model_dat}.
#' Weights are converted from grams to kilograms. The final year row is
#' filled with the penultimate year's values, and missing weights are
#' set to zero.
#'
#' @param model_dat A data frame with columns \code{year}, \code{age},
#'   \code{catch}, and \code{catch_weight}.
#' @param minage Minimum age to include.
#' @param maxage Maximum age to include.
#' @return A numeric matrix (kg) with years as rows and ages as columns.
#' @export
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

#' Build SAM spring (SMB) survey index matrix
#'
#' Extracts the spring groundfish survey (SMB) abundance indices from
#' \code{model_dat}. Only data from 1985 onwards are used. Negative
#' values are replaced with \code{NA} and indices are scaled by 1000.
#' The timing attribute is set to weeks 0.15--0.20 of the year.
#'
#' @param model_dat A data frame with columns \code{year}, \code{age},
#'   and \code{smb}.
#' @param minage Minimum age to include.
#' @param maxage Maximum age to include.
#' @return A numeric matrix with years as rows and ages as columns, with
#'   a \code{time} attribute set to \code{c(0.15, 0.20)}.
#' @export
hr_sam_smb <- function(
  model_dat,
  minage,
  maxage
) {
  # NSE variables
  year <- NULL

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


#' Build SAM autumn (SMH) survey index matrix
#'
#' Extracts the autumn groundfish survey (SMH) abundance indices from
#' \code{model_dat}. Only data from 1995 onwards are included, and the
#' final year is always excluded. Ages above 10 are set to \code{NA}.
#' Negative values are replaced with \code{NA} and indices are scaled by 1000.
#' The 2011 survey row is blanked due to data quality issues.
#' The timing attribute is set to weeks 0.75--0.80 of the year.
#'
#' @param model_dat A data frame with columns \code{year}, \code{age},
#'   and \code{smh}.
#' @param minage Minimum age to include.
#' @param maxage Maximum age to include.
#' @return A numeric matrix with years as rows and ages as columns, with
#'   a \code{time} attribute set to \code{c(0.75, 0.80)}.
#' @export
hr_sam_smh <- function(
  model_dat,
  minage,
  maxage
) {
  # NSE variables
  year <- age <- NULL

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

#' Build SAM stock mean weight matrix
#'
#' Extracts the stock mean weight-at-age matrix from \code{model_dat}.
#' Weights are converted from grams to kilograms. Missing weights are
#' filled with a small non-zero value (0.001 kg).
#'
#' @param model_dat A data frame with columns \code{year}, \code{age},
#'   and \code{stock_weight}.
#' @param minage Minimum age to include.
#' @param maxage Maximum age to include.
#' @return A numeric matrix (kg) with years as rows and ages as columns.
#' @export
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


#' Build SAM proportion mature matrix
#'
#' Extracts the proportion mature at age from \code{model_dat}.
#' Missing maturity values are filled with 1 (fully mature).
#'
#' @param model_dat A data frame with columns \code{year}, \code{age},
#'   and \code{maturity}.
#' @param minage Minimum age to include.
#' @param maxage Maximum age to include.
#' @return A numeric matrix with years as rows and ages as columns.
#' @export
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


#' Build SAM landing fraction matrix
#'
#' Creates a matrix of landing fractions (proportion of catch that is
#' landed, i.e. not discarded) with the same dimensions and dimnames as
#' the catch-at-age matrix. All values are set to 1, indicating no
#' discarding.
#'
#' @param model_dat Unused; retained for consistency with other
#'   \code{hr_sam_*} helpers.
#' @param cn The catch-at-age matrix returned by \code{\link{hr_sam_cn}},
#'   used to determine the required dimensions.
#' @return A numeric matrix of ones with the same shape as \code{cn}.
#' @export
hr_sam_lf <- function(
  model_dat,
  cn
) {
  lf <- array(1, dim = dim(cn))
  dimnames(lf) <- dimnames(cn)
  return(lf)
}


#' Build SAM proportion of F before spawning matrix
#'
#' Creates a matrix with the same dimensions as the catch-at-age matrix,
#' filled with 0.4, indicating that 40\% of fishing mortality occurs
#' before the spawning season.
#'
#' @param model_dat Unused; retained for consistency with other
#'   \code{hr_sam_*} helpers.
#' @param cn The catch-at-age matrix returned by \code{\link{hr_sam_cn}},
#'   used to determine the required dimensions.
#' @return A numeric matrix of 0.4 with the same shape as \code{cn}.
#' @export
hr_sam_pf <- function(
  model_dat,
  cn
) {
  pf <- array(0.4, dim = dim(cn))
  dimnames(pf) <- dimnames(cn)
  return(pf)
}


#' Build SAM proportion of M before spawning matrix
#'
#' Creates a matrix with the same dimensions as the catch-at-age matrix,
#' filled with 0.3, indicating that 30\% of natural mortality occurs
#' before the spawning season.
#'
#' @param model_dat Unused; retained for consistency with other
#'   \code{hr_sam_*} helpers.
#' @param cn The catch-at-age matrix returned by \code{\link{hr_sam_cn}},
#'   used to determine the required dimensions.
#' @return A numeric matrix of 0.3 with the same shape as \code{cn}.
#' @export
hr_sam_pm <- function(
  model_dat,
  cn
) {
  pm <- array(0.3, dim = dim(cn))
  dimnames(pm) <- dimnames(cn)
  return(pm)
}


#' Build SAM natural mortality matrix
#'
#' Extracts the natural mortality at age from \code{model_dat}.
#' Ages 1 through \code{maxage} are included. Missing values are
#' filled with 0.2.
#'
#' @param model_dat A data frame with columns \code{year}, \code{age},
#'   and \code{M}.
#' @param minage Minimum age (currently unused; ages always start at 1).
#' @param maxage Maximum age to include.
#' @return A numeric matrix with years as rows and ages as columns.
#' @export
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
