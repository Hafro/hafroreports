#' SPiCT surplus production model helpers
#'
#' Input, fit, management scenarios, results and advice tables of a SPiCT
#' assessment (package \code{spict}), lifted from the starry ray port
#' (\code{12-rjr}, \code{rjr_spict_*()}) for the stocks assessed with SPiCT
#' (starry ray, common whelk) and exploratory fits (offshore shrimp).
#'
#' @param catch Data frame with \code{year} and \code{catch} (tonnes).
#' @param index Data frame with \code{year} and \code{index}.
#' @param priors,phases,ini Lists set as \code{inp$priors}, \code{inp$phases}
#'   and \code{inp$ini} (\code{NULL} for none).
#' @param catch_years,index_years Years of the catch and the index used.
#' @param allow_gaps If \code{FALSE} (default), stop when a year of
#'   \code{catch_years} or \code{index_years} is missing from the data; if
#'   \code{TRUE}, use the years there are (SPiCT fits series with gaps; the
#'   old whelk input had no catch or index in 2020).
#' @return \subsection{hr_spict_input}{A SPiCT input list}
#' @name hr_spict
#' @export
hr_spict_input <- function(
  catch,
  index,
  catch_years,
  index_years,
  priors = NULL,
  phases = NULL,
  ini = NULL,
  allow_gaps = FALSE
) {
  # NSE variables
  year <- NULL

  lnd <- catch |>
    dplyr::filter(year %in% catch_years, !is.na(catch)) |>
    dplyr::arrange(year)
  idx <- index |>
    dplyr::filter(year %in% index_years, !is.na(index)) |>
    dplyr::arrange(year)
  if (!isTRUE(allow_gaps)) {
    stopifnot(
      identical(as.numeric(lnd$year), as.numeric(catch_years)),
      identical(as.numeric(idx$year), as.numeric(index_years))
    )
  }
  inp <- list(
    timeC = lnd$year,
    obsC = lnd$catch,
    timeI = idx$year,
    obsI = idx$index
  )
  if (!is.null(priors)) inp$priors <- priors
  if (!is.null(phases)) inp$phases <- phases
  if (!is.null(ini)) inp$ini <- ini
  inp
}

#' @param inp A SPiCT input list.
#' @return \subsection{hr_spict_fit}{The fit (\code{spict::fit.spict()}),
#'   with a warning if it didn't converge or has non-finite standard errors}
#' @rdname hr_spict
#' @export
hr_spict_fit <- function(inp) {
  if (!requireNamespace("spict", quietly = TRUE)) {
    stop("hr_spict_fit() needs the spict package")
  }
  fit <- spict::fit.spict(inp)
  if (fit$opt$convergence != 0 || !all(is.finite(fit$sd))) {
    warning(
      "SPiCT fit: convergence ", fit$opt$convergence,
      ", finite sd ", all(is.finite(fit$sd))
    )
  }
  fit
}

#' @param fit A SPiCT fit.
#' @return \subsection{hr_spict_fit_check}{A one-row tibble: convergence,
#'   objective, whether all standard errors are finite, spict version}
#' @rdname hr_spict
#' @export
hr_spict_fit_check <- function(fit) {
  tibble::tibble(
    convergence = fit$opt$convergence,
    objective = fit$opt$objective,
    finite_sd = all(is.finite(fit$sd)),
    spict_version = as.character(utils::packageVersion("spict"))
  )
}

#' @param year,start_month The year and month a management period (e.g. a
#'   fishing year from September) starts.
#' @return \subsection{hr_spict_management_start}{The start on the model's
#'   time grid (\code{dteuler}; \code{spict::manage()} shortens a period off
#'   the grid)}
#' @rdname hr_spict
#' @export
hr_spict_management_start <- function(fit, year, start_month = 1) {
  dt <- fit$inp$dteuler
  round((year + (start_month - 1) / 12) / dt) * dt
}

#' @param man The management scenarios (\code{spict::manage()}).
#' @return \subsection{hr_spict_manage_summary}{The scenarios of
#'   \code{spict::sumspict.manage()} (catch rounded to 0.1 t, B/B_MSY and
#'   F/F_MSY to 0.01) as a tibble}
#' @rdname hr_spict
#' @export
hr_spict_manage_summary <- function(man) {
  est <- spict::sumspict.manage(man)$est
  tibble::tibble(
    scenario = rownames(est),
    est.C = est[, 1],
    est.B.Bmsy = est[, 2],
    est.F.Fmsy = est[, 3]
  )
}

#' @param pattern Regular expression of the scenario of the advice. Default
#'   the ICES advice rule.
#' @return \subsection{hr_spict_advice}{The advice: the catch of the
#'   scenario in the management period (0.1 t), rounded to whole tonnes}
#' @rdname hr_spict
#' @export
hr_spict_advice <- function(manage_summary, pattern = "ICES advice rule") {
  tac <- manage_summary$est.C[grepl(pattern, manage_summary$scenario)]
  stopifnot(length(tac) == 1)
  round(tac)
}

#' @param scenario Scenario of \code{man$man} (e.g. \code{"currentF"},
#'   \code{"ices"}).
#' @param par SPiCT parameter, e.g. \code{"logBBmsy"}.
#' @param CI Width of the intervals.
#' @return \subsection{hr_spict_annual}{The parameter at the start of each
#'   year (every \code{1/dteuler}-th time step) with its interval: a tibble
#'   with \code{year}, \code{low}, \code{median}, \code{high}}
#' @rdname hr_spict
#' @export
hr_spict_annual <- function(man, par, scenario = "currentF", CI = 0.95) {
  sc <- man$man[[scenario]]
  stopifnot(!is.null(sc))
  x <- spict::get.par(par, sc, exp = TRUE, CI = CI)
  step <- round(1 / sc$inp$dteuler)
  x <- x[seq(1, nrow(x), by = step), , drop = FALSE]
  tibble::tibble(
    year = as.integer(round(as.numeric(rownames(x)))),
    low = x[, "ll"],
    median = x[, "est"],
    high = x[, "ul"]
  )
}

#' @return \subsection{hr_spict_catch}{Predicted catches of the scenario
#'   (\code{logCpred}) with their times and lengths (\code{timeCpred},
#'   \code{dtcp}) and intervals: a tibble with \code{time}, \code{dt},
#'   \code{low}, \code{median}, \code{high}}
#' @rdname hr_spict
#' @export
hr_spict_catch <- function(man, scenario = "currentF", CI = 0.95) {
  sc <- man$man[[scenario]]
  stopifnot(!is.null(sc))
  cc <- spict::get.par("logCpred", sc, exp = TRUE, CI = CI)
  tibble::tibble(
    time = sc$inp$timeCpred,
    dt = sc$inp$dtcp,
    low = cc[, "ll"],
    median = cc[, "est"],
    high = cc[, "ul"]
  )
}

#' @param x A matrix of SPiCT summary results, e.g.
#'   \code{spict::sumspict.parest()} or \code{sumspict.srefpoints()}.
#' @return \subsection{hr_spict_table}{The matrix as a tibble with the
#'   parameter names in \code{parameter}}
#' @rdname hr_spict
#' @export
hr_spict_table <- function(x) {
  tibble::tibble(parameter = trimws(rownames(x)), tibble::as_tibble(x))
}

#' @param results Data frame with \code{year} and \code{median_SSB},
#'   \code{low_SSB}, \code{high_SSB} (B/B_MSY) and \code{median_F},
#'   \code{low_F}, \code{high_F} (F/F_MSY).
#' @param species Species code.
#' @param assessment_year The assessment year.
#' @return \subsection{hr_spict_assessment_summary}{Rows of the assessment
#'   history: B/B_MSY as SSB and F/F_MSY as F up to the assessment year,
#'   without F in the assessment year}
#' @rdname hr_spict
#' @export
hr_spict_assessment_summary <- function(results, species, assessment_year) {
  # NSE variables
  year <- median_SSB <- low_SSB <- high_SSB <- median_F <- low_F <- high_F <- NULL

  results |>
    dplyr::filter(year <= assessment_year) |>
    dplyr::transmute(
      year,
      species = species,
      median_SSB, low_SSB, high_SSB,
      median_F = ifelse(year < assessment_year, median_F, NA_real_),
      low_F = ifelse(year < assessment_year, low_F, NA_real_),
      high_F = ifelse(year < assessment_year, high_F, NA_real_),
      assessment_year = assessment_year
    )
}
