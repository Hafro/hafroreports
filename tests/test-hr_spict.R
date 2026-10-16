if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

if (requireNamespace("spict", quietly = TRUE)) {
  data(pol, package = "spict")
  alb <- pol$albacore
  catch <- data.frame(year = alb$timeC, catch = alb$obsC)
  index <- data.frame(year = alb$timeI, index = alb$obsI)

  ok_group("hr_spict_input", {
    inp <- hr_spict_input(catch, index, catch_years = 1967:1989, index_years = 1967:1989,
      priors = list(logn = c(log(2), 2, 1)), phases = list(logn = -1))
    ok(ut_cmp_identical(inp$timeC, as.numeric(1967:1989)), "catch years")
    ok(ut_cmp_equal(inp$obsI, alb$obsI), "index")
    ok(ut_cmp_identical(inp$phases$logn, -1), "phases")
    ok(ut_cmp_error(hr_spict_input(catch[-5, ], index, 1967:1989, 1967:1989), "identical"), "a gap stops")
    gap <- hr_spict_input(catch[-5, ], index[-5, ], 1967:1989, 1967:1989, allow_gaps = TRUE)
    ok(ut_cmp_identical(length(gap$timeC), 22L), "gaps allowed on request")
  })

  ok_group("hr_spict_fit, manage, advice and results", {
    inp <- hr_spict_input(catch, index, 1967:1989, 1967:1989)
    fit <- suppressWarnings(hr_spict_fit(inp))
    ok(ut_cmp_identical(hr_spict_fit_check(fit)$convergence, 0L), "converged")
    man <- spict::manage(fit, maninterval = c(1990, 1991), maneval = 1992)
    ms <- hr_spict_manage_summary(man)
    ok(any(grepl("ICES advice rule", ms$scenario)), "ICES advice rule scenario")
    adv <- hr_spict_advice(ms)
    ok(ut_cmp_identical(adv, round(ms$est.C[grepl("ICES advice rule", ms$scenario)])), "advice is the rounded ICES rule catch")
    bb <- hr_spict_annual(man, "logBBmsy", "currentF")
    ok(all(bb$year == round(bb$year)) && min(bb$year) == 1967, "annual values from the first year")
    cc <- hr_spict_catch(man, "currentF")
    ok(all(c("time", "dt", "median") %in% names(cc)), "predicted catches")
    tab <- hr_spict_table(spict::sumspict.srefpoints(fit))
    ok("parameter" %in% names(tab), "summary table")
  })
} else {
  ok(TRUE, "spict not installed, skipped")
}
