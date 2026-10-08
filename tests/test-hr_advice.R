if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

ok_group("hr_advice_data_tac: no foreign landings", {
  out <- hr_advice_data_tac(
    advice_hist = data.frame(
      assessment_year = 2024:2025,
      advice_period = c("2024/2025", "2025/2026"),
      advice = c(100, 110)
    ),
    tac_hist = data.frame(assessment_year = 2024:2025, tac = c(100, 110)),
    landings_by_fishing_year_country = data.frame(
      fishing_year = c("2024/2025", "2024/2025"),
      country = "Iceland",
      catch = c(40000, 50000)
    )
  )
  ok(ut_cmp_equal(out$icelandic, c(90, NA)), "Icelandic landings")
  ok(ut_cmp_equal(out$foreign, c(NA_real_, NA_real_)), "No foreign column in the data: NA")
  ok(ut_cmp_equal(out$total, c(90, NA)), "Total is the Icelandic landings")
})
