if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

# A toy UWTV index held as SSB in an assessment history (millions)
hist <- data.frame(
  year = c(2016:2021, 2023, 2025),
  species = 40,
  median_SSB = c(636, 613, 480, 518, 416, 448, 814, 1454),
  low_SSB = c(611, 597, 469, 504, 399, 434, 773, 1324),
  high_SSB = c(661, 628, 492, 531, 433, 462, 854, 1584),
  median_HR = c(1.91, 1.51, 1.17, 0.37, 0.35, 0.19, 0, 0),
  assessment_year = 2026
)
d <- hr_advice_data_assessment(hist)

ok_group("hr_advice_plot_index: defaults unchanged", {
  p <- hr_advice_plot_index(d, 2026)
  b <- ggplot2::ggplot_build(p)
  ok(ut_cmp_equal(max(b$data[[1]]$ymax), 1.584), "Band in thousands by default")
  ok(ut_cmp_equal(nrow(b$data[[2]]), 8L), "Line over all years by default")
})

ok_group("hr_advice_plot_index: divisor, point_years, year_start", {
  p <- hr_advice_plot_index(d, 2026, divisor = 1, point_years = c(2023, 2025), year_start = 2017)
  b <- ggplot2::ggplot_build(p)
  ok(ut_cmp_equal(sort(unique(b$data[[2]]$x)), 2017:2021), "Line over the annual years after year_start")
  ok(ut_cmp_equal(sort(b$data[[3]]$x), c(2023, 2025)), "Points with error bars in the biennial years")
  ok(ut_cmp_equal(max(b$data[[3]]$ymax), 1584), "Not divided")
})

ok_group("hr_advice_table_no_rule", {
  for (lang in c("en", "is")) {
    withr::with_options(list(hr.lang = lang), {
      ft <- hr_advice_table_no_rule(0, 0, 2025, 2026)
      ok(inherits(ft, "flextable"), paste("A flextable,", lang))
      ok(ut_cmp_identical(ft$body$dataset[[3]], "-"), "No change from 0")
    })
  }
  ft <- hr_advice_table_no_rule(100, 80, 2025, 2026)
  ok(ut_cmp_identical(ft$body$dataset[[3]], "-20 %"), "Change in percent")
})

ok_group("UWTV tech report figures build", {
  ia <- expand.grid(year = c(2016:2021, 2023, 2025), area_name = c("A", "B"))
  ia$burrows_millions <- 100; ia$low <- 90; ia$high <- 110
  ok(inherits(ggplot2::ggplot_build(hr_techreport_plot_uwtv_index_area(ia, point_years = c(2023, 2025))), "ggplot_built"), "index by area")
  st <- data.frame(year = rep(c(2016, 2018), each = 10), dens = runif(20))
  ok(inherits(ggplot2::ggplot_build(hr_techreport_plot_station_density(st, years = 2016:2018)), "ggplot_built"), "station density")
  lr <- data.frame(year = c(1955, 1955, 1970, 1970), area = c("iceland", "other", "SA-mið", "SV-mið"), catch = c(1, 2, 3, 4), cpue = c(NA, NA, 10, 20))
  ok(inherits(ggplot2::ggplot_build(hr_techreport_plot_landings_region(lr, c("SA-mið" = "SE", "SV-mið" = "SW"))), "ggplot_built"), "landings by region")
  ok(inherits(ggplot2::ggplot_build(hr_techreport_plot_region_series(lr, c("SA-mið" = "SE", "SV-mið" = "SW"), year_start = 1960)), "ggplot_built"), "CPUE by region")
  ok(inherits(ggplot2::ggplot_build(hr_techreport_plot_series(data.frame(year = 1:3, value = 3:1))), "ggplot_built"), "series")
})
