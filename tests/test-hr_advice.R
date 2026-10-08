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

# Relative (SPiCT-like) assessment: B/B_MSY and F/F_MSY
ut_assessment <- function() {
  d <- expand.grid(year = 2000:2025, key = c("SSB", "F"), assessment_year = 2021:2025)
  d$median <- ifelse(d$key == "SSB", 1.2, 0.8)
  d$low <- d$median * 0.8
  d$high <- d$median * 1.2
  d$label.en <- ifelse(d$key == "SSB", "Biomass", "Fishing mortality")
  d$label.is <- d$label.en
  d
}
ref_rel <- list(MSY_btrigger = 0.5, B_lim = 0.3, F_msy = 1, F_lim = 1.7)

ok_group("hr_advice_plot_ssb: relative biomass", {
  p <- hr_advice_plot_ssb(ut_assessment(), 2025, ref_rel, biomass_scale = 1)
  b <- ggplot2::ggplot_build(p)
  ok(ut_cmp_equal(unique(b$data[[2]]$y), 1.2), "B/B_MSY not divided by 1000")
  ok(
    ut_cmp_equal(sort(c(b$data[[4]]$yintercept, b$data[[5]]$yintercept)), c(0.3, 0.5)),
    "Btrigger and Blim lines"
  )
  p <- hr_advice_plot_ssb(
    transform(ut_assessment(), median = median * 1e3, low = low * 1e3, high = high * 1e3),
    2025,
    list(MSY_btrigger = 0.5, B_lim = 0.3)
  )
  ok(
    ut_cmp_equal(unique(ggplot2::ggplot_build(p)$data[[2]]$y), 1.2),
    "Default: tonnes shown in thousand tonnes, as before"
  )
})

ok_group("hr_advice_plot_fpl / _retro: F_lim", {
  b <- ggplot2::ggplot_build(
    hr_advice_plot_fpl(ut_assessment(), 2025, ref_rel, "F", show_lim = TRUE)
  )
  hl <- unlist(lapply(b$data, function(x) x$yintercept))
  ok(ut_cmp_equal(sort(unique(hl)), c(1, 1.7)), "F_MSY and F_lim lines")
  b <- ggplot2::ggplot_build(hr_advice_plot_fpl(ut_assessment(), 2025, ref_rel, "F"))
  hl <- unlist(lapply(b$data, function(x) x$yintercept))
  ok(ut_cmp_equal(sort(unique(hl)), 1), "Default: no F_lim, as before")
  b <- ggplot2::ggplot_build(
    hr_advice_plot_retro(ut_assessment(), ref_rel, 2025, "F", biomass_scale = 1, show_lim = TRUE)
  )
  ok(ut_cmp_equal(max(b$data[[1]]$y), 1.2), "Retro: B/B_MSY not divided by 1000")
  hl <- unlist(lapply(b$data, function(x) x$yintercept))
  ok(1.7 %in% hl, "Retro: F_lim line")
})

ok_group("hr_advice_ref_table / hr_advice_basis_table: relative stocks", {
  basis <- data.frame(
    ref_point = c("F_msy", "F_lim"),
    render = c("F~MSY~", "F~lim~"),
    approach.en = "MSY", approach.is = "MSY",
    basis.en = "SPiCT", basis.is = "SPiCT"
  )
  ft <- hr_advice_ref_table(list(F_msy = 1, F_lim = 1.7), basis, round_values = FALSE)
  ok(ut_cmp_equal(sort(ft$body$dataset$value), c("1", "1.7")), "Values as they are")
  ft <- hr_advice_ref_table(list(F_msy = 1, F_lim = 1.7), basis)
  ok(ut_cmp_equal(sort(ft$body$dataset$value), c("1", "2")), "Default: rounded, as before")
  ft <- hr_advice_basis_table(
    data.frame(a.en = "Basis", b.en = "F~MSY~ = 1", a.is = "x", b.is = "y"),
    markdown = TRUE
  )
  ok(inherits(ft, "flextable"), "Basis table with markdown")
})

ok_group("hr_advice_data_landings: Nephrops trawl", {
  out <- hr_advice_data_landings(data.frame(
    year = 2020,
    gear_name = c("BMT", "NPT", "LLN"),
    catch = 1e6
  ))
  ok(
    ut_cmp_equal(levels(out$gear.en), c("Longline", "Nephrops trawl", "Bottom trawl")),
    "NPT has a label and its place in the stacking order"
  )
  ok("Humarvarpa" %in% levels(out$gear.is), "Icelandic label")
})

ok_group("hr_advice_plot_index: index A and B", {
  d <- data.frame(year = 2015:2025, key = "SSB", assessment_year = 2025)
  d$median <- seq(1000, 11000, by = 1000)
  d$low <- d$median
  d$high <- d$median
  d$label.en <- "Index"
  d$label.is <- "Vísitala"
  b <- ggplot2::ggplot_build(hr_advice_plot_index(d, 2025, index_ab = TRUE))
  ab <- b$data[[length(b$data)]]
  ok(
    ut_cmp_equal(sort(unique(ab$y)), c(8, 10.5)),
    "Index B = mean of 2021-2023, index A = mean of 2024-2025 (thousand t)"
  )
  b0 <- ggplot2::ggplot_build(hr_advice_plot_index(d, 2025))
  ok(ut_cmp_equal(length(b0$data), length(b$data) - 1), "Default: no lines, as before")
})

ok_group("hr_advice_plot_fpl: points and y label", {
  b <- ggplot2::ggplot_build(
    hr_advice_plot_fpl(ut_assessment(), 2025, ref_rel, "F", points = TRUE, y_label = "F")
  )
  ok(any(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPoint") && !inherits(l$geom, "GeomInteractivePoint"), logical(1))), "Visible points")
  ok(ut_cmp_equal(b$plot$labels$y, "F"), "Y label")
})

ok_group("hr_advice_plot_retro: panel order", {
  p <- hr_advice_plot_retro(ut_assessment(), ref_rel, 2025, "F", biomass_scale = 1, panel_order = c("SSB", "F"))
  ok(ut_cmp_equal(levels(p$data$label), c("Biomass", "Fishing mortality")), "Panels in the given order")
  p <- hr_advice_plot_retro(ut_assessment(), ref_rel, 2025, "F", biomass_scale = 1, panel_order = c("F", "SSB"))
  ok(ut_cmp_equal(levels(p$data$label), c("Fishing mortality", "Biomass")), "...whatever the alphabet says")
})

ok_group("hr_advice_plot_ssb: NA Btrigger", {
  p <- hr_advice_plot_ssb(
    ut_assessment(), 2025,
    list(MGT_btrigger = NA, MSY_btrigger = 0.5, B_lim = 0.3, B_pa = 0.4),
    biomass_scale = 1
  )
  hl <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(x) x$yintercept))
  ok(ut_cmp_equal(sort(hl), c(0.3, 0.5)), "NA MGT Btrigger: MSY Btrigger drawn instead")
  p <- hr_advice_plot_ssb(ut_assessment(), 2025, list(MGT_btrigger = NA, B_lim = 0.3), biomass_scale = 1)
  hl <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(x) x$yintercept))
  ok(ut_cmp_equal(hl, 0.3), "No Btrigger: only B_lim")
})

ok_group("hr_advice_plot_recruitment: scale", {
  d <- data.frame(year = 2000:2005, key = "recruitment", assessment_year = 2025,
    median = 500, low = 400, high = 600, label.en = "Juvenile index", label.is = "x")
  b <- ggplot2::ggplot_build(hr_advice_plot_recruitment(d, 2025, scale = 1))
  ok(ut_cmp_equal(unique(b$data[[1]]$y), 500), "Index as it is")
  b <- ggplot2::ggplot_build(hr_advice_plot_recruitment(d, 2025))
  ok(ut_cmp_equal(unique(b$data[[1]]$y), 0.5), "Default: thousands to millions, as before")
})
