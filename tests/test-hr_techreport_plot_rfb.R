if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

# A survey index with its lowest value in 1995, length distributions of the
# catch and the rule's components
survey_index <- data.frame(
  index = "smb_harv",
  year = 1990:2025,
  b = 1000 + 100 * abs(1995 - (1990:2025)),
  b_cv = 0.2
)
survey_index <- rbind(
  survey_index,
  data.frame(index = "smb_total", year = 1990:2025, b = 1, b_cv = 0.1)
)
ldist <- expand.grid(year = 2000:2024, length = 10:60)
ldist$n <- dnorm(ldist$length, 35 + (ldist$year - 2000) / 10, 8) * 1000
ref_points <- list(
  I_trigger = 1400,
  I_lim = 1000,
  Lc = 30,
  Linf = 60,
  target_reference_length = 0.75 * 30 + 0.25 * 60,
  F_msy_proxy = 1
)
rfb_prognosis <- data.frame(
  component = c("index_A", "index_B", "mean_catch_length"),
  value = c(3950, 3700, 41.5)
)
layer_classes <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1))
}

ok_group("hr_techreport_plot_rfb_index", {
  p <- hr_techreport_plot_rfb_index(
    survey_index, "smb_harv", rfb_prognosis, ref_points, assessment_year = 2024
  )
  b <- ggplot2::ggplot_build(p)
  ok(ut_cmp_equal(max(p$data$year), 2024), "Index up to the assessment year")
  ok(ut_cmp_equal(unique(p$data$index), "smb_harv"), "The index of the rule only")
  ok(
    ut_cmp_equal(p$data$upper, p$data$b * exp(1.96 * 0.2)),
    "Log-normal 95% interval from the CV"
  )
  pts <- p$layers[layer_classes(p) == "GeomPoint"][[1]]$data
  ok(ut_cmp_equal(pts$year, 1995), "I_loss point at the lowest index")
  red <- p$layers[[which(layer_classes(p) == "GeomLine")[2]]]$data
  ok(
    ut_cmp_equal(red$year, c(2022.5, 2024, 2019.5, 2022.5)),
    "Index A and B lines spanning their periods"
  )
  ok(ut_cmp_equal(red$value, c(3950, 3950, 3700, 3700)), "Index A and B from the rule")
  ok(ut_cmp_equal(p$labels$y, "Thous. tonnes"), "Thousand tonnes")

  p <- hr_techreport_plot_rfb_index(
    survey_index, "smb_harv", rfb_prognosis, ref_points, 2024,
    iloss = "line", iloss_label = "I[lim]", label_x = c(trigger = 1997),
    unit = 1, ci = FALSE, points = TRUE
  )
  cl <- layer_classes(p)
  ok(!("GeomRibbon" %in% cl), "ci = FALSE: no interval")
  ok(ut_cmp_equal(sum(cl == "GeomHline"), 2L), "iloss = 'line': lines at I_trigger and I_lim")
  ok(ut_cmp_equal(p$layers[[which(cl == "GeomHline")[2]]]$data$yintercept, 1000), "in tonnes")
  txt <- p$layers[cl == "GeomText"]
  ok(ut_cmp_equal(txt[[1]]$data$x, 1997), "I_trigger label where asked")
  ok(ut_cmp_equal(txt[[2]]$data$x, 1995), "I_loss label at the lowest index by default")
  ok(ut_cmp_equal(txt[[2]]$aes_params$label, "I[lim]"), "I_loss label")
  ok(ut_cmp_equal(p$labels$y, "Tonnes"), "Tonnes")

  withr::with_options(list(hr.lang = "is"), {
    p <- hr_techreport_plot_rfb_index(survey_index, "smb_harv", rfb_prognosis, ref_points, 2024)
  })
  ok(ut_cmp_equal(p$labels$title, "Vísitala"), "Icelandic title")
  ok(ut_cmp_equal(p$labels$x, "Ár"), "Icelandic x label")
})

ok_group("hr_techreport_plot_rfb_lc / _ml", {
  p <- hr_techreport_plot_rfb_lc(ldist, ref_points)
  red <- p$layers[[2]]$data
  ok(ut_cmp_equal(red$length, c(30, 38)), "Red bars at L_c and round(L_F=M)")
  ok(ut_cmp_equal(p$labels$title, "Length-based reference points"), "Title")
  withr::with_options(list(hr.lang = "is"), {
    p <- hr_techreport_plot_rfb_lc(ldist, ref_points)
  })
  ok(ut_cmp_equal(p$labels$x, "Lengd (cm)"), "Icelandic x label")

  p <- hr_techreport_plot_rfb_ml(ldist, ref_points, 2024)
  lines <- p$layers[[2]]$data
  d24 <- ldist[ldist$year == 2024 & ldist$length > 30, ]
  ok(
    ut_cmp_equal(lines$x, rep(c(37.5, sum(d24$length * d24$n) / sum(d24$n)), each = 2)),
    "L_F=M and the mean length above L_c of the data year"
  )
  ok(ut_cmp_equal(p$labels$title, "Length distribution, 2024"), "Title with the year")
  # The rule's mean length, and L_F=M named trl
  rp <- ref_points
  rp$target_reference_length <- NULL
  rp$trl <- 37.5
  p <- hr_techreport_plot_rfb_ml(ldist, rp, 2024, rfb_prognosis = rfb_prognosis)
  ok(ut_cmp_equal(p$layers[[2]]$data$x, rep(c(37.5, 41.5), each = 2)), "Mean length from rfb_prognosis")
})

ok_group("hr_techreport_plot_rfb_f", {
  p <- hr_techreport_plot_rfb_f(ldist, ref_points, year_start = 2005, year_end = 2020)
  ok(ut_cmp_equal(range(p$data$year), c(2005, 2020)), "Years")
  d <- ldist[ldist$year == 2010 & ldist$length > 30, ]
  ok(
    ut_cmp_equal(p$data$fpp[p$data$year == 2010], 37.5 / (sum(d$length * d$n) / sum(d$n))),
    "L_F=M / mean length above L_c"
  )
  p <- hr_techreport_plot_rfb_f(ldist, ref_points, y_limits = c(0.95, 1.05))
  lim <- p$scales$get_scales("y")$limits
  ok(ut_cmp_equal(lim[2], 1.05), "y_limits: at least the range given")
  ok(lim[1] <= min(p$data$fpp), "widened to the values")
  ok(ut_cmp_equal(lim[1], floor(min(p$data$fpp) * 10) / 10), "rounded out to 0.1")
})

ok_group("hr_techreport_dsl_basis / plots", {
  survey_ldist <- data.frame(length = 1:50, n = c(rep(10, 49), 1))
  b <- hr_techreport_dsl_basis(ldist, survey_ldist)
  all <- aggregate(n ~ length, ldist, sum)
  ok(ut_cmp_equal(b$Lc, min(all$length[all$n > max(all$n) / 2])), "L_c: first length over 50% of the mode")
  ok(ut_cmp_equal(b$max_len, 50), "Largest survey length")
  ok(ut_cmp_equal(b$Linf, mean(c(b$max_len, b$q99))), "L_inf")
  ok(ut_cmp_equal(b$LF, 0.75 * b$Lc + 0.25 * b$Linf), "L_F=M")
  p <- hr_techreport_plot_dsl_lc(b)
  ok(ggplot2::is_ggplot(p), "DSL length figure")
  p <- hr_techreport_plot_dsl_ml(ldist, b, 1996, 2024, y_limits = c(20, 50))
  ok(
    ut_cmp_equal(
      p$scales$get_scales("x")$breaks,
      c(1996, 2000, 2005, 2010, 2015, 2020, 2024)
    ),
    "Year breaks: first, every 5 years, last"
  )
  ok(ut_cmp_equal(p$layers[[4]]$data$x, 2015), "L_F=M label 9 years before the end")
})

# Long-format assessment history of a category 3 stock
ut_assessment <- function() {
  d <- expand.grid(year = 1990:2025, key = c("SSB", "F"), assessment_year = 2025)
  d$median <- ifelse(d$key == "SSB", 1000 + 10 * (d$year - 1990), 0.9 + (d$year - 1990) / 100)
  d$low <- d$median * 0.8
  d$high <- d$median * 1.2
  d$label.en <- ifelse(d$key == "SSB", "Biomass index", "Fishing pressure proxy")
  d$label.is <- d$label.en
  d
}

ok_group("hr_advice_plot_index: index A and B spanning their periods", {
  d <- ut_assessment()
  p <- hr_advice_plot_index(
    d, 2025, ref_points = list(I_trigger = 500),
    index_ab = TRUE, index_ab_span = "periods", index_ab_colour = "red"
  )
  ab <- p$layers[[length(p$layers)]]
  ok(ut_cmp_equal(ab$aes_params$colour, "red"), "Colour")
  ok(
    ut_cmp_equal(ab$data$year, c(2020.5, 2022, 2023.5, 2023.5, 2025)),
    "Index B from year - 4.5 to - 1.5, index A from there to the year"
  )
  ok(ut_cmp_equal(ab$data$avg, c(1320, 1320, 1320, 1345, 1345)), "Means of the periods")
  p2 <- hr_advice_plot_index(d, 2025, index_ab = TRUE)
  ok(
    ut_cmp_equal(p2$layers[[length(p2$layers)]]$data$year, 2021:2025),
    "Default: the lines span the years"
  )
})

ok_group("hr_advice_plot_fproxy", {
  d <- ut_assessment()
  p <- hr_advice_plot_fproxy(d, 2025, list(F_msy_proxy = 1), year_start = 1998, year_end = 2024, points = TRUE)
  ok(ut_cmp_equal(range(p$data$year), c(1998, 2024)), "Years")
  pt <- p$layers[[length(p$layers)]]
  ok(ut_cmp_equal(class(pt$geom)[1], "GeomPoint"), "Points last")
  ok(ut_cmp_equal(pt$aes_params$size, 0.8), "Point size 0.8")
  ok(ut_cmp_equal(p$labels$title, hr_label("fproxy", bold = TRUE)), "Title")

  # Values 0.9-1.25 from 1990: y_pad = 0.1 gives 0.8-1.4 (24-lem)
  p <- hr_advice_plot_fproxy(d, 2025, list(F_msy_proxy = 1), y_pad = 0.1)
  ok(ut_cmp_equal(p$scales$get_scales("y")$limits, c(0.8, 1.4)), "Axis from the data, widened by 0.1")
  # At least 0.8-1.2, widened to the data (60-norway-redfish)
  p <- hr_advice_plot_fproxy(d, 2025, list(F_msy_proxy = 1), y_limits = c(0.8, 1.2))
  ok(ut_cmp_equal(p$scales$get_scales("y")$limits, c(0.8, 1.3)), "Least range, widened")
  p <- hr_advice_plot_fproxy(d, 2025, list(F_msy_proxy = 1), year_end = 2010, y_limits = c(0.8, 1.2))
  ok(ut_cmp_equal(p$scales$get_scales("y")$limits, c(0.8, 1.2)), "Least range")
})

ok_group("hr_advice_data_tac: landings_year_end", {
  args <- list(
    advice_hist = data.frame(
      assessment_year = 2023:2025,
      advice_period = c("2023/2024", "2024/2025", "2025/2026"),
      advice = c(100, 110, 120)
    ),
    tac_hist = data.frame(assessment_year = 2023:2025, tac = c(100, 110, 120)),
    landings_by_fishing_year_country = data.frame(
      fishing_year = c("2023/2024", "2024/2025", "2024/2025"),
      country = c("Iceland", "Iceland", "Faroe Islands"),
      catch = c(80000, 90000, 5000)
    )
  )
  out <- do.call(hr_advice_data_tac, args)
  ok(ut_cmp_equal(out$total, c(80, 95, NA)), "All fishing years by default")
  out <- do.call(hr_advice_data_tac, c(args, landings_year_end = 2023))
  ok(ut_cmp_equal(out$total, c(80, NA, NA)), "Fishing years starting after landings_year_end left out")
  ok(ut_cmp_equal(out$foreign, c(NA_real_, NA, NA)), "No foreign landings left: NA")
})
