if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

# The 2026 Arnarfjordur shrimp prognosis (dlsrules::hr_index_advice())
prognosis <- data.frame(
  component = c(
    "previous_catch_advice", "biomass_index", "hr_proxy", "I_trigger",
    "biomass_safeguard", "initial_catch_advice", "stability_test",
    "stability_clause_applied", "catch_advice", "advice_change"
  ),
  value = c(0, 717, 0.346, 390, 1, 248.082, 0, 0, 248.082, 100)
)

ok_group("hr_advice_table_hr_index", {
  for (lang in c("en", "is")) {
    withr::with_options(list(hr.lang = lang), {
      ft <- hr_advice_table_hr_index(
        prognosis,
        2026,
        hr_proxy_desc = list(en = "Mean HR~proxy~ in 2010--2015", is = "Meðaltal HR~proxy~ áranna 2010--2015")
      )
      ok(inherits(ft, "flextable"), paste("A flextable,", lang))
      body <- ft$body$dataset
      ok(ut_cmp_equal(nrow(body), 10L), "Ten rows, as the old shrimp table")
      ok(any(body$value == "248"), "Advice 248 t")
      ok(any(body$value == "0.346"), "HR proxy")
      ok(any(grepl("2010--2015", body$chr_desc)), "Stock-specific HR proxy description")
      ok(ut_cmp_equal(flextable::nrow_part(ft, "footer"), 2L), "Two footnotes (formula, rounding)")
    })
  }
  ft <- hr_advice_table_hr_index(prognosis, 2026, stability = TRUE)
  ok(ut_cmp_equal(nrow(ft$body$dataset), 11L), "With the stability clause row")
  ok(ut_cmp_equal(flextable::nrow_part(ft, "footer"), 3L), "...and its footnote")
})

ok_group("hr_advice_table_chr / _rfb footnotes unchanged", {
  chr <- data.frame(
    component = c("previous_catch_advice", "biomass_index", "Fmsy_proxy", "last_index_value", "Itrigger", "Iloss", "biomass_safeguard", "multiplier", "initial_catch_advice", "stability_clause_applied", "catch_advice", "advice_change"),
    value = c(100, 850, 0.2, 850, 350, 250, 1, 0.5, 85, 1, 80, -20)
  )
  ft <- hr_advice_table_chr(chr, 2025)
  ok(ut_cmp_equal(flextable::nrow_part(ft, "footer"), 3L), "chr table: three footnotes")
})

ok_group("hr_advice_plot_index / _fpl / _landings: shrimp options", {
  assessment <- data.frame(
    year = 2020:2026, species = 41, assessment_year = 2026,
    median_SSB = c(535, 432, 698, 486, 489, 277, 717),
    low_SSB = c(81, 120, 387, 233, 147, 124, 254),
    high_SSB = c(3544, 1565, 1261, 1013, 1623, 616, 2019),
    median_F = c(0.37, 0.35, 0.35, 0.34, 0.31, 0, NA)
  )
  da <- hr_advice_data_assessment(assessment)
  p <- hr_advice_plot_index(da, 2026, ref_points = list(I_trigger = 390), ref_label = hr_label("Itrigger_Ilim"), ref_label_year = 2021)
  b <- ggplot2::ggplot_build(p)
  txt <- Filter(function(x) "label" %in% names(x), b$data)
  ok(any(vapply(txt, function(x) any(x$label == "I[trigger]==I[lim]"), logical(1))), "I_trigger = I_lim label")
  p <- hr_advice_plot_fpl(da, 2026, list(F_msy_proxy = 0.346), fishing_pressure = "F", ref_labels = list(F_msy_proxy = hr_label("HRtarget_proxy")))
  b <- ggplot2::ggplot_build(p)
  txt <- Filter(function(x) "label" %in% names(x), b$data)
  ok(any(vapply(txt, function(x) any(x$label == "target~HR[proxy]"), logical(1))), "Target HR proxy label")
  dl <- hr_advice_data_landings(data.frame(year = 2020:2025, gear_name = "SHT", catch = c(199, 150, 243, 165, 154, 0) * 1000))
  ok(ut_cmp_equal(as.character(unique(dl$gear.en)), "Shrimp trawl"), "Shrimp trawl gear label")
  ok(inherits(hr_advice_plot_landings(dl, 2026, year_start = 2019), "ggplot"), "Landings plot")
})

ok_group("hr_techreport_plot_* (invertebrate)", {
  idx <- expand.grid(index = c("total", "fishable", "juvenile", "female"), year = 2020:2026, stringsAsFactors = FALSE)
  idx$b <- 100 + seq_len(nrow(idx))
  idx$b_lower <- idx$b * 0.5
  idx$b_upper <- idx$b * 2
  p <- hr_techreport_plot_survey_indices(idx, ref_lines = list(fishable = 390))
  ok(ut_cmp_equal(length(unique(ggplot2::ggplot_build(p)$data[[1]]$PANEL)), 4L), "Four index panels")
  sub <- expand.grid(year = 2020:2026, stratum = 1:5, index = c("fishable", "total"), stringsAsFactors = FALSE)
  sub$b <- 10
  ok(inherits(hr_techreport_plot_index_subarea(sub), "ggplot"), "Index by sub-area")
  ld <- expand.grid(year = 2025:2026, length = seq(10, 25, 0.5), sex = 1:2)
  ld$b <- dnorm(ld$length, 17, 3)
  ok(inherits(hr_techreport_plot_ldist_sex(ld), "ggplot"), "Length distribution by sex")
  sc <- data.frame(year = 2026, lat = c(65.7, 65.75), lon = c(-23.5, -23.6), catch_nm = c(0, 20))
  ok(inherits(hr_techreport_plot_station_catch(sc, c(-24.1, -23.1), c(65.6, 65.9)), "ggplot"), "Station catch map")
  lc <- data.frame(fishing_year = "2024/2025", x = -23.5, y = 65.7, catch = 1000)
  ok(inherits(hr_techreport_plot_logbook_catch(lc, c(-24.1, -23.1), c(65.6, 65.9)), "ggplot"), "Logbook catch map")
  cp <- data.frame(year = 2020:2025, catch = c(199, 150, 243, 165, 154, 0), cpue = c(793, 836, 580, 1209, 374, NA))
  ok(inherits(hr_techreport_plot_catch_cpue(cp), "patchwork"), "Catch and CPUE")
  ok(inherits(hr_techreport_plot_advice_tac(
    data.frame(assessment_year = 2020:2026, advice = c(184, 148, 242, 166, 169, 0, 248)),
    data.frame(assessment_year = 2020:2026, tac = c(184, 148, 242, 166, 169, 0, NA)),
    data.frame(year = 2020:2025, landings = c(199, 150, 243, 165, 154, 0))
  ), "ggplot"), "Advice, TAC and landings")
  ok(inherits(hr_techreport_plot_survey_temperature(data.frame(year = 2020:2026, bottom = 5, surface = 9)), "ggplot"), "Temperature")
  bc <- expand.grid(species = c(1, 2, 4), size = 1:2, year = 2020:2026)
  bc$index <- 1e5
  p <- hr_techreport_plot_bycatch_index(bc)
  ok(ut_cmp_equal(length(unique(ggplot2::ggplot_build(p)$data[[1]]$PANEL)), 4L), "Cod and haddock, two sizes")
})

ok_group("hr_techreport_plot_* (invertebrate): second survey and seasons", {
  idx <- expand.grid(index = c("total", "fishable", "juvenile", "female"), year = 2020:2026, stringsAsFactors = FALSE)
  idx$b <- 100
  idx$b_lower <- 50
  idx$b_upper <- 200
  feb <- idx[idx$year %in% c(2021, 2024), ]
  p0 <- hr_techreport_plot_survey_indices(idx)
  p <- hr_techreport_plot_survey_indices(idx, survey_index2 = feb)
  b <- ggplot2::ggplot_build(p)
  ok(ut_cmp_equal(length(b$data), length(ggplot2::ggplot_build(p0)$data) + 3L), "Second survey: ribbon, dashed line and points added")
  ok(ut_cmp_equal(sum(!is.na(b$data[[6]]$y)), 8L), "...two years of four indices")
  ok(any(is.na(b$data[[5]]$y)), "...with gaps between irregular survey years")

  ld <- expand.grid(year = 2025:2026, length = seq(10, 25, 0.5), sex = 1:2)
  ld$b <- dnorm(ld$length, 17, 3)
  ld2 <- ld[ld$year == 2025, ]
  ld2$b <- ld2$b / 2
  ok(ut_cmp_equal(length(ggplot2::ggplot_build(hr_techreport_plot_ldist_sex(ld, ldist2 = ld2))$data), 6L), "Length distribution with dashed second survey")
  ok(ut_cmp_equal(length(ggplot2::ggplot_build(hr_techreport_plot_ldist_sex(ld))$data), 4L), "...default unchanged")

  sc <- data.frame(year = 2020, lat = c(66, 66.1), lon = c(-22.8, -22.9), catch_nm = c(0, 20), season = c("Autumn", "February"))
  p <- hr_techreport_plot_station_catch(sc, c(-23.3, -22.35), c(65.79, 66.33), size_label = hr_label("number_per_nm"))
  ok(inherits(p$facet, "FacetGrid"), "Seasons as rows of maps")
  ok(ut_cmp_equal(p$scales$get_scales("size")$name, "Number per nm"), "Size legend label")
  p <- hr_techreport_plot_station_catch(sc[, 1:4], c(-23.3, -22.35), c(65.79, 66.33))
  ok(inherits(p$facet, "FacetWrap") && p$scales$get_scales("size")$name == "Kilograms per nm", "...default unchanged")

  bc <- expand.grid(species = c(1, 2), size = 1:2, year = 2020:2022, season = c("Autumn", "February"))
  bc$index <- 1e5
  p <- hr_techreport_plot_bycatch_index(bc, sizes = 2)
  ok(inherits(p$facet, "FacetGrid") && ut_cmp_equal(nrow(p$data), 12L), "By-catch: 1 year and older, species by season")
})

ok_group("hr_techreport_plot_* (invertebrate): catch and vessels, CPUE effects, CPUE and index", {
  p <- hr_techreport_plot_catch_vessels(
    data.frame(year = 2020:2025, landings = c(1960, 2994, 1993, 1912, 1352, 1365)),
    data.frame(year = 2020:2025, vessels = c(6, 6, 5, 5, 4, 4))
  )
  ok(inherits(p, "patchwork"), "Catch and number of vessels")
  eff <- rbind(
    data.frame(effect = "year", level = 1989:1995, mean = seq(0.9, 1.7, length.out = 7)),
    data.frame(effect = "month", level = 2:12, mean = 1),
    data.frame(effect = "trawl_size", level = seq(1500, 5000, 500), mean = 1.2)
  )
  eff$lower <- eff$mean * 0.95
  eff$upper <- eff$mean * 1.05
  p <- hr_techreport_plot_cpue_effects(eff)
  ok(inherits(p, "patchwork") && length(p$patches$plots) + 1 == 3, "Three effect panels")
  ok(ut_cmp_equal(p[[1]]$labels$y, "Standardized CPUE"), "CPUE label on the first panel")
  idx <- data.frame(year = 1988:1996, b = c(35, 30, 45, 60, 55, 50, 70, 57, 105))
  p <- hr_techreport_plot_cpue_index(data.frame(year = 1989:1995, mean = seq(0.9, 1.7, length.out = 7)), idx)
  ok(inherits(p, "patchwork"), "CPUE against the index, and both by year")
  b <- ggplot2::ggplot_build(p[[2]])
  ok(ut_cmp_equal(nrow(b$data[[3]]), 7L), "Index points only in the CPUE years")
})
