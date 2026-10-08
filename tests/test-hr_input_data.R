if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

ok_group("hr_input_data_combine: separate weight fills, stock survey", {
  comm <- data.frame(year = c(2000, 2000, 2001), age = c(1, 2, 1), n = 1, mw = c(100, 200, 300))
  smb <- data.frame(year = c(2000, 2001), age = 1, n = 5, mw = c(50, NA), mat = 0.5)
  smh <- data.frame(year = c(2000, 2001), age = 1, n = 7, mw = c(70, 80), mat = 0.9)
  lnd <- data.frame(year = 2000:2001, catch = 1000)
  out <- hr_input_data_combine(2000, 2001, age_start = 1, age_end = 2,
    input_data_comm_index = comm, input_data_igfs_index = smb,
    input_data_agfs_index = smh, input_data_landings = lnd)
  ok(ut_cmp_equal(out$catch_weight, c(100, 200, 300, 200)), "Default: catch weights filled with the mean")
  ok(ut_cmp_equal(out$stock_weight, c(50, NA, 50, NA)), "Default: stock weights filled with the mean (none for age 2)")
  ok(ut_cmp_equal(names(out), c("year", "age", "catch", "catch_weight", "smb", "stock_weight", "maturity", "smh", "M")), "Columns as before")
  out <- hr_input_data_combine(2000, 2001, age_start = 1, age_end = 2,
    input_data_comm_index = comm, input_data_igfs_index = smb,
    input_data_agfs_index = smh, input_data_landings = lnd,
    stock_weight_fill_mean = FALSE, stock_survey = "agfs")
  ok(ut_cmp_equal(out$stock_weight, c(70, NA, 80, NA)), "Stock weights from the autumn survey, not filled")
  ok(ut_cmp_equal(out$maturity[out$age == 1], c(0.9, 0.9)), "Maturity from the autumn survey")
  ok(ut_cmp_equal(out$catch_weight, c(100, 200, 300, 200)), "Catch weights still filled")
})

ok_group("hr_muppet_input_datafiles: without the haddock fills", {
  d <- tidyr::expand_grid(year = 1990:1992, age = 1:3)
  d$catch <- 1; d$catch_weight <- NA; d$stock_weight <- NA; d$maturity <- NA
  d$smb <- 1; d$smh <- 1
  had <- hr_muppet_input_datafiles(d, 1990, 1992, 3)
  gen <- hr_muppet_input_datafiles(d, 1990, 1992, 3, haddock_fills = FALSE, smb_year_start = 1990, smh_year_start = 1991, survey_ages = 2:3)
  row <- function(x, y, a) strsplit(grep(paste0("^", y, "\t", a, "\t"), strsplit(x, "\n")[[1]], value = TRUE), "\t")[[1]]
  ok(ut_cmp_equal(row(had[["Files/catchandstockdata.dat"]], 1990, 3)[5], "481"), "Default: haddock stock weight fill, as before")
  ok(ut_cmp_equal(row(gen[["Files/catchandstockdata.dat"]], 1990, 3)[5], "-1"), "haddock_fills = FALSE: missing")
  ok(grepl("^1990\t1\t", had[["Files/marsurveydata.dat"]]), "Default: spring survey ages from 1, as before")
  ok(grepl("^1990\t2\t", gen[["Files/marsurveydata.dat"]]) && !grepl("\t1\t", gen[["Files/marsurveydata.dat"]]), "survey_ages: from age 2")
})
