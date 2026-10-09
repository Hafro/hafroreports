library(unittest)
library(hafroreports)

line_replace <- hafroreports:::line_replace

opt <- paste(c(
  "../Files/catch.dat \t # Catch file",
  "2020 \t # Last opt year i.e last year before assyear   <=lastdatayear",
  "2020 \t # Last data year, last year with catch at age data",
  "10 \t # Last model age",
  "1 \t # Plus group",
  "1 \t # Last year smb",
  "0 \t # Last year smh"
), collapse = "\n")

ok_group("line_replace", {
  txt <- c("1 \t # Plus group", "3 \t # Other", "0 \t # Plus group")
  ok(ut_cmp_equal(
    line_replace(txt, 12, "# Plus group"),
    c("12 \t # Plus group", "3 \t # Other", "12 \t # Plus group")
  ), "Every matching line is replaced by '<parameter> \\t <pattern>'")
  ok(ut_cmp_equal(line_replace(txt, pattern = "# Plus group"), txt),
     "No parameter: unchanged")
  ok(ut_cmp_error(line_replace(txt, 12, "# Last year smb"), "no line matches"),
     "No line matching the pattern is an error, not a silent no-op")
  if (requireNamespace("rmuppet", quietly = TRUE)) {
    ok(ut_cmp_identical(
      line_replace(txt, 12, "# Plus group"),
      rmuppet:::line_replace(txt, 12, "# Plus group")
    ), "Same result as rmuppet:::line_replace() when a line matches")
  }
})

ok_group("hr_muppet_input_optionfile", {
  out <- hr_muppet_input_optionfile(opt, "params/had.dat.opt", year_end = 2025)
  ok(ut_cmp_identical(names(out), "params/had.dat.opt"), "Named by out_name")
  ok(ut_cmp_identical(out[[1]], c(
    "Files/catch.dat \t # Catch file",
    "2024 \t # Last opt year i.e last year before assyear   <=lastdatayear",
    "2024 \t # Last data year, last year with catch at age data",
    "10 \t # Last model age",
    "1 \t # Plus group",
    "2025 \t # Last year smb",
    "2024 \t # Last year smh"
  )), "Year and age settings set")
  ok(ut_cmp_error(
    hr_muppet_input_optionfile(
      sub("# Last year smh", "# Smh last", opt, fixed = TRUE),
      "x", year_end = 2025
    ),
    "# Last year smh"
  ), "A missing setting is an error")
})
