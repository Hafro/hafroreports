library(unittest)
library(hafroreports)

ok_group("hr_locale", {
  ok(!anyNA(hr_locale), "No missing values in hr_locale")
  ok(ut_cmp_identical(hr_locale[hr_locale$key == "NE", "is"], "NA"),
     "Icelandic label of NE is the string \"NA\" (norðaustur), not NA")
  ok(!anyDuplicated(hr_locale$key), "One row per key")
})

ok_group("hr_label", {
  withr::with_options(list(hr.lang = "is"), {
    ok(ut_cmp_identical(hr_label("NE"), "NA"), "is: NE is NA (norðaustur)")
    ok(ut_cmp_identical(hr_label("year"), "Ár"), "is: year is Ár")
    ok(ut_cmp_identical(hr_label("no_such_key"), "no_such_key"),
       "Unknown key falls back to the key")
  })
  withr::with_options(list(hr.lang = "en"), {
    ok(ut_cmp_identical(hr_label("NE"), "NE"), "en: NE is NE")
    ok(ut_cmp_identical(hr_label("year"), "Year"), "en: year is Year")
  })
})
