if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

# A small pax database: two spring survey tows, one with counted fish (the
# ldist counts of sample 1 are already raised 2x by pax_mar_ldist())
ut_pcon <- function() {
  pcon <- pax::pax_connect(":memory:")
  pax::pax_import(
    pcon,
    data.frame(
      sample_id = c("1", "2"),
      year = 2019,
      month = 3,
      station = c(1, 2),
      sampling_type = 30,
      mfdb_gear_code = "BMT",
      gear_id = 73,
      tow_number = c(1, 2),
      begin_lat = c(64, 65),
      begin_lon = c(-22, -24),
      tow_length = 4
    ),
    name = "station"
  )
  pax::pax_import(
    pcon,
    data.frame(
      sample_id = c("1", "1", "2"),
      species = 1,
      length = c(40.2, 50, 50),
      sex = 1,
      count = c(20, 20, 10)
    ),
    name = "ldist"
  )
  pax::pax_import(
    pcon,
    data.frame(
      sample_id = c("1", "1", "2"),
      species = 1,
      measurement_type = c("LEN", "CNT", "LEN"),
      count = c(20, 20, 10)
    ),
    name = "measurement"
  )
  pax::pax_import(
    pcon,
    data.frame(species = 1, a = 0.01, b = 3),
    name = "lw_coeffs"
  )
  pcon
}

ok_group("dat_ldist_by_year: survey ldist not raised a second time", {
  pcon <- ut_pcon()
  out <- hafroreports:::dat_ldist_by_year(pcon, 30) |>
    dplyr::arrange(length) |>
    dplyr::collect()
  ok(ut_cmp_equal(out$length, c(40, 50)), "Lengths rounded")
  ok(ut_cmp_equal(out$n, c(20, 30)), "Counts as in ldist")
  DBI::dbDisconnect(pcon)
})
