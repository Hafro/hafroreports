if (!interactive()) {
  options(warn = 1, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)
library(hafroreports)

# Two years of landings of three boats
pcon <- pax::pax_connect(":memory:")
pax::pax_import(
  pcon,
  data.frame(
    year = rep(c(2020, 2021), each = 3),
    boat_id = rep(1:3, 2),
    catch = c(1000, 2000, 50, 3000, 10, 20)
  ),
  name = "landings"
)

y_label <- function(lang) {
  old <- options(hr.lang = lang)
  on.exit(options(old))
  p <- hr_techreport_plot_numboats(pcon)
  ggplot2::ggplot_build(p[[1]])$plot$labels$y
}

ok_group("hr_techreport_plot_numboats: y axis label", {
  ok(ut_cmp_identical(
    y_label("en"),
    "Number of vessels accounting for 95% of catch"
  ), "English")
  ok(ut_cmp_identical(
    y_label("is"),
    "Fjöldi báta sem veiða 95 % af heildarafla"
  ), "Icelandic")
})

DBI::dbDisconnect(pcon, shutdown = TRUE)
