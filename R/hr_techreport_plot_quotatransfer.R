# Was transfer_plot
hr_techreport_plot_quotatransfer <- function(pcon, assessment_year) {
  dat <- dplyr::tbl(pcon, "quotatransfer") |>
    dplyr::filter(as.numeric(substr(fishing_year, 1, 4)) < assessment_year)

  pax::pax_quotatransfer_plot(dat)
}
