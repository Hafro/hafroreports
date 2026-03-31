hr_techreport_plot_catch_age_prop <- function(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
) {
  # NSE variables
  year <- age <- catch_weight <- catch <- yc <- val <- NULL

  dat <- input_data |>
    dplyr::filter(
      year >= year_start,
      year <= year_end,
      !is.na(age),
      age >= age_start,
      age <= age_end
    ) |>
    dplyr::mutate(
      val = catch_weight * catch / 1e6,
      yc = as.ordered(year - age)
    ) |>
    dplyr::select(year, group = yc, val)

  two_panel_plot(
    dat,
    cols = rep(
      c(
        "#A6CEE3",
        "#1F78B4",
        "#B2DF8A",
        "#33A02C",
        "#FB9A99",
        "#E31A1C",
        "#FDBF6F",
        "#FF7F00",
        "#CAB2D6",
        "#6A3D9A",
        "#FFFF99",
        "#B15928"
      ),
      100
    )
  )
}
