# A toy pax database: a simulated stock ("species 99"), not real data.
# Ten years of a spring survey at 12 fixed stations in two strata, and 12
# commercial samples a year from two gears. pax_from_mar() builds the same
# tables (with many more columns) from the MFRI database.
set.seed(1)
years <- 2015:2024
ages <- 1:10
toy_species <- 99
# Numbers at age, von Bertalanffy growth and a maturity ogive at length
n_at_age <- function() 1000 * exp(-0.35 * (ages - 1) + rnorm(length(ages), 0, 0.3))
vb_length <- function(age) 90 * (1 - exp(-0.18 * (age + 0.3)))
p_mature <- function(length) stats::plogis((length - 45) / 4)
sim_fish <- function(n, l50 = 0) {
  age <- sample(ages, n, replace = TRUE, prob = n_at_age())
  length <- round(vb_length(age) * exp(rnorm(n, 0, 0.08)))
  keep <- runif(n) < stats::plogis((length - l50) / 3) # gear selection
  data.frame(age = age[keep], length = length[keep])
}

# Fixed survey stations: grid cells in divisions 101 and 107, with the
# stratum of each station and the stratum area (square nautical miles)
gridcells <- c(4211, 4213, 4221, 4222, 4141, 4142)
toy_strata_stations <- data.frame(
  station = 1:12,
  stratum = rep(1:2, each = 6),
  area = rep(c(3000, 5000), each = 6)
)

station <- ldist <- otoliths <- list()
sample_id <- 0
add_sample <- function(st, fish, n_aged, maturity) {
  sample_id <<- sample_id + 1
  id <- as.character(sample_id)
  station[[id]] <<- data.frame(sample_id = id, st)
  fish$sex <- sample(1:2, nrow(fish), replace = TRUE)
  ldist[[id]] <<- dplyr::count(
    data.frame(sample_id = id, species = toy_species, fish),
    sample_id, species, length, sex, name = "count"
  )
  aged <- fish[sample(nrow(fish), min(n_aged, nrow(fish))), ]
  otoliths[[id]] <<- data.frame(
    sample_id = id, species = toy_species, measurement_type = "OTOL",
    aged, count = 1,
    maturity_stage = if (maturity) ifelse(runif(nrow(aged)) < p_mature(aged$length), 2, 1) else NA
  )
}
for (y in years) {
  for (s in toy_strata_stations$station) {
    add_sample(
      data.frame(year = y, month = 3, station = s, sampling_type = 30,
        mfdb_gear_code = "BMT", gear_id = 73, tow_number = s,
        gridcell = gridcells[(s - 1) %% 6 + 1], tow_length = 4, tow_depth = 100),
      sim_fish(rpois(1, 60)), n_aged = 10, maturity = TRUE
    )
  }
  for (k in 1:12) {
    gear <- sample(c("BMT", "LLN"), 1)
    add_sample(
      # NB: for commercial samples tow_number is the haul number
      data.frame(year = y, month = sample(1:12, 1), station = NA, sampling_type = 1,
        mfdb_gear_code = gear, gear_id = if (gear == "BMT") 6 else 1,
        tow_number = sample(1:60, 1), gridcell = sample(gridcells, 1),
        tow_length = NA, tow_depth = 150),
      sim_fish(150, l50 = if (gear == "BMT") 40 else 50), n_aged = 25, maturity = FALSE
    )
  }
}
station <- dplyr::bind_rows(station)
ldist <- dplyr::bind_rows(ldist)
measurement <- dplyr::bind_rows(otoliths)
aldist <- measurement |>
  dplyr::mutate(weight = 0.01 * length^3) |>
  dplyr::count(sample_id, species, length, age, weight, name = "count")
landings <- tidyr::expand_grid(
  year = years, month = 1:12, mfdb_gear_code = c("BMT", "LLN")
) |>
  dplyr::mutate(
    species = toy_species, ices_area = "5a", country = "Iceland",
    boat_id = sample(1:30, dplyr::n(), replace = TRUE),
    catch = round(runif(dplyr::n(), 50e3, 150e3)) # kg
  )
lw_coeffs <- data.frame(species = toy_species, a = 0.01, b = 3)

pax_db <- pax::pax_connect(tempfile(fileext = ".duckdb"))
for (tbl_name in c("station", "ldist", "aldist", "measurement", "landings", "lw_coeffs")) {
  pax::pax_import(pax_db, get(tbl_name), name = tbl_name)
}
