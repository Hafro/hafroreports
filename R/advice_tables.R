hr_advice_table_landings <- function(landings_by_gear) {
  # Was advice/tables/landings.csv
  landings_by_gear |>
    dplyr::group_by(year, gear_name) |>
    dplyr::summarise(tonnes = sum(catch) / 1e6, .groups = "drop") |>
    # TODO: pax::pax_describe_mfdb_gear_code? We'd need icelandic
    dplyr::collect() |>
    dplyr::mutate(
      gear.is = forcats::fct_recode(
        gear_name,
        "Lína" = "LLN",
        "Dragnót" = "DSE",
        "Botnvarpa" = "BMT",
        "Annað og óskilgreint" = "Other"
      ) |>
        forcats::fct_relevel(
          "Lína",
          "Dragnót",
          "Botnvarpa",
          "Annað og óskilgreint"
        ),

      gear.en = forcats::fct_recode(
        gear_name,
        "Longline" = "LLN",
        "Demersal seine" = "DSE",
        "Bottom trawl" = "BMT",
        "Other and undefined gear" = "Other"
      ) |>
        forcats::fct_relevel(
          "Longline",
          "Demersal seine",
          "Bottom trawl",
          "Other and undefined gear"
        )
    )
}

hr_advice_table_assessment <- function(assessment) {
  assessment |>
    tidyr::gather(key, value, -c(year, species, assessment_year)) |>
    dplyr::filter(key != 'landings') |>
    tidyr::separate(key, c('stat', 'key')) |>
    tidyr::spread(stat, value) |>
    dplyr::mutate(
      label.is = ordered(
        forcats::fct_recode(
          key,
          'Nýliðun' = 'recruitment',
          'Hrygningarstofn' = 'SSB',
          'Viðmiðunarstofn' = 'refbio',
          'Veiðihlutfall' = 'HR',
          #'Landaður afli' = 'landings',
          'Veiðidánartala' = 'F'
        ),
        levels = c(
          'Nýliðun',
          'Hrygningarstofn',
          'Viðmiðunarstofn',
          'Veiðihlutfall',
          'Veiðidánartala'
        )
      ),
      label.en = ordered(
        forcats::fct_recode(
          key,
          'Recruitment' = 'recruitment',
          'SSB' = 'SSB',
          'Reference biomass' = 'refbio',
          'Harvest rate' = 'HR',
          #'Landaður afli' = 'landings',
          'Fishing mortality' = 'F'
        ),
        levels = c(
          'Recruitment',
          'SSB',
          'Reference biomass',
          'Harvest rate',
          'Fishing mortality'
        )
      )
    )
}
