hr_advice_table_landings <- function(pcon) {
  # i.e. advice/tables/landings.csv
  dplyr::tbl(pcon, "landings") |>
    dplyr::mutate(mfdb_gear_code = coalesce(mfdb_gear_code, "Other")) |>
    dplyr::group_by(year, mfdb_gear_code) |>
    dplyr::summarise(tonnes = sum(catch) / 1e6, .groups = "drop") |>
    # TODO: pax::pax_describe_mfdb_gear_code? We'd need icelandic
    dplyr::collect() |>
    dplyr::mutate(
      gear.is = forcats::fct_recode(
        mfdb_gear_code,
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
        mfdb_gear_code,
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
}
