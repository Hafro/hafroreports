hr_advice_data_prognosis <- function(
  basis_table,
  tac_hist,
  ref_points,
  stock_dev,
  assessment_year
) {
  tibble::tibble(
    assessment_year = assessment_year,
    basis.is = unlist(basis_table[1, "desc.is"]),
    basis.en = unlist(basis_table[1, "desc.en"]),
    HR = ref_points$HR_mgt,
    catch = tac_hist[tac_hist$assessment_year == assessment_year, "tac"],
    ssb = stock_dev[
      stock_dev$year == assessment_year + 2 & stock_dev$name == "ssb",
      "value"
    ],
    ssb_change = (unlist(stock_dev[
      stock_dev$year == assessment_year + 2 & stock_dev$name == "ssb_ratio",
      "value"
    ]) -
      1) *
      100,
    tac_current = tac_hist[
      tac_hist$assessment_year == assessment_year,
      "tac"
    ],
    tac_previous = tac_hist[
      tac_hist$assessment_year == assessment_year - 1,
      "tac"
    ]
  )
}

hr_advice_table_prognosis <- function(data_prognosis, assessment_year) {
  # NSE variables
  catch <- HR <- ssb <- ssb_change <- tac_current <- tac_previous <- tac_change <- advice_change <- NULL
  lang <- getOption("hr.lang", "en")

  if (any(!(c("tac_current", "tac_previous") %in% colnames(data_prognosis)))) {
    stop(
      "hr_advice_render_data_prognosis expects tac_current/tac_previous in data_prognosis, not tac_change as before"
    )
  }

  tac_previous <- data_prognosis[
    data_prognosis$assessment_year == assessment_year,
    'tac_previous'
  ]

  data_prognosis |>
    dplyr::filter(assessment_year == assessment_year) |>
    dplyr::select(-assessment_year) |>
    dplyr::select(
      basis = as.symbol(paste0('basis.', lang)),
      catch,
      HR,
      ssb,
      ssb_change,
      tac_current,
      tac_previous
    ) |>
    dplyr::mutate(
      catch = ifelse(catch == 0, "0", hr_red_dot_number(catch)),
      ssb = hr_red_dot_number(ssb),
      ssb_change = ifelse(
        ssb_change < 1,
        round(ssb_change, 1),
        round(ssb_change)
      ),
      tac_change = round(100 * (tac_current / tac_previous - 1)),
      advice_change = tac_change,
      advice_change = as.character(round(advice_change)),
      advice_change = dplyr::case_when(
        advice_change == 'Inf' ~ '-',
        .default = advice_change
      )
    ) |>
    dplyr::select(-c(tac_current, tac_previous)) |>
    flextable::flextable() |>
    ftExtra::colformat_md() |>
    flextable::mk_par(
      j = "basis",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Grunnur" else 'Basis'
      )
    ) |>
    flextable::mk_par(
      j = "catch",
      part = "header",
      value = flextable::as_paragraph(
        sprintf(
          '%s (%s)',
          if (lang == 'is') 'Afli' else 'Catch',
          assessment_year + 1
        )
      )
    ) |>
    flextable::mk_par(
      j = "HR",
      part = "header",
      value = flextable::as_paragraph(
        sprintf(
          '%s (%s)',
          if (lang == 'is') 'Veiðihlutfall' else 'Harvest rate',
          assessment_year + 1
        )
      )
    ) |>
    flextable::mk_par(
      j = "ssb",
      part = "header",
      value = flextable::as_paragraph(
        sprintf(
          '%s (%s)',
          if (lang == 'is') 'Hrygningarstofn' else 'SSB',
          assessment_year + 2
        )
      )
    ) |>
    flextable::mk_par(
      j = "ssb_change",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "% Breyting á hrygningarstofni" else 'SSB change (%)'
      )
    ) |>
    flextable::mk_par(
      j = "tac_change",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "% Breyting á aflamark" else 'TAC change (%)'
      )
    ) |>
    flextable::mk_par(
      j = "advice_change",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "% Breyting á ráðgjöf" else 'Advice change (%)'
      )
    ) |>
    flextable::colformat_num(
      i = 1,
      j = c(2, 4),
      big.mark = "  ",
      decimal.mark = ".",
      na_str = ""
    ) |>
    flextable::bg(bg = "#DEEAF6", part = "header") |>
    ### Total width of table in advice sheet is 9
    flextable::width(j = 1, width = 1.5) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::width(j = 3, width = 1.5) |>
    flextable::width(j = 4, width = 1.5) |>
    flextable::width(j = 5, width = 1.5) |>
    flextable::width(j = 6, width = 1.5) |>
    flextable::width(j = 7, width = 1.5) |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::align(j = 2:7, align = "center", part = "all") |>
    flextable::border_remove() |>
    flextable::border(part = "all", border = officer::fp_border()) |>
    flextable::footnote(
      i = 1,
      j = 5,
      value = flextable::as_paragraph(
        sprintf(
          '%s %s %s %s',
          if (lang == 'is') "Hrygningarstofn árið" else 'SSB in',
          assessment_year + 2,
          if (lang == 'is') {
            "miðað við hrygningarstofn"
          } else {
            'relative to SSB in'
          },
          assessment_year + 1
        )
      ),
      ref_symbols = "1) ",
      part = "header"
    ) |>
    flextable::footnote(
      i = 1,
      j = 6,
      value = flextable::as_paragraph(
        sprintf(
          '%s %s %s %s (%s t)',
          if (lang == 'is') "Ráðlagt aflamark fyrir" else "TAC value for",
          paste(assessment_year, assessment_year + 1, sep = '/'),
          if (lang == 'is') {
            "miðað við ráðlagt aflamark"
          } else {
            'relative to TAC value for'
          },
          paste(assessment_year - 1, assessment_year, sep = '/'),
          tac_previous
        )
      ),
      ref_symbols = "2) ",
      part = "header"
    ) |>
    flextable::footnote(
      i = 1,
      j = 7,
      value = flextable::as_paragraph(
        sprintf(
          '%s %s %s %s (%s t)',
          if (lang == 'is') "Ráðlagt aflamark fyrir" else "Advice value for",
          paste(assessment_year, assessment_year + 1, sep = '/'),
          if (lang == 'is') {
            "miðað við ráðlagt aflamark"
          } else {
            'relative to advice value for'
          },
          paste(assessment_year - 1, assessment_year, sep = '/'),
          tac_previous
        )
      ),
      ref_symbols = "3) ",
      part = "header"
    ) |>
    flextable::padding(padding = 2, part = "footer") |>
    flextable::fontsize(size = 8, part = "footer") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}
