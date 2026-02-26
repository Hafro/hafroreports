#' @title Advice sheet banner
#' @param tyr the assessment year
#' @param tac TAC
#' @param tac_last_year last years tac
#' @param lang language, only 'is' and 'en' supported
#' @param note banner note, if needed
#' @param publication_date ``BigD::fdt()``-parsable date string with the publication date
#' @param period Advisory period. Default is 'fishing_year', other options: 'annual', '2year', '3year', '5year'
#' @param publication_note publication note, if needed
#' @export
hr_advice_banner <- function(
  params = list(),
  year_end,
  tac,
  tac_last_year,
  lang = 'is',
  note = '',
  publication_date,
  period = 'fishing_year',
  publication_note = ''
) {
  dict <-
    array(
      c(
        'Ráðgjöf',
        'tonn',
        'Breyting á ráðgjöf',
        'Athugasemd',
        'Birting ráðgjafar',
        'Útgefið af Hafrannsóknastofnun.',
        'Advice',
        'tonnes',
        'Advice change',
        'Note',
        'Publication of Advice',
        'Published by Marine and Freshwater Research Institute.'
      ),
      dim = c(6, 2),
      dimnames = list(
        trans = c(
          'advice',
          'tonnes',
          'change',
          'finalnote',
          'publication',
          'publisher'
        ),
        lang = c('is', 'en')
      )
    )

  if (nchar(publication_note)) {
    dict['publisher', lang] <- paste(dict['publisher', lang], publication_note)
  }

  if (is.character(tac)) {
    dict['tonnes', ] <- ''
  }

  tac_change <-
    if (tac_last_year == 0 & tac != 0 | is.character(tac)) {
      ''
    } else if (tac_last_year == 0 & tac == 0) {
      '0 %'
    } else {
      paste(round(100 * (tac / tac_last_year - 1)), '%')
    }

  if (!(lang %in% c('is', 'en'))) {
    stop(sprintf('valid lang options are "is" and "en", %s was supplied', lang))
  }

  formatted_publication_date <- bigD::fdt(
    publication_date,
    format = "d. MMMM YYYY.",
    locale = lang
  )

  tac <-
    if (is.character(tac)) {
      tac
    } else {
      red_dot_number(tac)
    }

  tac_last_year <-
    if (is.character(tac_last_year)) {
      tac
    } else {
      red_dot_number(tac_last_year)
    }

  if (period == 'fishing_year') {
    this_year <- paste(year_end, year_end + 1, sep = '/')
    last_year <- paste(year_end - 1, year_end, sep = '/')
  } else if (period == 'annual') {
    this_year <- as.numeric(year_end + 1)
    last_year <- as.numeric(year_end)
  } else if (period == '2year') {
    this_year <- as.numeric(year_end + 1)
    last_year <- as.numeric(year_end - 1)
  } else if (period == '3year') {
    this_year <- as.numeric(year_end + 1)
    last_year <- as.numeric(year_end - 2)
  } else if (period == '5year') {
    this_year <- as.numeric(year_end + 1)
    last_year <- as.numeric(year_end - 4)
  } else {
    warning('No recognisable period declared, defaulting to annual')
    this_year <- as.numeric(year_end + 1)
    last_year <- as.numeric(year_end)
  }

  banner <-
    glue::glue(
      "
::::: {{.callout}}
:::: {{layout=\"[[33, 33, 33], [100]]\"}}

::: {{#first-column}}

<p style=\"text-align: center; font-size: 1.125em;\"><b>{advice} {this_year}</b></p>

<p style=\"text-align: center; font-size: 2rem; color: #5B9BD5;\"><b>{tac}</b></p>

<p style=\"text-align: center; font-size: 1.125em;\"><b>{tonnes}</b></p>

:::
    
::: {{#Second-column}}
<p style=\"text-align: center; font-size: 1.125em;\"><b>{advice} {last_year}</b></p>

<p style=\"text-align: center; font-size: 2rem; color: #5B9BD5;\"><b>{tac_last_year}</b></p>

<p style=\"text-align: center; font-size: 1.125em;\"><b>{tonnes}</b></p>
:::

::: {{#Third-column}}
<p style=\"text-align: center; font-size: 1.125em;\"><b>{change}</b></p>

<p style=\"text-align: center; font-size: 2rem; color: #5B9BD5;\"><b>{tac_change}</b></p>

:::
::::
",
      advice = dict['advice', lang],
      tonnes = dict['tonnes', lang],
      change = dict['change', lang],
      tac_change = tac_change
    )

  if (stringr::str_length(note) > 0) {
    banner <-
      paste(
        banner,
        glue::glue(
          "

<p style=\"text-align: justify; font-size: 1.125em;\"><b>{finalnote}:</b> {note}</p>

:::::
",
          finalnote = dict['finalnote', lang],
          note = note
        ),
        sep = '\n'
      )
  } else {
    banner <-
      paste(banner, ':::::', sep = '\n')
  }

  banner <-
    paste(
      banner,
      glue::glue(
        "
::: {{style=\"font-size: 11pt;color: #767676;line-height: normal;margin-bottom: -2%;\"}}
<p><span>{publication}: {formatted_publication_date}</span>   <span>{publisher}</span></p><br />
:::
",
        publication = dict['publication', lang],
        publisher = dict['publisher', lang]
      ),
      sep = '\n'
    )

  cat(banner)
}
