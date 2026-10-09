# Generate a glossary section

Writes a Markdown/Quarto glossary section to the console using `cat`.
The section title and term definitions are localised to the current
language setting (`options(hr.lang = "en")` or `"is"`). Currently
defines the acronym TAC.

## Usage

``` r
hr_glossary_section()
```

## Value

Invisibly returns `NULL`; the glossary is written to the console via
`cat`.

## Details

Intended to be called inside a Quarto code block with `output: asis`:

    #| echo: false
    #| output: asis

    hr_glossary_section()
