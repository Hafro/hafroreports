# Get current git revision string

Returns a string identifying the current git revision of a repository.
If HEAD is exactly on a tag, the tag name is returned. Otherwise,
returns a `branch:sha` string. A `-dirty` suffix is appended when there
are uncommitted changes.

## Usage

``` r
hr_git_revision(repo_path = ".", as_html = FALSE)
```

## Arguments

- repo_path:

  Path to the git repository root. Defaults to `"."`.

- as_html:

  Logical. If `TRUE`, wraps the output in an HTML `<code>` element with
  reduced opacity and font size, suitable for embedding in a report.
  Default is `FALSE`.

## Value

A character string with the git revision, optionally wrapped in HTML.
