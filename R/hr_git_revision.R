#' Get current git revision string
#'
#' Returns a string identifying the current git revision of a repository.
#' If HEAD is exactly on a tag, the tag name is returned. Otherwise, returns
#' a \code{branch:sha} string. A \code{-dirty} suffix is appended when there
#' are uncommitted changes.
#'
#' @param repo_path Path to the git repository root. Defaults to \code{"."}.
#' @param as_html Logical. If \code{TRUE}, wraps the output in an HTML
#'   \code{<code>} element with reduced opacity and font size, suitable for
#'   embedding in a report. Default is \code{FALSE}.
#' @return A character string with the git revision, optionally wrapped in HTML.
#' @export
hr_git_revision <- function(repo_path = ".", as_html = FALSE) {
  git <- function(...) {
    rv <- suppressWarnings(system2(
      Sys.which("git"),
      args = c("-C", repo_path, c(...)),
      stdout = TRUE,
      stderr = TRUE
    ))

    if (!is.null(attr(rv, "status"))) {
      stop("Failed to run git:\n", rv)
    }
    return(rv)
  }

  out <- tryCatch(
    {
      # Try getting the current tag
      git("describe", "--exact-match", "HEAD")
    },
    error = function(e) {
      # If that doesn't work, return (branch):(revision)
      paste(
        git("rev-parse", "--abbrev-ref", "HEAD"),
        git("rev-parse", "--short", "HEAD"),
        sep = ":"
      )
    }
  )

  # Add dirty marker to string if changes present
  if (length(git("status", "--porcelain")) > 0) {
    out <- paste0(out, "-dirty")
  }

  if (as_html) {
    out <- sprintf(
      '<code style="opacity: 0.7; font-size: 0.5rem">%s</code>',
      out
    )
  }
  return(out)
}
