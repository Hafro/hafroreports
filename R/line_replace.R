# Replace the MUPPET setting on every line matching `pattern` (a regular
# expression) with "<parameter>\t <pattern>", as rmuppet:::line_replace(),
# which rmuppet doesn't export. Unlike rmuppet's, stop when no line matches:
# rmuppet's silently returned the file unchanged (01-cod's cod_line_replace()).
line_replace <- function(txt, parameter, pattern) {
  if (missing(parameter)) return(txt)
  i <- grep(pattern, txt)
  if (length(i) == 0) {
    stop("MUPPET settings: no line matches '", pattern, "'")
  }
  txt[i] <- paste(as.character(parameter), "\t", pattern)
  txt
}
