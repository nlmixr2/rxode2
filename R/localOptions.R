local_options <- function(...) {
  if (requireNamespace("withr", quietly = TRUE)) {
    return(withr::local_options(...))
  }
  old <- options(...)
  invisible(old)
}
