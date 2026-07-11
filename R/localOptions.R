local_options <- function(...) { # nolint: object_name_linter.
  if (requireNamespace("withr", quietly = TRUE)) {
    return(withr::local_options(...))
  }
  old <- options(...)
  invisible(old)
}
