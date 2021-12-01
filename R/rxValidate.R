#' Validate rxode2
#' This allows easy validation/qualification of nlmixr by running the
#' testing suite on your system.
#'
#' @param type Type of test or filter of test type
#' @author Matthew L. Fidler
#' @return nothing
#' @export
rxValidate <- function(type = NULL) {
  pt <- proc.time()
  .filter <- NULL
  if (is.null(type)) type <- FALSE
  if (is.character(type)) {
    .filter <- type
    type <- TRUE
  }
  if (type == TRUE) {
    .oldCran <- Sys.getenv("NOT_CRAN")
    Sys.setenv("NOT_CRAN" = "true")
    on.exit(Sys.setenv("NOT_CRAN" = .oldCran))
  }
  .rxWithOptions(list(testthat.progress.max_fails = 10000000000), {
    path <- file.path(system.file("tests", package = "rxode2"), "testthat")
    .rxWithWd(path, {
      try(testthat::test_dir(path, filter = .filter))
      message("================================================================================")
      print(proc.time() - pt)
      message("================================================================================")
    })
  })
}

#' @rdname rxValidate
#' @export
rxTest <- rxValidate

#' Wrap a test in rxode2
#'
#' This wraps tests in rxode2 to allow testing on cran or not on cran
#'
#' @param code Code to be evaluated
#' @param test Test to be run.  Currently only accepts CRAN and not cran
#' @param silent is an ignored argument now
#' @return value of code or NULL
#' @keywords internal
#' @author Matthew Fidler
#' @export
rxode2Test <- function(code, test = "cran", silent = "ignore") {
  on.exit({
    rxUnloadAll()
  })
  .notCran <- Sys.getenv("NOT_CRAN") == "true"
  if (test == "cran") {
    return(force(code))
  } else if (.notCran) {
    return(force(code))
  } else {
    return(invisible())
  }
}
