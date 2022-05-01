#' Validate rxode2
#' This allows easy validation/qualification of nlmixr by running the
#' testing suite on your system.
#'
#' @param type Type of test or filter of test type, When this is an
#'   expression, evaluate the contents, respecting `skipOnCran`
#' @param skipOnCran when `TRUE` skip the test on CRAN.
#' @author Matthew L. Fidler
#' @return nothing
#' @export
rxValidate <- function(type = NULL, skipOnCran=TRUE) {
  if (is(substitute(type), "{")) {
    if (isTRUE(skipOnCran)) {
      if (!identical(Sys.getenv("NOT_CRAN"), "true") ||
            !identical(Sys.getenv("rxTest"), "")) {
        return(invisible())
      }
    }
    return(force(type))
  }
  pt <- proc.time()
  .filter <- NULL
  if (is.null(type)) type <- FALSE
  if (is.character(type)) {
    .filter <- type
    type <- TRUE
  }
  if (type == TRUE) {
    .oldCran <- Sys.getenv("NOT_CRAN")
    .oldRxTest <- Sys.getenv("rxTest")
    Sys.setenv("NOT_CRAN" = "true") # nolint
    Sys.setenv("rxTest" = "") # nolint
    on.exit(Sys.setenv("NOT_CRAN" = .oldCran, "rxTest"=.oldRxTest)) # nolint
  } else if (type == FALSE) {
    .oldCran <- Sys.getenv("NOT_CRAN")
    .oldRxTest <- Sys.getenv("rxTest")
    Sys.setenv("NOT_CRAN" = "false") # nolit
    Sys.setenv("rxTest" = "false") # nolint
    on.exit(Sys.setenv("NOT_CRAN" = .oldCran, "rxTest"=.oldRxTest)) # nolint
  }
  .rxWithOptions(list(testthat.progress.max_fails = 10000000000), {
    path <- file.path(system.file("tests", package = "rxode2"), "testthat")
    .rxWithWd(path, {
      try(devtools::test(path, filter = .filter))
      message("================================================================================")
      print(proc.time() - pt)
      message("================================================================================")
    })
  })
}

#' @rdname rxValidate
#' @export
rxTest <- rxValidate


