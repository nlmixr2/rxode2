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
