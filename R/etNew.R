# Default show flags (controls which columns print/as.data.frame expose)
.etDefaultShow <- function() {
  c(id = FALSE, low = FALSE, time = TRUE, high = FALSE, cmt = FALSE,
    amt = FALSE, rate = FALSE, ii = FALSE, addl = FALSE, evid = TRUE,
    ss = FALSE, dur = FALSE)
}

#' Create a new empty rxEt object
#' @param amountUnits character dose unit, e.g. "mg"
#' @param timeUnits character time unit, e.g. "hours"
#' @return rxEt object
#' @noRd
.newRxEt <- function(amountUnits = NA_character_, timeUnits = NA_character_) {
  .env <- new.env(parent = emptyenv())
  .env$chunks    <- list()
  .env$units     <- c(dosing = amountUnits, time = timeUnits)
  .env$show      <- .etDefaultShow()
  .env$IDs       <- 1L
  .env$nobs      <- 0L
  .env$ndose     <- 0L
  .env$randomType <- NA_integer_
  .env$canResize  <- TRUE
  structure(list(.env = .env), class = "rxEt")
}

#' Check if object is an rxEt event table
#' @param x object to test
#' @return logical
#' @export
is.rxEt <- function(x) {
  if (!inherits(x, "rxEt")) return(FALSE)
  # New-style: list with .env environment (pure-R rewrite)
  .env <- .subset2(x, ".env")
  if (is.environment(.env)) return(TRUE)
  # Old-style: data.frame-based rxEt (C++ et_() output, has .rxode2.lst in class attr)
  !is.null(attr(class(x), ".rxode2.lst"))
}
