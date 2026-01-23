.forderEnv <- new.env()
.forderEnv$useBase <- FALSE
.forderEnv$useFastMatch <- TRUE


.forder3 <- function(c1,c2,c3, decreasing=FALSE) {
  data.table::data.table(c1=c1,
                         c2=c2,
                         c3=c3,
                         decreasing=decreasing,
                         na.last=TRUE)[order(c1, c2, c3), which=TRUE]
}

.border3 <- function(c1,c2,c3, decreasing=FALSE) {
  base::order(c1, c2, c3, decreasing=decreasing, na.last=NA, method="radix")
}

.forder1 <- function(c1, decreasing=FALSE) {
  data.table::data.table(c1=c1)[order(c1, decreasing=decreasing, na.last=TRUE), which=TRUE]
}

.border1 <- function(c1, decreasing=FALSE) {
  base::order(c1, na.last=NA, decreasing=decreasing, method="radix")
}

.order1 <- function(c1, decreasing=FALSE) {
  if (.forderEnv$useBase) {
    .border1(c1, decreasing=decreasing)
  } else {
    .forder1(c1, decreasing=decreasing)
  }
}

.order3 <- function(c1,c2,c3, decreasing=FALSE) {
  if (.forderEnv$useBase) {
    .border3(c1,c2,c3, decreasing=decreasing)
  } else {
    .forder3(c1,c2,c3, decreasing=decreasing)
  }
}
#' Force using base order for rxode2 radix sorting
#'
#' @param forceBase boolean indicating if rxode2 should use R's
#'   [order()] for radix sorting instead of
#'   `data.table`'s parallel radix sorting.
#'
#' @return value of `forceBase` (can change if `data.table` is not
#'   available)
#'
#' @examples
#' \donttest{
#' forderForceBase(TRUE) # Use base `order` for rxode2 sorts
#' forderForceBase(FALSE) # Use `data.table` for rxode2 sorts
#' }
#' @export
#' @keywords internal
forderForceBase <- function(forceBase = FALSE){
  if (forceBase) {
    .forderEnv$useBase <- forceBase
  } else if (requireNamespace("data.table", quietly = TRUE)) {
    .forderEnv$useBase <- forceBase
  } else {
    .forderEnv$useBase <- TRUE
  }
  invisible(.forderEnv$useBase)
}

#' Control fastmatch usage for membership testing
#'
#' @param useFastMatch boolean indicating if rxode2 should use
#'   \code{fastmatch::fmatch()} for membership tests instead of base R's
#'   \code{match()}. Default is TRUE when fastmatch is available.
#'
#' @return value of \code{useFastMatch}
#'
#' @examples
#' \donttest{
#' chinUseFastMatch(TRUE)  # Use fastmatch (default)
#' chinUseFastMatch(FALSE) # Use base match()
#' }
#' @export
#' @keywords internal
chinUseFastMatch <- function(useFastMatch = TRUE) {
  if (useFastMatch && requireNamespace("fastmatch", quietly = TRUE)) {
    .forderEnv$useFastMatch <- TRUE
  } else {
    .forderEnv$useFastMatch <- FALSE
  }
  invisible(.forderEnv$useFastMatch)
}

.chin <- function(x, table) {
  if (.forderEnv$useFastMatch && requireNamespace("fastmatch", quietly = TRUE)) {
    !is.na(fastmatch::fmatch(x, table))
  } else {
    !is.na(match(x, table))
  }
}
