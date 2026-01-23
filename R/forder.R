.forderEnv <- new.env()
.forderEnv$useBase <- FALSE
.forderEnv$useFastMatch <- if (requireNamespace("fastmatch", quietly = TRUE)) TRUE else FALSE


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
#' inUseFastMatch(TRUE)  # Use fastmatch (default)
#' inUseFastMatch(FALSE) # Use base match()
#' }
#' @export
#' @keywords internal
inUseFastMatch <- function(useFastMatch = TRUE) {
  if (useFastMatch) {
    if (requireNamespace("fastmatch", quietly = TRUE)) {
      .forderEnv$useFastMatch <- useFastMatch
    } else {
      .forderEnv$useFastMatch <- FALSE
    }
  } else {
    .forderEnv$useFastMatch <- useFastMatch
  }
  invisible(.forderEnv$useFastMatch)
}
#' Fast replacement for %in%
#'
#' This is a fast replacement for \code{\%in\%} that uses
#' \code{fastmatch::fmatch()} when available.
#'
#'
#' @param x the x variable in \code{x \%in\% table}
#' @param table the table variable in \code{x \%in\% table}
#' @return logical vector indicating if elements of x are in table,
#'   possibly containing a hash table attribute for fastmatch speed
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
#' @examples
#'
#' .in(1:5, c(2,4,6))
#'
.in <- function(x, table) {
  if (.forderEnv$useFastMatch) {
    !is.na(fastmatch::fmatch(x, table))
  } else {
    !is.na(match(x, table))
  }
}
