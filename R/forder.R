.forderEnv <- new.env()
.forderEnv$useBase <- FALSE


# Below this many rows building a data.table costs more than base's radix sort
# (it ran once per etTrans() in posologyr MAP estimation, levenc/posologyr#70).
# Both give the same stable, NA-last order, except that base ties NA with NaN
# where data.table puts NaN first, so a double with missing values keeps
# data.table.
.forderMinRows <- 1e5

.forderUseBase <- function(...) {
  .cols <- list(...)
  length(.cols[[1]]) < .forderMinRows &&
    !any(vapply(.cols, function(x) is.double(x) && anyNA(x), logical(1)))
}

.forder3 <- function(c1, c2, c3, decreasing = FALSE) {
  if (.forderUseBase(c1, c2, c3)) {
    return(base::order(c1, c2, c3, na.last = TRUE, method = "radix"))
  }
  data.table::data.table(c1 = c1, c2 = c2, c3 = c3, decreasing = decreasing, na.last = TRUE)[
    order(c1, c2, c3),
    which = TRUE
  ]
}

.border3 <- function(c1, c2, c3, decreasing = FALSE) {
  base::order(c1, c2, c3, decreasing = decreasing, na.last = NA, method = "radix")
}

.forder1 <- function(c1, decreasing = FALSE) {
  if (.forderUseBase(c1)) {
    return(base::order(c1, decreasing = decreasing, na.last = TRUE, method = "radix"))
  }
  data.table::data.table(c1 = c1)[order(c1, decreasing = decreasing, na.last = TRUE), which = TRUE]
}

.border1 <- function(c1, decreasing = FALSE) {
  base::order(c1, na.last = NA, decreasing = decreasing, method = "radix")
}

.order1 <- function(c1, decreasing = FALSE) {
  if (.forderEnv$useBase) {
    .border1(c1, decreasing = decreasing)
  } else {
    .forder1(c1, decreasing = decreasing)
  }
}

.order3 <- function(c1, c2, c3, decreasing = FALSE) {
  if (.forderEnv$useBase) {
    .border3(c1, c2, c3, decreasing = decreasing)
  } else {
    .forder3(c1, c2, c3, decreasing = decreasing)
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
#' @family Solver options and performance
forderForceBase <- function(forceBase = FALSE) {
  if (forceBase) {
    .forderEnv$useBase <- forceBase
  } else if (requireNamespace("data.table", quietly = TRUE)) {
    .forderEnv$useBase <- forceBase
  } else {
    .forderEnv$useBase <- TRUE
  }
  invisible(.forderEnv$useBase)
}

.chin <- function(x, table) {
  x %in% table
}

#' Would `sortIds()` reorder the solve for this problem size?
#'
#' Mirrors the C-level gate exactly so that the throttle's documented
#' direction -- suppress the sort when `nall * throttle <= cores` -- is
#' asserted by a test rather than only by prose.
#'
#' @param cores Number of threads the solve will use.
#' @param nall Number of subject-solves (subjects times simulations).
#' @param throttle Throttle from [setRxThreads()]; there is no R-level
#'   getter for it, so callers pass the value they set.
#' @return `TRUE` when the run-time sort is taken, `FALSE` when the throttle
#'   suppresses it.
#' @noRd
.rxSortIdsWanted <- function(cores, nall, throttle) {
  .Call(`_rxode2_sortIdsWanted_`, as.integer(cores), as.double(nall), as.integer(throttle))
}
