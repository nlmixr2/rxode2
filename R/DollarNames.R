#' @importFrom utils .DollarNames
#' @export
.DollarNames.rxSolve <- function(x, pattern) {
  grep(pattern, .Call(`_rxode2_rxSolveDollarNames`, x), value = TRUE)
}
