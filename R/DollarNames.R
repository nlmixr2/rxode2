#' @importFrom utils .DollarNames
#' @export
.DollarNames.rxSolve <- function(x, pattern) {
  # the parameter names are listed, so build $params here rather than from C
  .env <- attr(class(x), ".rxode2.env")
  if (is.environment(.env)) {
    .rxSolveMaterializeParams(x, .env)
  }
  grep(pattern, .Call(`_rxode2_rxSolveDollarNames`, x), value = TRUE)
}
