#' Determine if Rstudio completion 2025+ is running
#'
#' @return boolean, is Rstudio completion 2025+ running
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
#' @examples
#' # Determines if rstudio is running completion
#' .rstudioComplete()
.rstudioComplete <- function() {
  .sc <- try(sys.calls(), silent=TRUE)
  if (inherits(.sc, "try-error")) {
    return(FALSE)
  }
  .sc <- try(.sc[[1]], silent=TRUE)
  if (inherits(.sc, "try-error")) {
    return(FALSE)
   }
  .sc <- try(.sc[[1]], silent=TRUE)
  if (inherits(.sc, "try-error")) {
    return(FALSE)
  }
  identical(quote(`.rs.rpc.get_completions`), .sc)
}
