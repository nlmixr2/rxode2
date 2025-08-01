#' Get the version of RStudio running
#'
#' @return NULL if Rstudio is not running or rstudioapi isn't installed
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
#' @examples
#' .rstudio()
.rstudio <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(numeric_version("0.0.0"))
  } else {
    .ret <- try(rstudioapi::getVersion(), silent=TRUE)
    if (inherits(.ret, "try-error")) {
      return(numeric_version("0.0.0"))
    } else {
      return(.ret)
    }
  }
}

.rstudioCompleteEnv <- new.env(parent = emptyenv())
.rstudioCompleteEnv$evalComplete <- NULL
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
  if (is.null(.rstudioCompleteEnv$evalComplete)) {
    .rstudioCompleteEnv$evalComplete <- .rstudio() >= numeric_version("2025.0.0")
  }
  if (!.rstudioCompleteEnv$evalComplete) return(FALSE)
  .sc <- try(sys.calls(), silent=TRUE)
  if (inherits(.sc, "try-error")) {
    return(FALSE)
  }
  .sc <- try(.sc[[1]], silent=TRUE)
  if (inherits(.sc, "try-error")) {
    return(FALSE)
  }
  .sc <- try(.sc[[1]], silent=TRUE)
  identical(quote(`.rs.rpc.get_completions`), .sc)
}
