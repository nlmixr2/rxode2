.rstudioComplete <- new.env(parent = emptyenv())
.rstudioComplete$running <- FALSE
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

#' @export
#' @rdname rxUiGet
rxUiGet.000rstudio <- function(x, ...) {
  .rstudioComplete$running <- TRUE
  stop("rxUiGet.000rstudio dummy function for completion")
  return(invisible())
}

#' @export
#' @rdname rxUiGet
rxUiGet.zzzzrstudio <- function(x, ...) {
  .rstudioComplete$running <- FALSE
  stop("rxUiGet.000rstudio dummy function for completion")
  return(invisible())
}

#' Determine if Rstudio completion 2025+ is running
#'
#' @return boolean, is Rstudio completion 2025+ running
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
#' @examples
#' # Determines if rstudio is running
#' .rstudioComplete()
.rstudioComplete <- function() {
  .rstudioComplete$running
}
