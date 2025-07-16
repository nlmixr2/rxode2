
#' Require namespace, otherwise throw error.
#'
#' @param pkg Package required for function to work.
#' @return Nothing
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
rxReq <- function(pkg) {
  ## nocov start
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(gettext("package \"%s\" needed for this function to work"), pkg),
      call. = FALSE
    )
  }
  ## nocov end
}
