#' Collect warnings and just warn once.
#'
#' @param expr R expression
#' @param lst When `TRUE` return a list with
#'     list(object,warnings) instead of issuing the warnings.
#'     Otherwise, when `FALSE` issue the warnings and return the
#'     object.
#' @return The value of the expression or a list with the value of
#'     the expression and a list of warning messages
#' @author Matthew L. Fidler
#' @export
.collectWarnings <- function(expr, lst = FALSE) {
  .ws <- NULL
  .thisEnv <- environment()
  .ret <- suppressWarnings(
    withCallingHandlers(expr,
      warning = function(w) {
        assign(".ws", unique(c(w$message, .ws)), .thisEnv)
      }
    )
  )
  if (lst) {
    return(list(.ret, .ws))
  } else {
    for (.w in .ws) {
      warning(.w, call. = FALSE)
    }
    return(.ret)
  }
}

