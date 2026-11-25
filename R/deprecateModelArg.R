#' Translate a deprecated \code{model} argument to \code{ui}
#'
#' Called as the first statement of a function that used to accept
#' \code{model} as its first argument but now accepts \code{ui}.
#' Inspects the calling function's \code{match.call()} to detect which
#' of \code{ui} / \code{model} was supplied and, when only \code{model}
#' was supplied, warns and assigns its value to \code{ui} in the
#' caller's frame.
#'
#' @return Invisibly \code{NULL}; called for its side effect on the
#'   calling function's frame.
#' @noRd
.useModelAsUi <- function() {
  callerEnv <- parent.frame()
  callerFun <- sys.function(-1L)
  callerCall <- match.call(definition = callerFun, call = sys.call(-1L))
  nms <- names(callerCall)
  hasModel <- "model" %in% nms
  hasUi <- "ui" %in% nms
  if (hasModel) {
    if (hasUi) {
      stop("'model' is deprecated and may not be supplied together with 'ui'; use 'ui' only", call. = FALSE)
    }
    warning("argument 'model' is deprecated; use 'ui' instead", call. = FALSE)
    assign("ui", get("model", envir = callerEnv), envir = callerEnv)
  }
  invisible(NULL)
}
