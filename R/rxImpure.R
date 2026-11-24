## Names registered with rxImpureFun()
.rxImpureEnv <- new.env(parent = emptyenv())
.rxImpureEnv$names <- character(0)

#' Register model functions whose calls must not be shared or skipped
#'
#' `rxOptExpr()` and the `rxode2.fullOpt` code generation treat user R
#' functions, `rxFun()` C functions and C functions from other packages as
#' pure: the same arguments give the same value with no side effects, so
#' repeated calls are evaluated once and a call whose result is unused may
#' be left out.  Random draws, `simeta()` and `simeps()` are always impure;
#' register any other function that draws random numbers, keeps state or
#' has side effects so every call is kept.  Registration changes the
#' generated code, so models compiled afterwards are recompiled.
#'
#' @param name character vector of function names, as written in the
#'   model
#' @return `rxImpureFun()` and `rxRmImpureFun()` invisibly return the
#'   registered names; `rxImpureFun()` without `name` returns them
#'   visibly
#' @author Matthew L. Fidler
#' @export
#' @family User functions
#' @examples
#' rxImpureFun("myCounter")
#' rxImpureFun()
#' rxRmImpureFun("myCounter")
rxImpureFun <- function(name) {
  if (missing(name)) {
    return(.rxImpureEnv$names)
  }
  checkmate::assertCharacter(name, any.missing = FALSE, min.chars = 1)
  .rxImpureEnv$names <- sort(unique(c(.rxImpureEnv$names, name)))
  invisible(.rxImpureEnv$names)
}

#' @rdname rxImpureFun
#' @export
rxRmImpureFun <- function(name) {
  checkmate::assertCharacter(name, any.missing = FALSE, min.chars = 1)
  .rxImpureEnv$names <- setdiff(.rxImpureEnv$names, name)
  invisible(.rxImpureEnv$names)
}

## The registered names (or `names`) together with the C spelling of any registered
## translation (rxFun()/package C function) they name
.rxImpureFunNames <- function(names = NULL) {
  .n <- if (is.null(names)) .rxImpureEnv$names else names
  if (length(.n) == 0L) {
    return(character(0))
  }
  .df <- rxode2parseGetTranslation()
  if (is.data.frame(.df) && nrow(.df) > 0L) {
    .n <- c(.n, .df$fun[.df$rxFun %in% .n])
  }
  unique(.n)
}
