#' Extract fixed effects from an rxode2 model (rxUi)
#'
#' @param object rxode2 ui model or a function that evaluates to one
#' @param ... other parameters (currently ignored)
#' @return named numeric vector of the fixed-effect (theta) estimates
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' one.compartment <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- 1
#'     tv <- 3.45
#'     eta.ka ~ 0.6
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     d/dt(depot) <- -ka * depot
#'     d/dt(center) <- ka * depot - cl / v * center
#'     cp <- center / v
#'   })
#' }
#'
#' ui <- one.compartment()
#' nlme::fixef(ui)
#' @keywords internal
fixef.rxUi <- function(object, ...) {
  object$theta
}

#' @rdname fixef.rxUi
#' @keywords internal
fixef.function <- function(object, ...) {
  ui <- rxode2(object)
  ui$theta
}

#' Extract the initial coefficients from an rxode2 model (rxUi)
#'
#' For an rxode2 model specification (`rxUi`) there are no realized random
#' effects (etas), so `coef()` returns the fixed-effect (`theta`) estimates by
#' default.  The random-effect variability matrix (`omega`) can be requested
#' with `level="omega"`, or both can be returned together with `level="all"`.
#'
#' @param object rxode2 ui model or a function that evaluates to one
#' @param level which coefficients to return: `"theta"`/`"fixed"` (the default)
#'   returns the named fixed-effect estimates; `"omega"`/`"random"` returns the
#'   random-effect variability matrix (or `NULL` when the model has no random
#'   effects); `"all"`/`"both"` returns a list with `theta` and `omega`.
#' @param ... other parameters (currently ignored)
#' @return depends on `level`: a named numeric vector of fixed effects, the
#'   `omega` matrix (or `NULL`), or a list with both `theta` and `omega`.
#' @author Matthew L. Fidler
#' @importFrom stats coef
#' @examples
#' one.compartment <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- 1
#'     tv <- 3.45
#'     eta.ka ~ 0.6
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     d/dt(depot) <- -ka * depot
#'     d/dt(center) <- ka * depot - cl / v * center
#'     cp <- center / v
#'   })
#' }
#'
#' ui <- one.compartment()
#' coef(ui)
#' coef(ui, level="omega")
#' coef(ui, level="all")
#' @export
coef.rxUi <- function(object,
                      level = c("theta", "fixed", "omega", "random", "all", "both"),
                      ...) {
  level <- match.arg(level)
  if (level %in% c("theta", "fixed")) {
    return(object$theta)
  }
  if (level %in% c("omega", "random")) {
    return(object$omega)
  }
  list(theta = object$theta, omega = object$omega)
}

#' @rdname coef.rxUi
#' @export
coef.function <- function(object,
                          level = c("theta", "fixed", "omega", "random", "all", "both"),
                          ...) {
  coef.rxUi(rxode2(object), level = level, ...)
}
