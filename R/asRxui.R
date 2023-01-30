#' As rxode2 ui
#'
#' @param x Object to convert to `rxUi` object
#' 
#' @return rxUi object (or error if it cannot be converted)
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' mod1 <- function() {
#'  ini({
#'    # central 
#'    KA=2.94E-01
#'    CL=1.86E+01
#'    V2=4.02E+01
#'    # peripheral
#'    Q=1.05E+01
#'    V3=2.97E+02
#'    # effects
#'    Kin=1
#'    Kout=1
#'    EC50=200 
#'  })
#'  model({
#'    C2 <- centr/V2
#'    C3 <- peri/V3
#'    d/dt(depot) <- -KA*depot
#'    d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
#'    d/dt(peri)  <- Q*C2 - Q*C3
#'    eff(0) <- 1
#'    d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
#'  })
#' }
#' 
#' as.rxUi(mod1)
#' 
as.rxUi <- function(x) {
  UseMethod("as.rxUi")
}
#' @rdname as.rxUi
#' @export
as.rxUi.rxode2 <- function(x) {
  as.rxUi(as.function(x))
}
#' @export
#' @rdname as.rxUi
as.rxUi.rxModelVars <- function(x) {
  as.rxUi(as.function(x))
}
#' @export
#' @rdname as.rxUi
as.rxUi.function <- function(x) {
  model <- try(rxode2(x), silent=TRUE)
  if (inherits(model, "try-error")) {
    stop("cannot convert to rxUi object", call.=FALSE)
  }
  model
}
#' @export
#' @rdname as.rxUi 
as.rxUi.rxUi <- function(x) {
  x
}
#' @export
#' @rdname as.rxUi
as.rxUi.default <- function(x) {
  stop("could not convert to rxUi object", call.=FALSE)
}
