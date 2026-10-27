#' @examples
#' \donttest{
#' ev <- et(amt = 100, cmt = "depot") |> et(0:24)
#' # splitBolus(): a depot bolus split into fast and slow absorption
#' f <- function() {
#'   ini({ka1 <- 1; ka2 <- 0.2; fr <- 0.6; cl <- 1; v <- 10})
#'   model({
#'     splitBolus(depot, depot1, depot2)
#'     f(depot1) <- fr
#'     f(depot2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(depot1) <- -ka1 * depot1
#'     d/dt(depot2) <- -ka2 * depot2
#'     d/dt(central) <- ka1 * depot1 + ka2 * depot2 - cl / v * central
#'   })
#' }
#' rxSolve(f, ev)
#' # splitInfusion(): a depot infusion split between two compartments
#' f <- function() {
#'   ini({fr <- 0.6; k1 <- 0.5; k2 <- 0.05})
#'   model({
#'     splitInfusion(depot, c1, c2)
#'     f(c1) <- fr
#'     f(c2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(c1) <- -k1 * c1
#'     d/dt(c2) <- -k2 * c2
#'   })
#' }
#' rxSolve(f, et(amt = 100, rate = 20, cmt = "depot") |> et(0:24))
#' # splitInfusionBolus(): a depot bolus becomes a central infusion plus a
#' # depot2 bolus; splitBolusInfusion(depot, depot2, central) is the same
#' # split with the targets in the other order
#' f <- function() {
#'   ini({tk0 <- 4; fr <- 0.7; ka2 <- 1.2; cl <- 0.1; v <- 10})
#'   model({
#'     splitInfusionBolus(depot, central, depot2)
#'     dur(central) <- tk0
#'     f(central) <- fr
#'     f(depot2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(depot2) <- -ka2 * depot2
#'     d/dt(central) <- ka2 * depot2 - cl / v * central
#'   })
#' }
#' rxSolve(f, ev)
#' }
