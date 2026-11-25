#' Converts a linear effect to a log-linear effect
#'
#' @inheritParams addIndirect
#' @return model converted from linear to log-linear effect
#' @family Pharmacodynamic building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#' pk2cmtNoDepot <- function() {
#'   ini({
#'     lcl <- 1; label("Clearance (CL)")
#'     lvc <- 3; label("Central volume of distribution (V)")
#'     lvp <- 5; label("Peripheral volume of distribution (Vp)")
#'     lq <- 0.1; label("Intercompartmental clearance (Q)")
#'     propSd <- 0.5; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     vp <- exp(lvp)
#'     q <- exp(lq)
#'     kel <- cl / vc
#'     k12 <- q / vc
#'     k21 <- q / vp
#'     d/dt(central) <- -kel * central - k12 * central + k21 * peripheral1
#'     d/dt(peripheral1) <- k12 * central - k21 * peripheral1
#'     Cc <- central / vc
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
#'
#' pk2cmtNoDepot |>
#'   addDirectLin() |>
#'   convertLogLin()
#'
#' pk2cmtNoDepot |>
#'   addIndirectLin(stim = "out") |>
#'   convertLogLin()
convertLogLin <- function(ui, ek = c("Ik", "Ek"), cc = c("Ce", "Cc")) {
  .ui <- assertRxUi(ui)
  cc <- assertExists(.ui, cc)
  ek <- assertVariableExists(.ui, ek)
  .modelLines <- .replaceMult(.ui$lstExpr, v1 = ek, v2 = cc, ret = paste0(ek, "*log(", cc, ")"))
  .ui <- rxUiDecompress(.ui)
  ## .ui$iniDf <- rbind(.theta,
  ##                    .thetaEk,
  ##                    .thetaErr,
  ##                    .eta)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui
}
