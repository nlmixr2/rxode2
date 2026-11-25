#' Convert linear elimination to quadratic elimination
#'
#' @inheritParams addIndirect
#' @param ek2 quadratic coefficient
#' @return model with linear effect converted to quadratic effect
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
#'   addIndirectLin(stim = "out") |>
#'   convertQuad()
#'
#' pk2cmtNoDepot |>
#'   addDirectLin() |>
#'   convertQuad()
#'
#' pk2cmtNoDepot |>
#'   addEffectCmtLin() |>
#'   convertQuad()
convertQuad <- function(ui, ek = c("Ik", "Ek"), cc = c("Ce", "Cc"), ek2 = "Ek2") {
  .ui <- assertRxUi(ui)
  .ui <- rxUiDecompress(.ui)
  cc <- assertExists(.ui, cc)
  ek <- assertVariableExists(.ui, ek)
  .modelLines <- c(
    list(str2lang(paste0(ek2, " <- u", ek2))),
    .replaceMult(.ui$lstExpr, v1 = ek, v2 = cc, ret = paste0(ek, "*", cc, "+", ek2, "*", cc, "^2"))
  )
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .iniAddTheta(.ui, paste0("u", ek2), label = paste0("untransformed quadratic slope (", ek2, ")"))
}
