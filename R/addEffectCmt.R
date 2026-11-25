#' Add effect compartment
#'
#' @inheritParams addIndirect
#' @param ke0 This is the effect compartment keo rate
#' @param ce This is the concentration in the effect compartment
#' @return a model with an effect compartment attached
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
#'   addEffectCmtLin()
#'
#' # Can also be changed to the more typical Emax with constant (estimated) baselie
#' pk2cmtNoDepot |>
#'   addEffectCmtLin() |>
#'   convertEmaxHill() |>
#'   addBaselineConst()
#'
addEffectCmtLin <- function(ui, ke0 = "ke0", cc = "Cc", ce = "Ce", ek = "Ek", effect = "effect") {
  if (missing(ui)) {
    return(fakeCc(addEffectCmtLin, ke0 = ke0, cc = cc, ce = ce, ek = ek, effect = effect))
  }
  .effectSd <- defaultCombine(effect, "sd")
  .ui <- assertRxUi(ui)
  .ui <- rxUiDecompress(.ui)
  assertExists(.ui, cc)
  assertCompartmentNew(.ui, ce)
  assertCompartmentNew(.ui, ek)
  assertVariableNew(.ui, effect)
  assertVariableNew(.ui, .effectSd)
  .ce <- str2lang(paste0("d/dt(", ce, ") <- ", ke0, "*(", cc, "-", ce, ")"))
  .ef <- str2lang(paste0("effect <- ", ce, "*", ek))
  .err <- str2lang(paste0("effect ~ add(", .effectSd, ")"))
  .modelLines <- c(
    list(str2lang(paste0(ke0, "<- exp(l", ke0, ")")), str2lang(paste0(ek, "<- u", ek))),
    .ui$lstExpr,
    list(.ce, .ef)
  )

  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- .iniAddTheta(.ui, paste0("l", ke0), label = paste0("effect compartment rate (", ke0, ")"))
  .ui <- .iniAddTheta(.ui, paste0("u", ek), label = paste0("untransformed linear slope (", ek, ")"))
  # the endpoint goes on by itself so rxode2 creates the residual parameter
  # and decides its condition, err and lower bound
  .ui <- .modelAppend(.ui, list(.err))
  .iniAddTheta(.ui, .effectSd, label = paste0("additive error for ", effect))
}
