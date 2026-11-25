#' Add direct linear effect with baseline=0
#'
#' @inheritParams addIndirectLin
#' @return model with direct linear effect added (baseline=0)
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
#'
#' # Direct linear model
#' pk2cmtNoDepot |>
#'   addDirectLin()
#'
#' # Direct emax model
#' pk2cmtNoDepot |>
#'   addDirectLin() |>
#'   convertEmax()
#'
addDirectLin <- function(ui, ek = "Ek", cc = c("Ce", "Cc"), effect = "effect") {
  if (missing(ui)) {
    return(fakeCc(addDirectLin, ek = ek, cc = cc, effect = effect))
  }
  .ui <- assertRxUi(ui)
  cc <- assertExists(.ui, cc)
  .effectSd <- defaultCombine(effect, "sd")
  assertVariableNew(.ui, ek)
  assertVariableNew(.ui, effect)
  assertVariableNew(.ui, .effectSd)

  .eff <- str2lang(paste0(effect, " <- ", ek, "*", cc))
  .modelLines <- c(list(paste0(ek, " <- u", ek)), .ui$lstExpr, .eff)
  .errLine <- str2lang(paste0(effect, " ~ add(", .effectSd, ")"))

  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- .iniAddTheta(.ui, paste0("u", ek), label = paste0("untransformed slope (", ek, ")"))
  # the endpoint goes on by itself so rxode2 creates the residual parameter
  # and decides its condition, err and lower bound
  .ui <- .modelAppend(.ui, list(.errLine))
  .iniAddTheta(.ui, .effectSd, label = paste0("additive error for ", effect))
}
