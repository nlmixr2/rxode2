#' Convert linear effect to Emax effect
#'
#' @param ui rxode2 model
#'
#' @param emax Emax parameter
#' @param ec50 EC50 parameter
#' @param imax Imax parameter used when input model contains "Ik"
#'   instead of "Ek"
#' @param ic50 IC50 parameter used when input model contains "Ik"
#'   instead of "Ek"
#' @inheritParams addIndirectLin
#'
#'
#' @return Model with the linear effect converted to an Emax effect
#'
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
#'   addIndirectLin(stim = "in") |>
#'   convertEmax()
#'
#' # When emax=1
#' pk2cmtNoDepot |>
#'   addIndirectLin(stim = "in") |>
#'   convertEmax(emax = 1)
#'
convertEmax <- function(
  ui,
  emax = "Emax",
  ec50 = "EC50",
  imax = "Imax",
  ic50 = "IC50",
  ek = c("Ik", "Ek"),
  cc = c("Ec", "Cc")
) {
  .ui <- assertRxUi(ui)
  cc <- assertExists(.ui, cc)
  ek <- assertVariableExists(.ui, ek)
  if (ek == "Ik") {
    emax <- imax
    ec50 <- ic50
  }
  if (inherits(emax, "character")) {
    assertVariableNew(.ui, emax)
    .emaxMult <- paste0(emax, "*")
  } else if (is.numeric(emax) && emax == 1.0) {
    .emaxMult <- ""
  } else {
    stop("'", emax, "' not specified correctly", call. = FALSE)
  }
  assertVariableNew(.ui, ec50)
  .modelLines <- .replaceMult(.ui$lstExpr, v1 = ek, v2 = cc, ret = paste0(.emaxMult, cc, "/(", cc, "+", ec50, ")"))
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .theta <- .tmp$theta
  .eta <- .tmp$eta
  .tmp <- .dropLines(.ui, .modelLines, .theta, .eta, ek)
  .modelLines <- .tmp$modelLines
  .theta <- .tmp$theta
  .eta <- .tmp$eta

  if (length(.theta$ntheta) == 0) {
    .ntheta <- 0
  } else {
    .ntheta <- max(.theta$ntheta)
  }

  .emaxLine <- NULL
  if (.emaxMult != "") {
    .emaxLine <- list(str2lang(paste0(emax, "<- exp(l", emax, ")")))
  }

  .ui <- rxUiDecompress(.ui)
  # .theta/.eta are subsets of this model's own iniDf (ek dropped), so binding
  # them assumes nothing about its columns; the new parameters go in via ini().
  .ui$iniDf <- rbind(.theta, .eta)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- c(.emaxLine, list(str2lang(paste0(ec50, "<- exp(l", ec50, ")"))), .modelLines)
  if (!is.null(.emaxLine)) {
    .ui <- .iniAddTheta(.ui, paste0("l", emax), label = paste0("Maximum effect (", emax, ")"))
  }
  .iniAddTheta(.ui, paste0("l", ec50), label = paste0("Concentration of 50% ", emax, " (", emax, ")"))
}

#'  Convert linear effect to Emax-Hill effect
#'
#' @inheritParams convertEmax
#' @param g hill coefficient
#' @return Model with the linear effect converted to an Emax effect
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
#'   addIndirectLin(stim = "in") |>
#'   convertEmaxHill()
#'
#' # can also specify as emax=1
#'
#' pk2cmtNoDepot |>
#'   addIndirectLin(stim = "in") |>
#'   convertEmaxHill(emax = 1)
#'
convertEmaxHill <- function(
  ui,
  emax = "Emax",
  ec50 = "EC50",
  g = "g",
  imax = "Imax",
  ic50 = "IC50",
  ek = c("Ik", "Ek"),
  cc = c("Ec", "Cc")
) {
  .ui <- assertRxUi(ui)
  cc <- assertExists(.ui, cc)
  ek <- assertVariableExists(.ui, ek)
  if (ek == "Ik") {
    emax <- imax
    ec50 <- ic50
  }
  .emaxMult <- paste0(emax, "*")
  if (inherits(emax, "character")) {
    assertVariableNew(.ui, emax)
  } else if (is.numeric(emax) && emax == 1.0) {
    .emaxMult <- ""
  } else {
    stop("'", emax, "' not specified correctly", call. = FALSE)
  }
  assertVariableNew(.ui, ec50)

  .modelLines <- .replaceMult(
    .ui$lstExpr,
    v1 = ek,
    v2 = cc,
    ret = paste0(.emaxMult, cc, "^", g, "/(", cc, "^", g, "+", ec50, "^", g, ")")
  )
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .theta <- .tmp$theta
  .eta <- .tmp$eta
  .tmp <- .dropLines(.ui, .modelLines, .theta, .eta, ek)
  .modelLines <- .tmp$modelLines
  .theta <- .tmp$theta
  .eta <- .tmp$eta

  if (length(.theta$ntheta) == 0) {
    .ntheta <- 0
  } else {
    .ntheta <- max(.theta$ntheta)
  }

  .emaxLine <- NULL
  if (.emaxMult != "") {
    .emaxLine <- list(str2lang(paste0(emax, "<- exp(l", emax, ")")))
  }

  .ui <- rxUiDecompress(.ui)
  # .theta/.eta are subsets of this model's own iniDf (ek dropped), so binding
  # them assumes nothing about its columns; the new parameters go in via ini().
  .ui$iniDf <- rbind(.theta, .eta)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- c(
    .emaxLine,
    list(str2lang(paste0(ec50, "<- exp(l", ec50, ")")), str2lang(paste0(g, "<- expit(lg", g, ", 0.1, 10)"))),
    .modelLines
  )
  if (!is.null(.emaxLine)) {
    .ui <- .iniAddTheta(.ui, paste0("l", emax), label = paste0("Maximum effect (", emax, ")"))
  }
  .ui <- .iniAddTheta(.ui, paste0("l", ec50), label = paste0("Concentration of 50% ", emax, " (", emax, ")"))
  .iniAddTheta(.ui, paste0("lg", g), est = logit(1, 0.1, 10), label = paste0("logit-constrained Hill coefficient ", g))
}
