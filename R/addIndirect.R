#' Add linear indirect response model
#'
#' @param ui rxode2 model
#'
#' @param stim what type of stimulation indirect response model:
#'
#' - `in`: stimulation of input
#'
#' - `out`: stimulation of output
#'
#' @param inhib what type of inhibition indirect response model:
#'
#' - `in`: inhibition of input
#'
#' - `out`: inhibition of output
#'
#' @param ek simulation linear constant
#'
#' @param ik inhibition linear constant
#'
#' @param kin this is the kin parameter name
#'
#' @param kout this is the kout parameter name
#'
#' @param R drug response compartment
#'
#' @param cc the concentration value
#'
#' @param effect the effect variable that will be modeled
#'
#' @return model with linear indirect effect added
#'
#' Note that while linear indirect effects are not common, it allows an easier
#' hook to produce other standard effect curves like Emax/Imax, Hill, etc.
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
#' pk2cmtNoDepot |> addIndirectLin(stim = "in")
#'
#' pk2cmtNoDepot |> addIndirectLin(stim = "out")
#'
#' pk2cmtNoDepot |> addIndirectLin(inhib = "in")
#'
#' pk2cmtNoDepot |> addIndirectLin(inhib = "out")
#'
addIndirectLin <- function(
  ui,
  stim = c("in", "out"),
  inhib = c("in", "out"),
  ek = "Ek",
  ik = "Ik",
  kin = "kin",
  kout = "kout",
  cc = "Cc",
  R = "R",
  effect = "effect"
) {
  if (
    (missing(stim) && missing(inhib)) ||
      (!missing(stim) && !missing(inhib))
  ) {
    stop("need either 'stim' or 'inhib' specified", call. = FALSE)
  }
  .doStim <- FALSE
  if (!missing(stim)) {
    if (missing(ui)) {
      return(fakeCc(
        addIndirectLin,
        stim = stim,
        ek = ek,
        ik = ik,
        kin = kin,
        kout = kout,
        cc = cc,
        R = R,
        effect = effect
      ))
    }
    .doStim <- TRUE
    stim <- match.arg(stim)
  } else {
    if (missing(ui)) {
      return(fakeCc(
        addIndirectLin,
        stim = stim,
        ek = ek,
        ik = ik,
        kin = kin,
        kout = kout,
        cc = cc,
        R = R,
        effect = effect
      ))
    }
    inhib <- match.arg(inhib)
  }
  .effectSd <- defaultCombine(effect, "sd")
  .ui <- assertRxUi(ui)
  assertExists(.ui, cc)
  assertVariableNew(.ui, kin)
  assertVariableNew(.ui, kout)
  assertCompartmentNew(.ui, R)
  assertVariableNew(.ui, effect)
  assertVariableNew(.ui, .effectSd)
  .modelLines <- .ui$lstExpr
  if (.doStim) {
    assertVariableNew(.ui, ek)
    if (stim == "in") {
      .eff <- str2lang(paste0("d/dt(", R, ") <- ", kin, "*(1+", ek, "*", cc, ") - ", kout, "*", R))
    } else {
      .eff <- str2lang(paste0("d/dt(", R, ") <- ", kin, " - ", kout, "*", R, "*(1+", ek, "*", cc, ")"))
    }
  } else {
    assertVariableNew(.ui, ik)
    if (inhib == "in") {
      .eff <- str2lang(paste0("d/dt(", R, ") <- ", kin, "*(1-", ik, "*", cc, ") - ", kout, "*", R))
    } else {
      .eff <- str2lang(paste0("d/dt(", R, ") <- ", kin, " - ", kout, "*", R, "*(1-", ik, "*", cc, ")"))
    }
  }
  .modelLines <- c(
    list(
      str2lang(paste0(kin, " <- exp(l", kin, ")")),
      str2lang(paste0(kout, " <- exp(l", kout, ")")),
      str2lang(ifelse(.doStim, paste0(ek, " <- exp(l", ek, ")"), paste0(ik, " <- exp(l", ik, ")")))
    ),
    .modelLines,
    list(str2lang(paste0(R, "(0) <- ", kin, "/", kout)), .eff, str2lang(paste0(effect, " <- ", R)))
  )
  .errLine <- str2lang(paste0(effect, " ~ add(", .effectSd, ")"))

  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- .iniAddTheta(.ui, paste0("l", kin), label = paste0("zero order response production(", kin, ")"))
  .ui <- .iniAddTheta(.ui, paste0("l", kout), label = paste0("first order rate response loss (", kout, ")"))
  if (.doStim) {
    .ui <- .iniAddTheta(.ui, paste0("l", ek), label = paste0("linear effect constant (", ek, ")"))
  } else {
    .ui <- .iniAddTheta(
      .ui,
      paste0("l", ik),
      lower = -Inf,
      upper = 1,
      label = paste0("linear inhibition constant (", ik, ")")
    )
  }
  # the endpoint goes on by itself so rxode2 creates the residual parameter
  # and decides its condition, err and lower bound
  .ui <- .modelAppend(.ui, list(.errLine))
  .iniAddTheta(.ui, .effectSd, label = paste0("additive error for ", effect))
}

#' Add an indirect response model to a PK model
#'
#' @inheritParams addIndirectLin
#' @inheritParams convertEmaxHill
#' @param hill Boolean stating if a hill sigmoid coefficient will be added
#' @param imax maximum inhibitory concentration
#' @param ic50 concentration where half of the Imax occurs
#' @return pk model with indirect response model added
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
#'   addIndirect(stim = "in", hill = TRUE)
#'
#' pk2cmtNoDepot |>
#'   addIndirect(inhib = "out", imax = 1)
addIndirect <- function(
  ui,
  stim = c("in", "out"),
  inhib = c("in", "out"),
  hill = FALSE,
  ek = "Ek",
  ik = "Ik",
  emax = "Emax",
  ec50 = "EC50",
  imax = "Imax",
  ic50 = "IC50",
  kin = "kin",
  kout = "kout",
  g = "g",
  cc = "Cc",
  R = "R",
  effect = "effect"
) {
  if (
    (missing(stim) && missing(inhib)) ||
      (!missing(stim) && !missing(inhib))
  ) {
    stop("need either 'stim' or 'inhib' specified", call. = FALSE)
  }
  .doStim <- FALSE
  if (!missing(stim)) {
    .doStim <- TRUE
    stim <- match.arg(stim)
  } else {
    inhib <- match.arg(inhib)
  }
  if (.doStim) {
    .mod1 <- addIndirectLin(ui, stim = stim, ek = ek, ik = ek, kin = kin, kout = kout, cc = cc, R = R, effect = effect)
    if (hill) {
      convertEmaxHill(.mod1, emax = emax, ec50 = ec50, g = g, ek = ek, cc = cc)
    } else {
      convertEmax(.mod1, emax = emax, ec50 = ec50, ek = ek, cc = cc)
    }
  } else {
    .mod1 <- addIndirectLin(
      ui,
      inhib = inhib,
      ek = ik,
      ik = ik,
      kin = kin,
      kout = kout,
      cc = cc,
      R = R,
      effect = effect
    )
    if (hill) {
      convertEmaxHill(.mod1, emax = imax, ec50 = ic50, g = g, ek = ik, cc = cc)
    } else {
      convertEmax(.mod1, emax = imax, ec50 = ic50, ek = ik, cc = cc)
    }
  }
}
