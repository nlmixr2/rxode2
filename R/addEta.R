#' @title Get the left handed of the model based on defined initial conditions
#' @param model rxode2 model
#' @return The variable to left handed side of the model return
#' @noRd
#' @author Matthew L. Fidler
.getVarLhs <- function(model) {
  .ui <- assertRxUi(model)
  .varLhs <- .ui$varLhs
  # $getSplitMuModel is registered by nlmixr2est; only reached when it is loaded
  if (is.null(.varLhs)) {
    .varLhs <- .ui$getSplitMuModel$pureMuRef
  }
  .varLhs
}


#' Add random effects to a model
#'
#' @param ui The model as a function
#' @param eta vector with the parameters to add random effects
#'   (sometimes referred to as inter-individual variability, IIV) on
#' @param priorName logical, if TRUE, the parameter name specified in
#'   `eta` will be used to add the eta value prior name is used
#'   instead of the left handed side of the equation.  Defaults to
#'   `options(rxode2.priorEta=)` (or the legacy `nlmixr2lib.priorEta`).
#' @param etaCombineType the option for the how to combine the eta
#'   with the parameter name.  Can be: "default", "snake", "camel",
#'   "dot", "blank".  When missing it is taken from
#'   `options(rxode2.etaCombineType=)` (or the legacy
#'   `nlmixr2lib.etaCombineType`).
#' @param model Deprecated alias for \code{ui}. Supplying \code{model}
#'   instead of \code{ui} still works but emits a deprecation warning.
#' @return The model with eta added to the requested parameters
#' @author Bill Denney, Richard Hooijmaijers & Matthew L. Fidler
#' @family Parameter and naming building blocks
#' @export
#' @examples
#' pk1cmt <- function() {
#'   ini({
#'     lka <- 0.45; label("Absorption rate (Ka)")
#'     lcl <- 1; label("Clearance (CL)")
#'     lvc <- 3.45; label("Central volume of distribution (V)")
#'     propSd <- 0.5; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     ka <- exp(lka)
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     Cc <- linCmt()
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
#' library(rxode2)
#' pk1cmt |> addEta("ka")
#' @export
addEta <- function(
  ui,
  eta,
  priorName = getOption("rxode2.priorEta", getOption("nlmixr2lib.priorEta", TRUE)),
  etaCombineType = c("default", "snake", "camel", "dot", "blank"),
  model
) {
  .useModelAsUi()
  if (missing(etaCombineType)) {
    etaCombineType <- .getCombineTypeFromRoption(
      if (is.null(getOption("rxode2.etaCombineType"))) {
        "nlmixr2lib.etaCombineType"
      } else {
        "rxode2.etaCombineType"
      }
    )
  }
  if (etaCombineType != "default") {
    .combineEnv$old <- .combineEnv$default
    .combineEnv$default <- etaCombineType
    on.exit({
      .combineEnv$default <- .combineEnv$old
    })
  }
  checkmate::assertLogical(priorName, any.missing = FALSE)
  mod <- ui # save to apply everything later
  .eta <- as.character(substitute(eta))
  .eta2 <- try(force(eta))
  if (inherits(.eta2, "try-error")) {
    eta <- .eta
  }
  if (is.character(eta)) {
    # Assign a default value
    eta <- stats::setNames(rep(0.1, length(eta)), eta)
  }
  checkmate::assert_numeric(eta, lower = 0, null.ok = FALSE, min.len = 1)
  # Get the mu-referenced parameter names
  murefNames <- .getVarLhs(ui)
  etaMap <- character(0)
  for (currentEta in names(eta)) {
    etaName <- currentEta
    if (currentEta %in% names(murefNames)) {
      # do nothing
    } else if (currentEta %in% murefNames) {
      # Remap the parameter to the mu-referenced value for modification
      priorEta <- currentEta
      currentEta <- names(murefNames)[murefNames %in% currentEta]
      if (length(currentEta) > 1) {
        currentEta <- currentEta[1]
      }
      names(eta)[names(eta) == priorEta] <- currentEta
      cli::cli_alert(sprintf("Adding eta to %s instead of %s due to mu-referencing", currentEta, priorEta))
      if (priorName) {
        etaName <- priorEta
      } else {
        etaName <- currentEta
      }
    }
    etaName <- defaultCombine("eta", etaName)
    etaMap <- c(etaMap, stats::setNames(etaName, currentEta))
    ui <-
      searchReplace(
        object = ui,
        find = currentEta,
        replace = sprintf("%s + %s", currentEta, etaName)
      )
  }
  etaIni <- lapply(X = paste0(etaMap[names(eta)], "~", eta), FUN = base::str2lang)
  iniArgs <-
    append(
      list(ui),
      etaIni
    )
  # Work around rxode2 issue #277
  lotri <- lotri
  # Return the function itself or the updated ui
  fun <- do.call(ini, iniArgs)$fun
  rxode2(mod) <- body(fun)
  mod
}
