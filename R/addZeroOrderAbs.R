#' Does a variable have an assignment line in the model
#'
#' @param modelLines list of model lines
#' @param var variable name
#' @return logical indicating an assignment line exists
#' @noRd
.hasVarAssignmentLine <- function(modelLines, var) {
  .exprs <- list(str2lang(paste0(var, "<- .")), str2lang(paste0(var, "= .")))
  any(vapply(
    seq_along(modelLines),
    function(i) {
      .cur <- modelLines[[i]]
      any(vapply(
        .exprs,
        function(e) {
          .matchesLangTemplate(.cur, e)
        },
        logical(1),
        USE.NAMES = FALSE
      ))
    },
    logical(1),
    USE.NAMES = FALSE
  ))
}

#' Collect every symbol used by the model lines
#'
#' Walks the parse trees instead of reparsing the deparsed text, so
#' endpoint lines (\code{Cc ~ prop(propSd)}) are handled too.  Function
#' position names (rxode2 builtins and operators) are dropped; argument
#' position symbols are kept.
#'
#' @param modelLines list of model lines
#' @return character vector of unique symbol names
#' @noRd
.modelLineSymbols <- function(modelLines) {
  .collect <- function(e) {
    if (is.symbol(e)) {
      as.character(e)
    } else if (length(e) > 1) {
      unlist(lapply(as.list(e)[-1], .collect), use.names = FALSE)
    } else {
      character(0)
    }
  }
  unique(unlist(lapply(modelLines, .collect), use.names = FALSE))
}

#' Convert a model to zero-order absorption (Monolix \code{Tk0})
#'
#' Zero-order absorption releases the dose into the central compartment
#' at a constant rate over a modeled duration \code{Tk0}, matching
#' Monolix's \code{absorption(type=2, Tk0)} oral route.  Following
#' monolix2rx's translation, this is expressed as a modeled duration on
#' the central compartment (\code{dur(central) <- tk0}), so a depot
#' compartment (and its \code{ka}) is removed first.  Dosing records
#' for the zero-order route must request the modeled duration in the
#' event table (\code{RATE = -2}; see \code{et(rate=-2)}); ordinary
#' bolus records bypass the modeled duration.
#'
#' Lag time and bioavailability can be combined with [addLag()] and
#' [addBioavailability()] (`scale = "logit"`).
#'
#' @inheritParams addDepot
#' @inheritParams addTransit
#' @param tk0 zero-order absorption duration parameter name (Monolix's
#'   \code{Tk0})
#' @return a model with zero-order absorption
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#' pk1cmtDes <- function() {
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
#'     kel <- cl / vc
#'     d/dt(depot) <- -ka * depot
#'     d/dt(central) <- ka * depot - kel * central
#'     Cc <- central / vc
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
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
#' pk1cmtDes |> addZeroOrderAbs()
#'
#' # a model without a depot gets the same modeled duration input
#' pk2cmtNoDepot |> addZeroOrderAbs()
addZeroOrderAbs <- function(
  ui,
  central = "central",
  depot = "depot",
  transit = "transit",
  ktr = "ktr",
  ka = "ka",
  tk0 = "tk0"
) {
  .ui <- assertRxUi(ui)
  central <- assertCompartmentExists(.ui, central)
  assertCompartmentName(depot)
  assertVariableName(tk0)
  assertVariableNew(.ui, tk0)
  assertVariableNew(.ui, paste0("l", tk0))
  .cp <- .ui$props$cmtProp
  if (
    !is.null(.cp) &&
      any(.cp$Compartment == central & .cp$Property == "dur")
  ) {
    stop("modeled duration already present for compartment '", central, "'", call. = FALSE)
  }
  if (testCompartmentExists(.ui, paste0(transit, "1"))) {
    .ui <- removeTransit(.ui, central = central, depot = depot, transit = transit, ktr = ktr, ka = ka)
    warning("transit compartments removed for zero-order absorption model", call. = FALSE)
  }
  if (testCompartmentExists(.ui, depot)) {
    .ui <- removeDepot(.ui, central = central, depot = depot, ka = ka)
    warning("'", depot, "' removed for zero-order absorption model", call. = FALSE)
  }
  .ui <- addLogEstimates(.ui, stats::setNames("Zero-order absorption duration (Tk0)", tk0))
  .modelLines <- .ui$lstExpr
  .w <- .whichDdt(.modelLines, central)
  .tmp <- .extractModelLinesAtW(.modelLines, .w)
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- c(.tmp$pre, .tmp$w, str2lang(paste0("dur(", central, ") <- ", tk0)), .tmp$post)
  rxUiCompress(.ui)
}

#' Remove zero-order absorption from a model
#'
#' Removes the modeled duration on the central compartment, leaving the
#' dose as an intravenous bolus input.  The duration variable is taken
#' from the right hand side of the \code{dur(central)} line: when it is
#' defined by an assignment in the model (the \code{tk0 <- exp(ltk0)}
#' added by [addZeroOrderAbs()] or the \code{durCentral} added by
#' [addDur()]) the variable and its initial estimate are dropped too; a
#' bare estimated parameter that is not used elsewhere in the model is
#' dropped from the initial estimates.  First-order absorption can be
#' restored with [addDepot()].
#'
#' @inheritParams addDepot
#' @return a model where the zero-order absorption is removed
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#' pk1cmtDes <- function() {
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
#'     kel <- cl / vc
#'     d/dt(depot) <- -ka * depot
#'     d/dt(central) <- ka * depot - kel * central
#'     Cc <- central / vc
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
#'
#' pk1cmtDes |> addZeroOrderAbs() |> removeZeroOrderAbs()
removeZeroOrderAbs <- function(ui, central = "central") {
  .ui <- assertRxUi(ui)
  central <- assertCompartmentExists(.ui, central)
  .modelLines <- .ui$lstExpr
  .w <- .whichDdt(.modelLines, central, start = "dur(", end = ")")
  .rhs <- .modelLines[[.w]][[3]]
  .modelLines <- .modelLines[-.w]
  .var <- NULL
  .dropIni <- FALSE
  if (is.name(.rhs)) {
    .var <- as.character(.rhs)
    if (!.hasVarAssignmentLine(.modelLines, .var)) {
      # a bare parameter (monolix2rx style `dur(central) <- Tk0`); it
      # can only be dropped when nothing else in the model uses it
      if (.var %in% .modelLineSymbols(.modelLines)) {
        .var <- NULL
      } else {
        .dropIni <- TRUE
      }
    }
  }
  .ui <- rxUiDecompress(.ui)
  if (.dropIni) {
    .tmp <- .getEtaTheta(.ui)
    .ui$iniDf <- rbind(.dropTheta(.tmp$theta, .var), .tmp$eta)
  }
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- rxUiCompress(.ui)
  if (!is.null(.var) && !.dropIni) {
    .ui <- rxUiCompress(removeLinesAndInis(.ui, .var))
  }
  .ui
}
