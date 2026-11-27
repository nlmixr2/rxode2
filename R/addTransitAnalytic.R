#' Add analytic transit absorption
#'
#' Adds a transit absorption delay evaluated with the `transit()`
#' pseudo-function instead of explicit compartments.  The delay is
#' parameterized by an estimated mean transit time `mtt` and transit
#' rate `Ktr`; the implied number of transit compartments is
#' `Mtt * Ktr - 1`.
#'
#' Dosing records stay on `depot`.  The model gains
#' `transit(Mtt * Ktr - 1, Mtt)`, which adds the amount that has
#' traversed the transit chain at each solve time, plus a
#' `transitDose(depot)` directive.  The directive makes `etTrans()`
#' translate bolus records on `depot` to phantom doses: the record's
#' amount drives `transit()` but is not added to the depot state, so
#' ordinary dosing records need no `evid = 7`.  `ka` then drains the
#' depot into `central` as usual.
#'
#' This is the analytic sibling of [addTransit()], which builds one ODE
#' state per transit compartment and estimates `ktr`; both forms are
#' supported.  [removeTransitAnalytic()] undoes this function.
#'
#' @inheritParams addDepot
#' @param transit the transit compartment prefix of an explicit chain,
#'   removed (with a warning) when present
#' @param ktr the transit rate variable of an explicit chain, used for
#'   that removal
#' @param mtt mean transit time variable name
#' @param Ktr transit rate variable name
#' @return a model with analytic transit absorption
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
#' pk1cmtDes |> addTransitAnalytic()
#'
addTransitAnalytic <- function(
  ui,
  central = "central",
  depot = "depot",
  ka = "ka",
  transit = "transit",
  ktr = "ktr",
  mtt = "Mtt",
  Ktr = "Ktr"
) {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  central <- assertCompartmentExists(.ui, central)
  assertCompartmentName(transit)
  assertVariableName(ktr)
  assertVariableName(mtt)
  assertVariableName(Ktr)
  if (testCompartmentExists(.ui, paste0(transit, "1"))) {
    .ui <- removeTransit(.ui, central = central, depot = depot, transit = transit, ktr = ktr, ka = ka)
    warning("transit compartments removed for analytic transit model", call. = FALSE)
  }
  if (!testCompartmentExists(.ui, depot)) {
    .ui <- addDepot(.ui, central = central, depot = depot, ka = ka)
    warning("'", depot, "' added to model for transit model", call. = FALSE)
  }
  .hasSplit <- any(vapply(
    .ui$lstExpr,
    function(l) grepl("split", deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (.hasSplit) {
    stop("a dose-splitting model cannot take an analytic transit input", call. = FALSE)
  }
  assertVariableNew(.ui, mtt)
  assertVariableNew(.ui, paste0("l", mtt))
  assertVariableNew(.ui, Ktr)
  assertVariableNew(.ui, paste0("l", Ktr))
  .ui <- addLogEstimates(.ui, stats::setNames("Mean transit time (Mtt)", mtt))
  .ui <- addLogEstimates(.ui, stats::setNames("Transit rate (Ktr)", Ktr))
  .modelLines <- .ui$lstExpr
  .w <- .whichDdt(.modelLines, depot)
  .rhs <- .modelLines[[.w]][[3L]]
  .transit <- str2lang(paste0("transit(", mtt, " * ", Ktr, " - 1, ", mtt, ")"))
  .modelLines[[.w]][[3L]] <- .absorptionAddTerm(.transit, .rhs)
  .modelLines <- c(
    .modelLines[seq_len(.w)],
    list(str2lang(paste0("transitDose(", depot, ")"))),
    .modelLines[-seq_len(.w)]
  )
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  rxUiCompress(.ui)
}

#' Remove analytic transit absorption
#'
#' Removes the `transit()` input and the `transitDose(depot)` directive
#' added by [addTransitAnalytic()], and drops the `mtt`/`Ktr`
#' parameters.  Use [removeTransit()] for an explicit transit chain
#' instead.
#'
#' @inheritParams addTransitAnalytic
#' @inheritParams addTransit
#' @return a model without the analytic transit input
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
removeTransitAnalytic <- function(ui, depot = "depot", mtt = "Mtt", Ktr = "Ktr", model) {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  .modelLines <- .ui$lstExpr
  .w <- .whichDdt(.modelLines, depot)
  .rhs <- .modelLines[[.w]][[3L]]
  .stripped <- .stripTransitInput(.rhs)
  if (identical(.stripped, .rhs)) {
    stop("no transit() input on '", depot, "'", call. = FALSE)
  }
  .modelLines[[.w]][[3L]] <- .stripped
  .tLine <- paste0("transitDose(", depot, ")")
  .modelLines <- Filter(function(l) deparse1(l) != .tLine, .modelLines)
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- rxUiCompress(.ui)
  removeLinesAndInis(.ui, c(mtt, Ktr))
}

# transit(...) + (-ka * depot) reads better as transit(...) - ka * depot
.absorptionAddTerm <- function(term, rhs) {
  if (is.call(rhs) && length(rhs) == 2L && identical(rhs[[1L]], as.name("-"))) {
    return(as.call(list(as.name("-"), term, rhs[[2L]])))
  }
  as.call(list(as.name("+"), term, rhs))
}

# drop transit() calls from an additive expression, keeping the signs of
# the remaining terms
.stripTransitInput <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }
  .op <- as.character(expr[[1L]])
  if (identical(.op, "transit")) {
    return(NULL)
  }
  if (.op %in% c("+", "-") && length(expr) == 3L) {
    .l <- .stripTransitInput(expr[[2L]])
    .r <- .stripTransitInput(expr[[3L]])
    if (is.null(.l) && is.null(.r)) {
      return(NULL)
    }
    if (is.null(.l)) {
      if (.op == "-") {
        return(as.call(list(as.name("-"), .r)))
      }
      return(.r)
    }
    if (is.null(.r)) {
      return(.l)
    }
    return(as.call(list(expr[[1L]], .l, .r)))
  }
  expr
}
