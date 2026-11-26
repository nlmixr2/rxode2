#' Convert double absorption to sequential delays
#'
#' Reparameterizes a double-absorption model so the second path's delay
#' starts after the first path's zero-order duration, matching Monolix's
#' "sequential" double-absorption models.  Only valid when the first
#' path absorbs zero-order; raises an error otherwise.
#'
#' The second delay is rewritten as one of the Monolix sequential
#' parameterizations, depending on how it is expressed and whether the
#' first path also has a lag time (`Tlag1`):
#' \itemize{
#'   \item lag time: `Tlag2 = Tk01` (or `Tlag2 = Tlag1 + Tk01`)
#'   \item no delay: a lag time is added and tied the same way
#'   \item transit chain: converted to `Mtt2` (see
#'     [convertTransitMtt()]), then `Mtt2 = Tk01 + diffMtt2` (or
#'     `Mtt2 = Tlag1 + Tk01 + diffMtt2`)
#' }
#'
#' @inheritParams addDepot
#' @param depot2 name of the second depot compartment
#' @param transit2 transit-compartment prefix of the second path
#' @param ktr2 transit rate variable of the second path
#' @param mtt2 mean transit time variable of the second path
#' @return a model with the second delay tied to the first duration
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
convertAbsSequential <- function(
  ui,
  central = "central",
  depot = "depot",
  depot2 = "depot2",
  transit2 = "transitB",
  ktr2 = "ktrB",
  mtt2 = "Mtt2"
) {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  central <- assertCompartmentExists(.ui, central)
  # sequential ordering is only defined for a zero-order first path,
  # which addZeroOrderAbs() expresses as a modeled duration on central
  # (the depot compartment itself is gone -- a first-order depot still
  # present means this precondition fails even when central happens
  # to carry some other duration, e.g. an IV infusion)
  .cp <- .ui$props$cmtProp
  .durCmt <- if (!is.null(.cp)) .cp$Compartment[.cp$Property == "dur"] else character(0)
  if (testCompartmentExists(.ui, depot) || !central %in% .durCmt) {
    stop(
      "sequential double absorption requires a zero-order first path ('",
      central,
      "' has no modeled duration)",
      call. = FALSE
    )
  }
  # the first path's delay: Tlag1 + Tk01 when it has a lag, Tk01 otherwise
  .durVar <- .absorptionFirstDelay(central, .ui$lstExpr)
  .baseExpr <- .absorptionFirstDelayExpr(.ui$lstExpr, central, .durVar)
  if (testCompartmentExists(.ui, paste0(transit2, "1"))) {
    return(.absorptionSequentialTransit(.ui, transit2, ktr2, mtt2, .baseExpr))
  }
  .absorptionSequentialLag(.ui, depot2, .baseExpr)
}

# the first path's lag variable, or NULL when it has no lag time
.absorptionFirstLag <- function(modelLines, central) {
  .pat <- sprintf("lag(%s) <- ", central)
  .w <- which(vapply(
    modelLines,
    function(l) grepl(.pat, deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (length(.w) == 0L) {
    return(NULL)
  }
  .rhs <- modelLines[[.w[1L]]][[3L]]
  if (!is.name(.rhs)) {
    stop("the first absorption path's lag time is not a plain variable", call. = FALSE)
  }
  as.character(.rhs)
}

# the variable holding the first path's modeled duration (from its dur() line)
.absorptionFirstDelay <- function(central, modelLines) {
  .durLine <- sprintf("dur(%s) <- ", central)
  .wd <- which(vapply(
    modelLines,
    function(l) grepl(.durLine, deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (length(.wd) == 0L) {
    stop("the first absorption path ('", central, "') has no modeled duration", call. = FALSE)
  }
  as.character(modelLines[[.wd[1L]]][[3L]])
}

# Tlag1 + Tk01 when the first path has a lag, Tk01 otherwise
.absorptionFirstDelayExpr <- function(modelLines, central, durVar) {
  .lag1 <- .absorptionFirstLag(modelLines, central)
  if (is.null(.lag1)) durVar else paste0(.lag1, " + ", durVar)
}

.absorptionSequentialLag <- function(ui, depot2, baseExpr) {
  .lagLine <- sprintf("lag(%s) <- ", depot2)
  .hasLag <- any(vapply(
    ui$lstExpr,
    function(l) grepl(.lagLine, deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (!.hasLag) {
    # the sequential model with no chosen delays adds the lag
    ui <- addLag(ui, depot2)
  }
  # the second-path lag variable is orphaned; its name follows
  # addLag()'s defaultCombine() convention (lagDepot2)
  .lagVar <- as.character(defaultCombine("lag", depot2))
  .absorptionSetLagExpr(ui, depot2, baseExpr, dropVar = .lagVar)
}

.absorptionSequentialTransit <- function(ui, transit2, ktr2, mtt2, baseExpr) {
  if (!testVariableExists(ui, mtt2)) {
    ui <- convertTransitMtt(ui, transit = transit2, ktr = ktr2, mtt = mtt2)
  }
  .absorptionSetMttExpr(
    ui,
    mtt2,
    paste0(baseExpr, " + diffMtt2"),
    increment = "diffMtt2",
    label = "Second mean transit time increment beyond the first absorption (diffMtt2)"
  )
}

.absorptionDropDescription <- function(ui) {
  if (exists("description", envir = ui$meta)) {
    rm("description", envir = ui$meta)
  }
}
