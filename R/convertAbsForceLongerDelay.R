#' Force the second absorption delay longer than the first
#'
#' Reparameterizes a double-absorption model so the second path's delay
#' is estimated as an increment over the first path's delay, keeping the
#' second absorption later than the first.  Each path's delay may be a
#' lag time or a transit chain; a transit chain is reparameterized to
#' its mean transit time (see [convertTransitMtt()]) first.
#'
#' The resulting parameterization depends on the two delay types:
#' - lag and lag: `Tlag2 = Tlag1 + diffTlag2`
#' - lag and transit: `Mtt2 = Tlag1 + diffMtt2`
#' - transit and lag: `Tlag2 = Mtt1 + diffTlag2`
#' - transit and transit: `Mtt2 = Mtt1 + diffMtt2`
#'
#' @inheritParams addDepot
#' @param depot2 name of the second depot compartment
#' @param transit transit-compartment prefix of the first path
#' @param ktr transit rate variable of the first path
#' @param mtt mean transit time variable of the first path
#' @param transit2 transit-compartment prefix of the second path
#' @param ktr2 transit rate variable of the second path
#' @param mtt2 mean transit time variable of the second path
#' @return a model with the second delay tied above the first
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
convertAbsForceLongerDelay <- function(
  ui,
  central = "central",
  depot = "depot",
  depot2 = "depot2",
  transit = "transit",
  ktr = "ktr",
  mtt = "Mtt",
  transit2 = "transitB",
  ktr2 = "ktrB",
  mtt2 = "Mtt2"
) {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  central <- assertCompartmentExists(.ui, central)
  # a zero-order first path (no depot) keeps its delay on central; a
  # zero-order second path (no depot2) does the same
  .cmt1 <- if (testCompartmentExists(.ui, depot)) depot else central
  .cmt2 <- if (testCompartmentExists(.ui, depot2)) depot2 else central
  .d1 <- .absorptionDelay(.ui, .cmt1, transit, ktr, mtt)
  .d2 <- if (is.null(.d1)) NULL else .absorptionDelay(.d1$ui, .cmt2, transit2, ktr2, mtt2)
  if (is.null(.d1) || is.null(.d2)) {
    stop(
      "forcing a longer second delay requires a lag time or transit chain on both absorption paths",
      call. = FALSE
    )
  }
  .ui <- .d2$ui
  if (.d2$kind == "transit") {
    .expr <- paste0(.d1$var, " + diffMtt2")
    return(.absorptionSetMttExpr(
      .ui,
      mtt2,
      .expr,
      increment = "diffMtt2",
      label = "Second mean transit time increment over the first (diffMtt2)"
    ))
  }
  .ui <- .absorptionSetLagExpr(.ui, .cmt2, paste0(.d1$var, " + diffTlag2"), dropVar = .d2$var)
  .iniAddTheta(.ui, "diffTlag2", label = "Second lag increment over the first (diffTlag2)")
}

# the delay of one absorption path: a transit chain (returned in mean
# transit time form) takes precedence over a lag time
.absorptionDelay <- function(ui, cmt, transit, ktr, mtt) {
  if (testCompartmentExists(ui, paste0(transit, "1"))) {
    if (!testVariableExists(ui, mtt)) {
      ui <- convertTransitMtt(ui, transit = transit, ktr = ktr, mtt = mtt)
    }
    return(list(ui = ui, kind = "transit", var = mtt))
  }
  .pat <- sprintf("lag(%s) <- ", cmt)
  .w <- which(vapply(
    ui$lstExpr,
    function(l) grepl(.pat, deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (length(.w) == 0L) {
    return(NULL)
  }
  .rhs <- ui$lstExpr[[.w[1L]]][[3L]]
  if (!is.name(.rhs)) {
    stop("the absorption delay on '", cmt, "' is not a plain variable", call. = FALSE)
  }
  list(ui = ui, kind = "lag", var = as.character(.rhs))
}

# rewrite lag(cmt) to a new expression, dropping an orphaned delay
# variable (its exp() line and theta) when one is given
.absorptionSetLagExpr <- function(ui, cmt, expr, dropVar = NULL) {
  .modelLines <- ui$lstExpr
  .pat <- sprintf("lag(%s) <- ", cmt)
  .w <- which(vapply(
    .modelLines,
    function(l) grepl(.pat, deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (length(.w) == 0L) {
    stop("no lag time on compartment '", cmt, "'", call. = FALSE)
  }
  .modelLines[[.w[1L]]] <- str2lang(paste0("lag(", cmt, ") <- ", expr))
  .ui <- rxUiDecompress(ui)
  .absorptionDropDescription(.ui)
  model(.ui) <- .modelLines
  .ui <- rxUiCompress(.ui)
  if (!is.null(dropVar) && testVariableExists(.ui, dropVar)) {
    .ui <- rxUiCompress(removeLinesAndInis(.ui, dropVar))
  }
  .ui
}

# rewrite the mean transit time of a chain to a new expression, moving
# its theta to an optional increment parameter
.absorptionSetMttExpr <- function(ui, mttVar, expr, increment = NULL, label = NULL) {
  .modelLines <- ui$lstExpr
  .expMtt <- str2lang(paste0(mttVar, " <- exp(l", mttVar, ")"))
  .w <- which(vapply(
    .modelLines,
    function(l) .matchesLangTemplate(l, .expMtt),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (length(.w) != 1L) {
    stop("expected one '", mttVar, " <- exp(l", mttVar, ")' line", call. = FALSE)
  }
  .modelLines[[.w]] <- str2lang(paste0(mttVar, " <- ", expr))
  # the mean-transit-time theta moves to the increment, so drop it before
  # the model block is replaced (an ini parameter with no model use errors)
  .lmtt <- paste0("l", mttVar)
  .tmp <- .getEtaTheta(ui)
  .theta <- .dropTheta(.tmp$theta, .lmtt)
  .eta <- .dropEta(.tmp$eta, .lmtt)
  .ui <- rxUiDecompress(ui)
  .ui$iniDf <- rbind(.theta, .eta)
  .absorptionDropDescription(.ui)
  model(.ui) <- .modelLines
  .ui <- rxUiCompress(.ui)
  if (!is.null(increment)) {
    .ui <- .iniAddTheta(.ui, increment, label = label)
  }
  .ui
}
