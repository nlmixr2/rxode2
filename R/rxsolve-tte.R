# tte() simulation solve preparation: checks on the model and event table
# and the C configuration (see src/tte.h).

#' Does this ui have a tte() endpoint?
#'
#' @param ui rxode2 ui (or anything else)
#' @return logical
#' @noRd
.rxTteHas <- function(ui) {
  if (!inherits(ui, "rxUi")) {
    return(FALSE)
  }
  .predDf <- ui$predDf
  !is.null(.predDf) && any(.predDf$distribution == "tte")
}

#' Refuse a delay() of a cumulative hazard with steady-state doses
#'
#' The hazard never reaches a steady state, so the history a steady-state
#' dose records for `delay(rx.tteH.<ev>)` has no meaning.
#'
#' @param model the simulation model
#' @param events event table
#' @return nothing, called for the error
#' @author Matthew L. Fidler
#' @noRd
.rxTteAssertSsDelay <- function(model, events) {
  if (!grepl("delay(rx.tteH.", rxModelVars(model)$model["normModel"], fixed = TRUE)) {
    return(invisible())
  }
  if (rxIs(events, "rx.event")) events <- as.data.frame(events)
  if (!is.data.frame(events)) return(invisible())
  .col <- function(name, default) {
    .c <- .rxTteCol(events, name)
    if (is.null(.c)) rep(default, nrow(events)) else events[[.c]]
  }
  .s <- .col("ss", 0)
  .ii <- .col("ii", 0)
  .amt <- .col("amt", 0)
  .evid <- .col("evid", 0)
  # the steady-state rule of _rxDeriveFlg(), plus classic steady-state evids
  .isSs <- (.s %in% c(1, 2) & .ii > 0) | (.s == 1 & .ii == 0 & .amt == 0) |
    (.evid >= 100 & .evid %% 100 %in% c(9, 10, 19, 20, 40))
  if (any(.isSs, na.rm = TRUE)) {
    stop("a delay() of a tte() cumulative hazard cannot be used with steady-state doses: the hazard has no steady state",
         call. = FALSE)
  }
  invisible()
}

#' Prepare a ui solve with tte() endpoints
#'
#' @param ui rxode2 ui
#' @param lst the combined `rxSolve.default()` argument list
#' @return the updated argument list; the C configuration is set as a side
#'   effect
#' @author Matthew L. Fidler
#' @noRd
.rxTtePrepareSolve <- function(ui, lst) {
  .predDf <- ui$predDf
  .w <- which(.predDf$distribution == "tte")
  .state <- rxModelVars(lst[[1]])$state
  .hIdx <- match(.rxTteStateName(.predDf$var[.w]), .state) - 1L
  .opts <- lapply(.w, function(.i) .rxTteOptions(ui, .predDf[.i, ]))
  .m <- as.integer(lst$method)
  if (length(.m) == 1L && !is.na(.m) && (.m >= 200L || .m == 4L)) {
    stop("simulating a tte() endpoint is not supported with the adjoint sensitivity methods", call. = FALSE)
  }
  .events <- lst$events
  .rxTteAssertSsDelay(lst[[1]], if (is.null(.events)) lst$params else .events)
  if (is.null(.events) && (rxIs(lst$params, "rx.event") || is.data.frame(lst$params))) {
    lst$params <- .rxTteEvents(ui, lst$params)
  } else {
    lst$events <- .rxTteEvents(ui, .events)
  }
  .Call(
    `_rxode2_rxTteSetup`,
    list(
      as.integer(.hIdx),
      vapply(.opts, function(.o) as.double(.o$maxEventNumber), double(1)),
      vapply(.opts, function(.o) as.integer(.o$eventType == "intervalCensored"), integer(1)),
      vapply(.opts, function(.o) as.integer(.o$dropout), integer(1)),
      as.integer(stats::runif(1, 0, .Machine$integer.max))
    )
  )
  lst
}
