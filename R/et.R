#' @importFrom utils .DollarNames
#' @export
.DollarNames.rxEt <- function(x, pattern) {
  if (is.rxEt(x)) {
    .envProps <- c("randomType", "canResize", "IDs", "show", "ndose", "nobs")
    .methods <- c(
      "expand", "getSampling", "get.sampling", "getDosing", "get.dosing",
      "get.nobs", "get.obs.rec", "getEventTable", "get.EventTable",
      "copy", "importEventTable", "import.EventTable", "simulate",
      "clearDosing", "clear_dosing", "clear.dosing",
      "clearSampling", "clear_sampling", "clear.sampling",
      "addSampling", "add_sampling", "add.sampling",
      "addDosing", "add_dosing", "add.dosing",
      "get_units", "getUnits", "get.units", "units"
    )
    .dataCols <- rev(c("id", "low", "time", "high", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur"))
    return(grep(pattern, c(.envProps, .methods, .dataCols, "env"), value = TRUE))
  }
  grep(pattern, .Call(`_rxode2_etDollarNames`, x), value = TRUE)
}

.isRxEt <- function(obj) {
  inherits(obj, "rxEt")
}


.etAddCls <- function(x) {
  if (.isRxEt(x)) {
    .x <- x
    .cls <- class(x)
    class(.x) <- "data.frame"
    if (!is.null(.x[["evid"]])) {
      class(.x[["evid"]]) <- "rxEvid"
      .tmp <- .x[["rate"]]
      .cls2 <- class(.tmp)
      if (!inherits(.cls2, "rxRateDur")) {
        class(.tmp) <- c("rxRateDur", .cls2)
      }
      .x[["rate"]] <- .tmp
      .tmp <- .x[["dur"]]
      .cls2 <- class(.tmp)
      if (!inherits(.cls2, "rxRateDur")) {
        class(.tmp) <- c("rxRateDur", .cls2)
      }
      .x[["dur"]] <- .tmp
      class(.x) <- .cls
      return(.x)
    } else {
      return(x)
    }
  } else {
    return(x)
  }
}
#' Event Table Function
#'
#' @param ... Times or event tables.  They can also be one of the named arguments below.
#'
#' @param time Time is the time of the dose or the sampling times.
#'     This can also be unspecified and is determined by the object
#'     type (list or numeric/integer).
#'
#' @param amt Amount of the dose. If specified, this assumes a dosing
#'     record, instead of a sampling record.
#'
#' @param evid Event ID; This can be:
#'
#' | Numeric Value | Description |
#' |---------------|-------------|
#' | 0             | An observation. This can also be specified as `evid=obs` |
#' | 1             | A dose observation.  This can also be specified as `evid=dose` |
#' | 2             | A non-dose event. This can also be specified as `evid=other` |
#' | 3             | A reset event.  This can also be specified as `evid=reset`. |
#' | 4             |Dose and reset event.  This can also be specified as `evid=doseReset` or `evid=resetDose` |
#'
#' Note a reset event resets all the compartment values to zero and turns off all infusions.
#'
#' @param cmt Compartment name or number.  If a number, this is an
#'   integer starting at 1.  Negative compartments turn off a
#'   compartment. If the compartment is a name, the compartment name
#'   is changed to the correct state/compartment number before
#'   running the simulation.  For a compartment named "-cmt" the
#'   compartment is turned off.
#'
#'     Can also specify `cmt` as `dosing.to`,
#'     `dose.to`, `doseTo`, `dosingTo`, and
#'     `state`.
#'
#' @param ii When specifying a dose, this is the inter-dose interval
#'     for `ss`, `addl` and `until` options (described below).
#'
#' @param addl The number of additional doses at a inter-dose
#'     interval after one dose.
#'
#' @param ss Steady state flag;  It can be one of:
#'
#' | Value | Description |
#' |------------|-------------|
#' | 0 | This dose is not a steady state dose
#' | 1 | This dose is a steady state dose with the between/inter-dose interval of `ii` |
#' | 2 | Superposition steady state |
#'
#' When `ss=2` the steady state dose that uses the super-position
#' principle to allow more complex steady states, like 10 mg in the
#' morning and 20 mg at night, or dosing at 8 am 12 pm and 8 pm
#' instead of every 12 hours.  Since it uses the super positioning
#' principle, it only makes sense when you know the kinetics are
#' linear.
#'
#' All other values of `SS` are currently invalid.
#'
#' @param rate When positive, this is the rate of infusion.  Otherwise:
#'
#' | Value | Description |
#' |-------|--------------------------------|
#' | 0     |  No infusion is on this record |
#' | -1    | Modeled rate (in rxode2:`rate(cmt) =`); Can be `et(rate=model)`. |
#' |-2     | Modeled duration (in rxode2: `dur(cmt) =`); Can be`et(dur=model)` or `et(rate=dur)`. |
#'
#' When a modeled bioavailability is applied to positive rates
#' (`rate` > 0), the duration of infusion is changed. This is
#' because the data specify the rate and amount, the only think that
#' modeled bioavailability can affect is duration.
#'
#' If instead you want the modeled bioavailability to increase the
#' rate of infusion instead of the duration of infusion, specify the
#' `dur` instead or model the duration with `rate=2`.
#'
#' @param dur Duration of infusion.  When `amt` and `dur`
#'     are specified the rate is calculated from the two data items.
#'     When `dur` is specified instead of `rate`, the
#'     bioavailability changes will increase rate instead of
#'     duration.
#'
#' @param until This is the time until the dosing should end.  It can
#'     be an easier way to figure out how many additional doses are
#'     needed over your sampling period.
#'
#' @param id A integer vector of IDs to add or remove from the event
#'     table.  If the event table is identical for each ID, then you
#'     may expand it to include all the IDs in this vector.  All the
#'     negative IDs in this vector will be removed.
#'
#' @param amountUnits The units for the dosing records (`amt`)
#'
#' @param timeUnits The units for the time records (`time`)
#'
#' @param addSampling This is a boolean indicating if a sampling time
#'     should be added at the same time as a dosing time.  By default
#'     this is `FALSE`.
#'
#' @param x This is the first argument supplied to the event table.
#'     This is named to allow `et` to be used in a pipe-line
#'     with arbitrary objects.
#'
#' @inheritParams base::eval
#' @inheritParams base::seq
#' @return A new event table
#'
#' @template etExamples
#' @importFrom Rcpp evalCpp
#' @importFrom stats simulate end setNames start
#' @importFrom utils assignInMyNamespace
#' @importFrom methods is
#' @export
et <- function(x, ..., envir = parent.frame()) {
  UseMethod("et")
}

.pipelineRx <- NULL
.pipelineInits <- NULL
.pipelineEvents <- NULL
.pipelineParams <- NULL
.pipelineICov <- NULL
.pipelineKeep <- NULL
.pipelineThetaMat <- NULL
.pipelineOmega <- NULL
.pipelineIov <- NULL
.pipelineSigma <- NULL
.pipelineDfObs <- NULL
.pipelineDfSub <- NULL
.pipelineNSub <- NULL

.isNa1 <- function(x) {
  if (inherits(x, "logical") ||
        inherits(x, "numeric") ||
        inherits(x, "integer")) {
    if (length(x) == 1) {
      return(is.na(x))
    }
  }
  FALSE
}

.pipelineNStud <- NULL

#' Assign in the rxode2 pipeline
#'
#'
#' @param obj  Object to assign.  If NA return the value.
#' @return The pipeline object (invisibly)
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.pipeRx <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineRx))
  assignInMyNamespace(".pipelineRx", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeInits <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineInits))
  assignInMyNamespace(".pipelineInits", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeEvents <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineEvents))
  assignInMyNamespace(".pipelineEvents", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeParams <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineParams))
  assignInMyNamespace(".pipelineParams", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeKeep <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineKeep))
  assignInMyNamespace(".pipelineKeep", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeThetaMat <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineThetaMat))
  assignInMyNamespace(".pipelineThetaMat", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeOmega <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineOmega))
  assignInMyNamespace(".pipelineOmega", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeSigma <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineSigma))
  assignInMyNamespace(".pipelineSigma", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeDfObs <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineDfObs))
  assignInMyNamespace(".pipelineDfObs", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeDfSub <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineDfSub))
  assignInMyNamespace(".pipelineDfSub", obj)
  return(invisible(obj))
}

#' @rdname dot-pipeRx
#' @export
.pipeNSub <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineNSub))
  assignInMyNamespace(".pipelineNSub", obj)
  return(invisible(obj))
}


#' @rdname dot-pipeRx
#' @export
.pipeNStud <- function(obj) {
  if (.isNa1(obj)) return(invisible(.pipelineNStud))
  assignInMyNamespace(".pipelineNStud", obj)
  return(invisible(obj))
}

#' Clear/Set pipeline
#'
#' @param rx rxode2 object
#' @keywords internal
#' @return None, clears rxode2 pipeline
#' @export
.clearPipe <- function(rx = NULL, inits = NULL,
                       events = NULL, params = NULL,
                       iCov = NULL, keep = NULL,
                       thetaMat = NULL, omega = NULL,
                       sigma = NULL, dfObs = NULL,
                       dfSub = NULL, nSub = NULL,
                       nStud = NULL) {
  assignInMyNamespace(".pipelineRx", rx)
  assignInMyNamespace(".pipelineInits", inits)
  assignInMyNamespace(".pipelineEvents", events)
  assignInMyNamespace(".pipelineParams", params)
  assignInMyNamespace(".pipelineICov", iCov)
  assignInMyNamespace(".pipelineKeep", keep)
  assignInMyNamespace(".pipelineThetaMat", thetaMat)
  assignInMyNamespace(".pipelineOmega", omega)
  assignInMyNamespace(".pipelineSigma", sigma)
  assignInMyNamespace(".pipelineDfObs", dfObs)
  assignInMyNamespace(".pipelineDfSub", dfSub)
  assignInMyNamespace(".pipelineNSub", nSub)
  assignInMyNamespace(".pipelineNStud", nStud)
}

#' @rdname et
#' @export
et.rxode2 <- function(x, ..., envir = parent.frame()) {
  .clearPipe()
  assignInMyNamespace(".pipelineRx", x)
  do.call(et, c(list(...), list(envir = envir)), envir = envir)
}

#' @rdname et
#' @export
et.function <- et.rxode2

#' @rdname et
#' @export
et.rxUi <- et.rxode2

#' @rdname et
#' @export
et.rxSolve <- function(x, ..., envir = parent.frame()) {
  ## Need to extract:
  ## 1. rxode2 model
  assignInMyNamespace(".pipelineRx", x$.args.object)
  ## 2. rxode2 parameters
  assignInMyNamespace(".pipelineParams", x$.args.par0)
  assignInMyNamespace(".pipelineICov", x$.args$iCov)
  assignInMyNamespace(".pipelineKeep", x$.args$keep)
  ## 3. rxode2 inits
  assignInMyNamespace(".pipelineInits", x$.args.inits)
  ## 4. rxode2 thetaMat
  assignInMyNamespace(".pipelineThetaMat", x$.args$thetaMat)
  ## 5. rxode2 omega
  assignInMyNamespace(".pipelineOmega", x$.args$omega)
  ## 6. rxode2 sigma
  assignInMyNamespace(".pipelineSigma", x$.args$sigma)
  ## 7. rxode2 dfObs
  assignInMyNamespace(".pipelineDfObs", x$env$.args$dfObs)
  ## 8. rxode2 dfSub
  assignInMyNamespace(".pipelineDfSub", x$env$.args$dfSub)
  do.call(et, c(list(...), list(envir = envir)), envir = envir)
}

#' @rdname et
#' @export
et.rxParams <- function(x, ..., envir = parent.frame()) {
  ## Need to extract:
  ## 1. rxode2 model
  ## 2. rxode2 parameters
  if (!is.null(x$params)) assignInMyNamespace(".pipelineParams", x$params)
  if (!is.null(x$iCov)) assignInMyNamespace(".pipelineICov", x$iCov)
  if (!is.null(x$keep)) assignInMyNamespace(".pipelineKeep", x$keep)
  ## 3. rxode2 inits
  if (!is.null(x$inits)) assignInMyNamespace(".pipelineInits", x$inits)
  ## 4. rxode2 thetaMat
  if (!is.null(x$thetaMat)) assignInMyNamespace(".pipelineThetaMat", x$thetaMat)
  ## 5. rxode2 omega
  if (!is.null(x$omega)) assignInMyNamespace(".pipelineOmega", x$omega)
  ## 6. rxode2 sigma
  if (!is.null(x$sigma)) assignInMyNamespace(".pipelineSigma", x$sigma)
  ## 7. rxode2 dfObs
  if (!is.null(x$dfObs)) assignInMyNamespace(".pipelineDfObs", x$dfObs)
  ## 8. rxode2 dfSub
  if (!is.null(x$dfSub)) assignInMyNamespace(".pipelineDfSub", x$dfSub)
  if (!is.null(x$nSub)) assignInMyNamespace(".pipelineNSub", x$nSub)
  if (!is.null(x$nStud)) assignInMyNamespace(".pipelineNStud", x$nStud)

  do.call(et, c(list(...), list(envir = envir)), envir = envir)
}

#' @rdname et
#' @export
et.default <- function(x, ..., time = NULL, amt = NULL, evid = NULL, cmt = NULL,
                        ii = NULL, addl = NULL, ss = NULL, rate = NULL, dur = NULL,
                        until = NULL, id = NULL,
                        amountUnits = NULL, timeUnits = NULL, addSampling = NULL,
                        envir = parent.frame(),
                        by = NULL, length.out = NULL) {

  # ---- Capture missing status ----
  .xMissing <- missing(x)
  .timeMissing <- missing(time)
  .amtMissing <- missing(amt)
  .evidMissing <- missing(evid)
  .cmtMissing <- missing(cmt)
  .iiMissing <- missing(ii)
  .addlMissing <- missing(addl)
  .ssMissing <- missing(ss)
  .rateMissing <- missing(rate)
  .durMissing <- missing(dur)
  .untilMissing <- missing(until)
  .idMissing <- missing(id)
  .amountUnitsMissing <- missing(amountUnits)
  .timeUnitsMissing <- missing(timeUnits)
  .addSamplingMissing <- missing(addSampling)

  # ---- Determine base rxEt object ----
  .xIsRxEt <- !.xMissing && is.rxEt(x)
  if (.xIsRxEt) {
    .et <- x$copy()  # always copy: et() returns a new object, never mutates x
  } else {
    .et <- .newRxEt()
  }
  .envRef <- .rxEtEnv(.et)

  # ---- Conflicting alias checks ----
  .dotArgs <- list(...)
  id <- .etHandleAliases(.dotArgs, id, ii, amt, cmt, timeUnits, time, dur,
                         .idMissing, .iiMissing, .amtMissing, .cmtMissing,
                         .timeUnitsMissing, .timeMissing, .durMissing)

  # ---- seq helpers: by / length.out ----
  .res <- .etHandleSeq(by, length.out, .xIsRxEt, .envRef, x, ..., envir = envir, time = time, .et = .et,
                       .xMissing = .xMissing, .timeMissing = .timeMissing)
  if (.res$done) return(invisible(.rxEtSyncData(.res$et)))

  # ---- Positional args / data.frame import ----
  .res <- .etHandlePositional(x, ..., time = time, .xIsRxEt = .xIsRxEt, envir = envir, .envRef = .envRef, .et = .et,
                              .xMissing = .xMissing, .timeMissing = .timeMissing)
  if (.res$done) return(invisible(.rxEtSyncData(.res$et)))
  .posCmt  <- .res$posCmt
  .listObs <- .res$listObs
  time     <- .res$time

  # ---- Units ----
  .etHandleUnits(.envRef, amountUnits, timeUnits, .dotArgs, .amountUnitsMissing, .timeUnitsMissing)

  # ---- ID expansion ----
  .res <- .etHandleId(id, .envRef, .xIsRxEt, envir)
  .resolvedId  <- .res$resolvedId
  .targetIds   <- .res$targetIds
  .doResize    <- .res$doResize
  .addedIds    <- .res$addedIds
  .removedIds  <- .res$removedIds
  .existingIds <- .res$existingIds

  # ---- Resolve evid / cmt ----
  .evidExpr <- substitute(evid)
  .evidSym <- as.character(.evidExpr)
  .evidVal <- .etHandleEvid(.evidExpr, .evidSym, envir, .evidMissing)

  .cmtExpr <- substitute(cmt)
  .cmtSym <- as.character(.cmtExpr)
  .cmtVal  <- .etHandleCmt(.cmtExpr, .cmtSym, .posCmt, envir, .cmtMissing)

  # ---- Deferred list obs ----
  if (!is.null(.listObs)) {
    .dfObs <- .etObsChunk(.listObs, cmt = .cmtVal)
    .etAddChunk(.envRef, .dfObs, .targetIds)
    .envRef$nobs <- .envRef$nobs + length(.listObs) * max(1L, length(.targetIds))
    if (.amtMissing && is.null(amt)) return(invisible(.rxEtSyncData(.et)))
  }

  # ---- Dose record ----
  .amtExpr <- substitute(amt)
  .timeExpr <- substitute(time)
  .iiExpr <- substitute(ii)
  .addlExpr <- substitute(addl)
  .ssExpr <- substitute(ss)
  .rateExpr <- substitute(rate)
  .durExpr <- substitute(dur)
  .untilExpr <- substitute(until)
  .rateSym <- deparse(.rateExpr)

  .res <- .etHandleDose(amt, .amtExpr, .dotArgs, .envRef, envir, time, .timeExpr, .iiExpr, .addlExpr, .ssExpr, .rateExpr, .durExpr, .untilExpr,
                        .evidVal, .cmtVal, .resolvedId, .targetIds, addSampling, .et, .rateSym,
                        .amtMissing, .timeMissing, .iiMissing, .addlMissing,
                        .ssMissing, .rateMissing, .durMissing, .untilMissing,
                        .addSamplingMissing)
  if (.res$done) return(invisible(.rxEtSyncData(.res$et)))

  # ---- Infusion/SS dose without explicit amt ----
  .res <- .etHandleInfusionNoAmt(amt, .amtExpr, .rateExpr, .ssExpr, .envRef, envir, time, .timeExpr, .iiExpr, .durExpr, .evidVal, .cmtVal, .targetIds, .et, .rateSym,
                                 .amtMissing, .rateMissing, .ssMissing, .timeMissing, .iiMissing, .durMissing)
  if (.res$done) return(invisible(.rxEtSyncData(.res$et)))

  # ---- Observation record ----
  .res <- .etHandleObs(time, .timeExpr, envir, .envRef, .evidVal, .cmtVal, .targetIds, .et, .timeMissing)
  if (.res$done) return(invisible(.rxEtSyncData(.res$et)))

  # ---- Piping pattern ----
  .res <- .etHandlePiping(.xIsRxEt, x, time, .timeExpr, amt, .amtExpr, .dotArgs, .cmtVal, .targetIds, .evidVal, .envRef, .et, .timeMissing, .amtMissing, envir)
  if (.res$done) return(invisible(.rxEtSyncData(.res$et)))

  # ---- ID-only resize ----
  if (.doResize) {
    if (length(.addedIds) > 0L) {
      .tid <- if (length(.existingIds) > 0L) .existingIds[1L] else NA_integer_
      if (!is.na(.tid) && !is.null(.envRef$chunks[[.tid]])) {
        .template <- .envRef$chunks[[.tid]]
        for (.newId in .addedIds) {
          .newChunk <- .template
          .newChunk$id <- as.integer(.newId)
          .envRef$chunks[[.newId]] <- .newChunk
          .envRef$nobs  <- .envRef$nobs  + sum(.template$evid == 0L, na.rm = TRUE)
          .envRef$ndose <- .envRef$ndose + sum(.template$evid != 0L, na.rm = TRUE)
        }
      }
    }
    for (.rmId in .removedIds) {
      if (.rmId <= length(.envRef$chunks) && !is.null(.envRef$chunks[[.rmId]])) {
        .df <- .envRef$chunks[[.rmId]]
        .envRef$nobs  <- .envRef$nobs  - sum(.df$evid == 0L, na.rm = TRUE)
        .envRef$ndose <- .envRef$ndose - sum(.df$evid != 0L, na.rm = TRUE)
        .envRef$chunks[.rmId] <- list(NULL)
      }
    }
  }

  # ---- Pure-evid ----
  if (!is.null(.evidVal)) {
    .df <- .etObsChunk(0.0, cmt = .cmtVal)
    if (.evidVal != 0L) .df$evid <- as.integer(.evidVal)
    .etAddChunk(.envRef, .df, .targetIds)
    if (.evidVal == 0L) .envRef$nobs <- .envRef$nobs + max(1L, length(.targetIds))
    else .envRef$ndose <- .envRef$ndose + max(1L, length(.targetIds))
    return(invisible(.rxEtSyncData(.et)))
  }

  invisible(.rxEtSyncData(.et))
}

#' @export
`$.rxEt` <- function(obj, arg) {
  # 0. "env" is an alias for the mutable .env environment
  if (arg %in% c("env", ".env")) return(.rxEtEnv(obj))
  # 1. Check method closures in .env$methods (new-style) or direct list slots (internal mini-rxEt)
  .env <- .rxEtEnv(obj)
  if (is.environment(.env)) {
    .direct <- if (!is.null(.env$methods)) .env$methods[[arg]] else unclass(obj)[[arg]]
  } else {
    .direct <- unclass(obj)[[arg]]
  }
  if (!is.null(.direct)) return(.direct)

  # 2. Check mutable env properties (nobs, ndose, units, show, IDs, chunks)
  # "id" returns the unique sorted IDs present in the materialized table
  if (arg == "id" && !is.null(.env)) {
    .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
    return(sort(unique(as.integer(.mat$id))))
  }
  if (!is.null(.env) && exists(arg, envir = .env, inherits = FALSE)) {
    return(get(arg, envir = .env, inherits = FALSE))
  }

  # 3. Materialize and return data column (time, amt, evid, etc.)
  if (!is.null(.env)) {
    .mat <- .etMaterialize(obj)
    if (arg %in% names(.mat)) {
      .val <- .mat[[arg]]
      if (requireNamespace("units", quietly = TRUE)) {
        .tu <- .env$units["time"]
        .du <- .env$units["dosing"]
        .hasTimeU <- !is.na(.tu) && nchar(.tu) > 0
        .hasDoseU <- !is.na(.du) && nchar(.du) > 0
        if (arg == "time" && .hasTimeU) {
          return(units::set_units(.val, .tu, mode = "standard"))
        } else if (arg == "amt" && .hasDoseU) {
          return(units::set_units(.val, .du, mode = "standard"))
        } else if (arg == "ii" && .hasTimeU) {
          return(units::set_units(.val, .tu, mode = "standard"))
        } else if (arg == "rate" && .hasDoseU && .hasTimeU) {
          .rateU <- paste0(.du, "/", .tu)
          return(units::set_units(.val, .rateU, mode = "standard"))
        }
      }
      return(.val)
    }
    return(NULL)
  }

  # 4. Old-style C++ EventTable: delegate to C++ etUpdate handler
  .Call(`_rxode2_etUpdate`, obj, arg, NULL, TRUE)
}

#' @export
select.rxEt <- function(.data, ...) {
  .full <- as.data.frame(.data, all = TRUE)
  dplyr::select(.full, ...)
}

#' @export
filter.rxEt <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  .full <- as.data.frame(.data, all = TRUE)
  dplyr::filter(.full, ..., .by = {{ .by }}, .preserve = .preserve)
}

#' @export
rename.rxEt <- function(.data, ...) {
  .full <- as.data.frame(.data, all = TRUE)
  dplyr::rename(.full, ...)
}

#' @export
names.rxEt <- function(x) {
  names(as.data.frame(x, all = TRUE))
}

#' @export
`[.rxEt` <- function(x, i, j, drop = FALSE) {
  .env0 <- .rxEtEnv(x)
  if (missing(i) && missing(j)) return(x)
  if (missing(j)) {
    # Row-only subset: return rxEt
    .full <- .etMaterialize(x)
    .sub  <- if (missing(i)) .full else .full[i, , drop = FALSE]
    .newEnv <- new.env(parent = emptyenv())
    .newEnv$units      <- .env0$units
    .newEnv$show       <- .env0$show
    .newEnv$randomType <- .env0$randomType
    .newEnv$canResize  <- FALSE
    .newEnv$chunks     <- list()
    if (nrow(.sub) > 0L) {
      .newEnv$IDs  <- sort(unique(as.integer(.sub$id)))
      .newEnv$nobs  <- sum(.sub$evid == 0L, na.rm = TRUE)
      .newEnv$ndose <- sum(.sub$evid != 0L, na.rm = TRUE)
      for (.ii in .newEnv$IDs)
        .newEnv$chunks[[.ii]] <- .sub[.sub$id == .ii, , drop = FALSE]
    } else {
      .newEnv$IDs   <- 1L
      .newEnv$nobs  <- 0L
      .newEnv$ndose <- 0L
    }
    return(structure(c(list(.env = .newEnv), .etBuildMethods(.newEnv)), class = "rxEt"))
  }
  .mat <- as.data.frame(x, all = TRUE)
  if (missing(i)) return(.mat[, j, drop = drop])
  .mat[i, j, drop = drop]
}

#' @export
`$<-.rxEt` <- function(x, name, value) {
  .df <- .etMaterialize(x)
  .df[[name]] <- value
  .rxEtRebuildShell(x, .df)
}

#' @export
drop_units.rxEt <- function(x) {
  if (!requireNamespace("units", quietly = TRUE)) {
    stop("requires package 'units'", call. = FALSE)
  }
  if (is.rxEt(x)) {
    .env <- .rxEtEnv(x)
    .env$units["dosing"] <- NA_character_
    .env$units["time"]   <- NA_character_
    return(x)
  }
  .Call(`_rxode2_et_`, list(amountUnits = NA_character_, timeUnits = NA_character_), x)
}

#' @export
set_units.rxEt <- function(x, value, ..., mode = .setUnitsMode()) {
  if (is.null(mode)) {
    stop("requires package 'units'", call. = FALSE)
  }
  if (missing(value)) {
    value <- .unitless()
  } else if (mode == "symbols") {
    value <- substitute(value)
    if (is.numeric(value) && !identical(value, 1) && !identical(value, 1L)) {
      stop("the only valid number defining a unit is '1', signifying a unitless unit", call. = FALSE)
    }
  }
  if (identical(value, .unitless())) {
    warning("clearing both amount and time units\nfor more precise control use 'et(amountUnits=\"\")' or 'et(timeUnits=\"\")'",
      call. = FALSE
    )
    if (is.rxEt(x)) {
      .env <- .rxEtEnv(x)
      .env$units["dosing"] <- NA_character_
      .env$units["time"]   <- NA_character_
      return(x)
    }
    return(suppressWarnings({
      .Call(`_rxode2_et_`, list(amountUnits = "", timeUnits = ""), x)
    }))
  } else {
    if (!inherits(value, "character")) value <- deparse(value)
    .tUnit <- units::set_units(1, "sec", mode = "standard")
    .isTime <- try(units::set_units(units::set_units(1, value, mode = "standard"), "sec"), silent = TRUE)
    if (inherits(.isTime, "try-error")) {
      ## Amount
      if (is.rxEt(x)) {
        .env <- .rxEtEnv(x)
        .env$units["dosing"] <- value
        return(x)
      }
      return(.Call(`_rxode2_et_`, list(amountUnits = value), x))
    } else {
      ##
      if (is.rxEt(x)) {
        .env <- .rxEtEnv(x)
        .env$units["time"] <- value
        return(x)
      }
      return(.Call(`_rxode2_et_`, list(timeUnits = value), x))
    }
  }
}
#' Dispatch solve to 'rxode2' solve
#'
#'
#' @param x rxode2 solve dispatch object
#' @param ...  other arguments
#' @return if 'rxode2'  is loaded, a solved object, otherwise an error
#' @author Matthew L. Fidler
#' @export
rxEtDispatchSolve <- function(x, ...) {
  UseMethod("rxEtDispatchSolve")
}

#' @rdname rxEtDispatchSolve
#' @export
rxEtDispatchSolve.default <- function(x, ...) {
  stop("need 'rxode2' loaded for piping to a simulation")
}

#' @export
simulate.rxEt <- function(object, nsim = 1, seed = NULL, ...) {
    .isPipe <- as.character(substitute(object))
    if (length(.isPipe) == 1 && .isPipe == ".") {
      .isPipe <- TRUE
    } else if (missing(object)) {
      .isPipe <- FALSE
    } else {
      .isPipe <- substitute(object)
      if (is.call(.isPipe) && length(.isPipe) >= 1L) {
        # This will assume the input is going to be an et compatible object
        .isPipe <- TRUE
      } else {
        if (is.symbol(.isPipe)) {
          .isPipe <- TRUE
        } else {
          .isPipe <- FALSE
        }
      }
    }
    if (is.null(.pipelineRx) || !.isPipe) {
      if (!missing(nsim)) warning("'nsim' is ignored when simulating event tables", call. = FALSE)
      if (!is.null(seed)) set.seed(seed)
      if (is.rxEt(object)) {
        .env0 <- .rxEtEnv(object)
        .mat <- .etMaterialize(object)
        .hasWin <- !is.na(.mat$low) & !is.na(.mat$high)
        if (!any(.hasWin)) {
          warning("simulating event table without windows returns identical event table", call. = FALSE)
        } else {
          .rt <- .env0$randomType
          if (!is.na(.rt) && .rt == 3L) {
            .mat$time[.hasWin] <- stats::rnorm(sum(.hasWin), .mat$low[.hasWin], .mat$high[.hasWin])
          } else if (!is.na(.rt) && .rt == 2L) {
            .mat$time[.hasWin] <- stats::runif(sum(.hasWin), .mat$low[.hasWin], .mat$high[.hasWin])
          } else {
            warning("unclear windows, identical event table", call. = FALSE)
          }
        }
        .newEnv <- new.env(parent = emptyenv())
        .newEnv$units      <- .env0$units
        .newEnv$show       <- .env0$show
        .newEnv$IDs        <- .env0$IDs
        .newEnv$nobs       <- .env0$nobs
        .newEnv$ndose      <- .env0$ndose
        .newEnv$randomType <- NA_integer_
        .newEnv$canResize  <- FALSE
        .newEnv$chunks     <- list()
        if (nrow(.mat) > 0L) {
          .ids <- unique(as.integer(.mat$id))
          for (.i in .ids) .newEnv$chunks[[.i]] <- .mat[.mat$id == .i, , drop = FALSE]
        }
        return(structure(c(list(.env = .newEnv), .etBuildMethods(.newEnv)), class = "rxEt"))
      }
      return(.Call(`_rxode2_et_`, list(simulate = TRUE), object))
    } else {
      .ret <- list(object, ..., seed = seed, nsim = nsim)
      class(.ret) <- "rxode2et"
      return(rxEtDispatchSolve(.ret))
    }
}

#' Add dosing to eventTable
#'
#' This adds a dosing event to the event table.  This is provided for
#' piping syntax through magrittr.  It can also be accessed by `eventTable$add.dosing(...)`
#'
#' @param eventTable eventTable object; When accessed from object it would be `eventTable$`
#' @param dose numeric scalar, dose amount in `amount.units`;
#' @param nbr.doses integer, number of doses;
#' @param dosing.interval required numeric scalar, time between doses
#'     in `time.units`, defaults to 24 of
#'     `time.units="hours"`;
#' @param dosing.to integer, compartment the dose goes into (first
#'     compartment by default);
#' @param rate for infusions, the rate of infusion (default is
#'     `NULL`, for bolus dosing;
#' @param amount.units optional string indicating the dosing units.
#'     Defaults to `NA` to indicate as per the original
#'     `EventTable` definition.
#' @param start.time required dosing start time;
#' @param do.sampling logical, should observation sampling records be
#'     added at the dosing times? Defaults to `FALSE`.
#' @param time.units optional string indicating the time units.
#'     Defaults to `"hours"` to indicate as per the original
#'     `EventTable` definition.
#' @param ... Other parameters passed to [et()].
#' @return eventTable with updated dosing (note the event table will
#'     be updated anyway)
#' @author Matthew L. Fidler
#' @template etExamples
#' @export
add.dosing <- function(eventTable, dose, nbr.doses = 1L,
                       dosing.interval = 24, dosing.to = 1L,
                       rate = NULL, amount.units = NA_character_,
                       start.time = 0.0, do.sampling = FALSE,
                       time.units = NA_character_, ...) {
  if (is.rxEt(eventTable)) {
    .lst <- list(
      x = eventTable,
      amt = dose,
      time = start.time,
      ii = if (nbr.doses > 1L) dosing.interval else 0.0,
      addl = as.integer(nbr.doses) - 1L,
      addSampling = do.sampling
    )
    if (!is.null(rate)) .lst$rate <- rate
    if (!is.na(amount.units)) .lst$amountUnits <- amount.units
    if (!is.na(time.units)) .lst$timeUnits <- time.units
    .extra <- list(...)
    if (is.null(.extra$cmt) && is.null(.extra$dosing.to) &&
        !identical(dosing.to, 1L)) {
      .lst$cmt <- dosing.to
    }
    return(invisible(do.call(et, c(.lst, .extra))))
  }
  # Fallback for rxSolve objects and old-style EventTable via C++
  .lst <- list(
    dose = dose, nbr.doses = nbr.doses, start.time = start.time,
    do.sampling = do.sampling, ...
  )
  if (!is.na(amount.units)) .lst$amount.units <- amount.units
  if (!is.na(time.units))   .lst$time.units   <- time.units
  if (dosing.to != 1) .lst$dosing.to <- dosing.to
  if (!is.null(rate)) .lst$rate <- rate
  .lst$dosing.interval <- if (nbr.doses > 1L) dosing.interval else 0.0
  .Call(`_rxode2_et_`, .lst, eventTable)
}

#' Add sampling to eventTable
#'
#' This adds a dosing event to the event table.  This is provided for
#' piping syntax through magrittr.  It can also be accessed by
#' `eventTable$add.sampling()`
#'
#' @param eventTable An eventTable object. When accessed from object it would be `eventTable$`
#' @param time a vector of time values (in `time.units`).
#' @param time.units an optional string specifying the time
#'     units. Defaults to the units specified when the
#'     `EventTable` was initialized.
#' @return eventTable with updated sampling.  (Note the event table
#'     will be updated even if you don't reassign the eventTable)
#' @template etExamples
#' @export
add.sampling <- function(eventTable, time, time.units = NA_character_) {
  if (is.rxEt(eventTable)) {
    eventTable$add.sampling(time, time.units = time.units)
    return(invisible(.rxEtSyncData(eventTable)))
  }
  # Fallback for rxSolve objects and old-style EventTable via C++
  .lst <- list(time = time)
  if (!is.na(time.units)) .lst$time.units <- time.units
  .Call(`_rxode2_et_`, .lst, eventTable)
}


#' Create an event table object
#'
#' Initializes an object of class \sQuote{EventTable} with methods for
#' adding and querying dosing and observation records
#'
#' @param amount.units string denoting the amount dosing units, e.g.,
#'      \dQuote{mg}, \dQuote{ug}. Default to `NA` to denote
#'      unspecified units.  It could also be a solved rxode2 object.  In
#'      that case, eventTable(obj) returns the eventTable that was used
#'      to solve the rxode2 object.
#'
#' @param time.units string denoting the time units, e.g.,
#'      \dQuote{hours}, \dQuote{days}. Default to `"hours"`.
#'
#'  An `eventTable` is an object that consists of a data.frame
#'  storing ordered time-stamped events of an (unspecified) PK/PD
#'  dynamic system, units (strings) for dosing and time records, plus a
#'  list of functions to add and extract event records.
#'
#'  Currently, events can be of two types: dosing events that represent
#'  inputs to the system and sampling time events that represent
#'  observations of the system with \sQuote{amount.units} and
#'  \sQuote{time.units}, respectively.
#'
#'
#' @return A modified data.frame with the following accessible functions:
#'
#' * `get.EventTable()` returns the current event table
#'
#' * [add.dosing()]  adds dosing records to the event table.
#'
#' * `get.dosing()` returns a data.frame of dosing records.
#'
#' * `clear.dosing()` clears or deletes all dosing from event table
#'
#' *  `[add.sampling()] adds sampling time observation records to the
#'        event table.
#'
#' * `get.sampling()`returns a data.frame of sampled observation records.
#'
#' * `clear.sampling()` removes all sampling from event table.
#'
#' * `get.obs.rec()` returns a logical vector indicating whether each
#'    event record represents an observation or not.
#'
#' * `get.nobs()` returns the number of observation (not dosing) records.
#'
#' * `get.units()` returns a two-element character vector with the
#'        dosing and time units, respectively
#'
#' * `copy()` makes a copy of the current event table. To create
#'        a copy of an event table object use `qd2 <- qd$copy()`
#'
#' * `expand()` Expands the event table for multi-subject solving.
#'    This is done by `qd$expand(400)` for a 400 subject data expansion
#'
#' @author Matthew Fidler, Melissa Hallow and Wenping Wang
#'
#' @seealso [et()]
#'
#' @examples
#' # create dosing and observation (sampling) events
#' # QD 50mg dosing, 5 days followed by 25mg 5 days
#' #
#' qd <- eventTable(amount.units = "mg", time.units = "days")
#' #
#' qd$add.dosing(dose = 50, nbr.doses = 5, dosing.interval = 1, do.sampling = FALSE)
#' #
#' # sample the system's drug amounts hourly the first day, then every 12 hours
#' # for the next 4 days
#' qd$add.sampling(seq(from = 0, to = 1, by = 1 / 24))
#' qd$add.sampling(seq(from = 1, to = 5, by = 12 / 24))
#' #
#' # print(qd$get.dosing())     # table of dosing records
#' print(qd$get.nobs()) # number of observation (not dosing) records
#' #
#' # BID dosing, 5 days
#' bid <- eventTable("mg", "days") # only dosing
#' bid$add.dosing(
#'   dose = 10000, nbr.doses = 2 * 5,
#'   dosing.interval = 12, do.sampling = FALSE
#' )
#' #
#' # Use the copy() method to create a copy (clone) of an existing
#' # event table (simple assignments just create a new reference to
#' # the same event table object (closure)).
#' #
#' bid.ext <- bid$copy() # three-day extension for a 2nd cohort
#' bid.ext$add.dosing(
#'   dose = 5000, nbr.doses = 2 * 3,
#'   start.time = 120, dosing.interval = 12, do.sampling = FALSE
#' )
#'
#' # You can also use the Piping operator to create a table
#'
#' qd2 <- eventTable(amount.units = "mg", time.units = "days") |>
#'   add.dosing(dose = 50, nbr.doses = 5, dosing.interval = 1, do.sampling = FALSE) |>
#'   add.sampling(seq(from = 0, to = 1, by = 1 / 24)) |>
#'   add.sampling(seq(from = 1, to = 5, by = 12 / 24))
#' # print(qd2$get.dosing())     # table of dosing records
#' print(qd2$get.nobs()) # number of observation (not dosing) records
#'
#' # Note that piping with |> will update the original table.
#'
#' qd3 <- qd2 |> add.sampling(seq(from = 5, to = 10, by = 6 / 24))
#' print(qd2$get.nobs())
#' print(qd3$get.nobs())
#' @keywords models data
#' @concept ordinary differential equations
#' @concept Nonlinear regression
#' @concept Pharmacokinetics (PK)
#' @concept Pharmacodynamics (PD)
#' @export
eventTable <- function(amount.units = NA, time.units = NA) {
  .amtU <- if (!missing(amount.units)) as.character(amount.units) else NA_character_
  .timU <- if (!missing(time.units))   as.character(time.units)   else NA_character_
  .newRxEt(amountUnits = .amtU, timeUnits = .timU)
}
# nolint end

#' Sequence of event tables
#'
#' This combines a sequence of event tables.
#'
#' @param ... The event tables and optionally time between event
#'     tables, called waiting times in this help document.
#'
#' @param samples How to handle samples when repeating an event
#'     table.  The options are:
#'
#' * `"clear"` Clear sampling records before combining the datasets
#' * `"use"` Use the sampling records when combining the datasets
#'
#' @param waitII This determines how waiting times between events are
#'     handled. The options are:
#'
#' * `"smart"` This "smart" handling of waiting times is the
#'   default option.  In this case, if the waiting time is above the
#'   last observed inter-dose interval in the first combined event
#'   table, then the actual time between doses is given by the wait
#'   time.  If it is smaller than the last observed inter-dose
#'   interval, the time between event tables is given by the inter-dose
#'   interval + the waiting time between event tables.
#'
#' * `"+ii"` In this case, the wait time is added to the
#'    inter-dose interval no matter the length of the wait time or
#'    inter-dose interval
#'
#' @param ii If there was no inter-dose intervals found in the event
#'     table, assume that the interdose interval is given by this
#'     `ii` value.  By default this is `24`.
#'
#' @return An event table
#'
#' @details
#'
#' This `seq`uences all the event tables in added in the
#' argument list `...`.  By default when combining the event
#' tables the offset is at least by the last inter-dose interval in
#' the prior event table (or `ii`).  If you separate any of the
#' event tables by a number, the event tables will be separated at
#' least the wait time defined by that number or the last inter-dose
#' interval.
#'
#' @template etExamples
#'
#' @export
etSeq <- function(..., samples = c("clear", "use"),
                  waitII = c("smart", "+ii"), ii = 24) {
  .samples    <- match.arg(samples)
  .waitType   <- match.arg(waitII)
  .args       <- list(...)
  .explicitIi <- !missing(ii)

  .chunks    <- list()
  .nobs      <- 0L
  .ndose     <- 0L
  .units     <- NULL
  .show      <- NULL
  .IDs       <- integer(0)
  .timeDelta <- 0.0
  .lastIi    <- 0.0
  .lastDose  <- 0.0

  # Pre-scan: warn once if no event table has an inter-dose interval
  .etItems <- Filter(is.rxEt, .args)
  if (length(.etItems) > 1L) {
    .hasAnyIi <- any(vapply(.etItems, function(.a) {
      isTRUE(.rxEtEnv(.a)$show[["ii"]])
    }, logical(1)))
    if (!.hasAnyIi) {
      warning("No inter-dose interval found in event tables; using ii=", ii, call. = FALSE)
    }
  }

  for (.item in .args) {
    if (is.rxEt(.item)) {
      .env <- .rxEtEnv(.item)
      if (is.null(.units)) {
        .units <- .env$units
        .show  <- .env$show
      } else {
        .show <- .show | .env$show
      }
      .IDs <- sort(unique(c(.IDs, .env$IDs)))
      .mat <- .etMaterialize(.item)
      # Strip units for arithmetic; units preserved in .env$units
      if (requireNamespace("units", quietly = TRUE)) {
        for (.tmCol in c("time", "low", "high", "ii")) {
          if (!is.null(.mat[[.tmCol]]) && inherits(.mat[[.tmCol]], "units"))
            .mat[[.tmCol]] <- as.numeric(.mat[[.tmCol]])
        }
      }
      .mat$time <- .mat$time + .timeDelta
      if (!is.null(.mat$low)  && any(!is.na(.mat$low)))  .mat$low[!is.na(.mat$low)]   <- .mat$low[!is.na(.mat$low)]   + .timeDelta
      if (!is.null(.mat$high) && any(!is.na(.mat$high))) .mat$high[!is.na(.mat$high)] <- .mat$high[!is.na(.mat$high)] + .timeDelta
      if (!is.null(.mat$ii)) .mat$ii[is.na(.mat$ii)] <- 0.0
      .doseRows <- .mat[.mat$evid != 0L, , drop = FALSE]
      if (nrow(.doseRows) > 0L) {
        .lastDoseRow <- .doseRows[nrow(.doseRows), ]
        .addlVal <- if (is.null(.lastDoseRow$addl) || is.na(.lastDoseRow$addl)) 0L else as.integer(.lastDoseRow$addl)
        if (.addlVal > 0L) .lastIi <- .lastDoseRow$ii
        .lastDose <- .lastDoseRow$time + .addlVal * .lastIi
      }
      .maxTime <- max(.mat$time, na.rm = TRUE)
      if (.samples == "use") {
        .chunks <- .addRowsToChunks(.chunks, .mat)
        .nobs   <- .nobs + .env$nobs
      } else {
        .doseOnly <- .mat[.mat$evid != 0L, , drop = FALSE]
        .chunks <- .addRowsToChunks(.chunks, .doseOnly)
      }
      .ndose <- .ndose + .env$ndose
      # Advance past last dose period; also respect max obs time (for samples="use")
      if (.explicitIi && identical(ii, 0)) {
        .timeDelta <- .timeDelta
      } else {
        .effectiveIi <- if (.lastIi > 0) .lastIi else ii
        .timeDelta   <- max(.maxTime, .lastDose + .effectiveIi)
      }
    } else if (is.numeric(.item) || is.integer(.item)) {
      .wait        <- as.numeric(.item)
      .effectiveIi <- if (.lastIi > 0) .lastIi else ii
      if (.waitType == "+ii") {
        .timeDelta <- .lastDose + .effectiveIi + .wait
      } else {  # smart
        if (.wait < .effectiveIi) {
          .timeDelta <- .lastDose + .effectiveIi
        } else {
          .timeDelta <- .lastDose + .wait
        }
      }
    }
  }

  .newEnv <- new.env(parent = emptyenv())
  .newEnv$chunks     <- .chunks
  .newEnv$units      <- if (!is.null(.units)) .units else c(dosing = NA_character_, time = NA_character_)
  .newShow <- if (!is.null(.show)) .show else .etDefaultShow()
  if (.explicitIi || .ndose > 0L) {
    .newShow["ii"]   <- TRUE
    .newShow["addl"] <- TRUE
  }
  .newEnv$show       <- .newShow
  .newEnv$IDs        <- if (length(.IDs) > 0L) .IDs else 1L
  .newEnv$nobs       <- .nobs
  .newEnv$ndose      <- .ndose
  .newEnv$randomType <- NA_integer_
  .newEnv$canResize  <- FALSE
  if (length(.newEnv$IDs) > 1L) .newEnv$show["id"] <- TRUE
  structure(c(list(.env = .newEnv), .etBuildMethods(.newEnv)), class = "rxEt")
}
#' Combining event tables
#'
#' @inheritParams etSeq
#' @param id This is how rbind will handle IDs.  There are two different types of options:
#'
#' * `merge` with `id="merge"`, the IDs are merged together,
#' overlapping IDs would be merged into a single event table.
#'
#' * `unique` with `id="unique"`, the IDs will be renumbered
#' so that the IDs in all the event tables are not overlapping.
#'
#' @param
#' deparse.level The `deparse.level` of a traditional
#'     `rbind` is ignored.
#'
#' @author Matthew L Fidler
#'
#' @return An event table
#'
#' @template etExamples
#'
#' @export
etRbind <- function(..., samples = c("use", "clear"),
                    waitII = c("smart", "+ii"),
                    id = c("merge", "unique")) {
  .samples  <- match.arg(samples)
  .uniqueId <- match.arg(id) == "unique"
  .ets <- list(...)

  .chunks  <- list()
  .nobs    <- 0L
  .ndose   <- 0L
  .units   <- NULL
  .show    <- NULL
  .IDs     <- integer(0)
  .nextId  <- 0L

  for (.et in .ets) {
    if (!is.rxEt(.et)) next
    .env <- .rxEtEnv(.et)
    if (is.null(.units)) {
      .units <- .env$units
      .show  <- .env$show
    } else {
      .show <- .show | .env$show
    }
    # ID remapping for unique mode (always materializes)
    if (.uniqueId || .samples == "clear") {
      .mat    <- .etMaterialize(.et)
      if (.uniqueId) {
        .oldIds <- sort(unique(.mat$id))
        .map    <- seq_along(.oldIds) + .nextId
        .nextId <- .nextId + length(.oldIds)
        .mat$id <- .map[match(.mat$id, .oldIds)]
        .IDs    <- c(.IDs, .map)
      } else {
        .IDs <- sort(unique(c(.IDs, .env$IDs)))
      }
      if (.samples == "clear") {
        .mat <- .mat[.mat$evid != 0L, , drop = FALSE]
      }
      .chunks <- .addRowsToChunks(.chunks, .mat)
    } else {
      # Merge indexed chunks directly
      for (.ci in seq_along(.env$chunks)) {
        if (!is.null(.env$chunks[[.ci]])) {
          .existing <- if (.ci <= length(.chunks)) .chunks[[.ci]] else NULL
          .chunks[[.ci]] <- as.data.frame(
            data.table::rbindlist(list(.existing, .env$chunks[[.ci]]), fill = TRUE)
          )
        }
      }
      .IDs    <- sort(unique(c(.IDs, .env$IDs)))
    }
    if (.samples == "use") {
      .nobs  <- .nobs + .env$nobs
    }
    .ndose <- .ndose + .env$ndose
  }

  .newEnv <- new.env(parent = emptyenv())
  .newEnv$chunks     <- .chunks
  .newEnv$units      <- if (!is.null(.units)) .units else c(dosing = NA_character_, time = NA_character_)
  .newEnv$show       <- if (!is.null(.show)) .show else .etDefaultShow()
  .newEnv$IDs        <- if (length(.IDs) > 0L) .IDs else 1L
  .newEnv$nobs       <- .nobs
  .newEnv$ndose      <- .ndose
  .newEnv$randomType <- NA_integer_
  .newEnv$canResize  <- FALSE
  if (length(.newEnv$IDs) > 1L) .newEnv$show["id"] <- TRUE
  structure(c(list(.env = .newEnv), .etBuildMethods(.newEnv)), class = "rxEt")
}

#' @rdname etRbind
#' @export
rbind.rxEt <- function(..., deparse.level = 1) {
  if (!missing(deparse.level)) warning("'deparse.level' not used with rxode2 event tables", call. = FALSE)
  do.call(etRbind, list(...))
}

#' @rdname etSeq
#' @export
seq.rxEt <- function(...) {
  do.call(etSeq, list(...))
}

#' @export
c.rxEt <- function(...) {
  do.call(etSeq, list(...))
}

#' Repeat an rxode2 event table
#'
#' @param x An rxode2 event table
#' @param times Number of times to repeat the event table
#' @param length.out Invalid with rxode2 event tables, will throw an
#'     error if used.
#' @param each Invalid with rxode2 event tables, will throw an error
#'     if used.
#' @param n The number of times to repeat the event table.  Overrides
#'     `times`.
#' @param wait Waiting time between each repeated event table.  By
#'     default there is no waiting, or wait=0
#' @inheritParams et
#' @inheritParams etSeq
#' @template etExamples
#' @return An event table
#' @export
etRep <- function(x, times = 1, length.out = NA, each = NA, n = NULL, wait = 0, id = integer(0),
                  samples = c("clear", "use"),
                  waitII = c("smart", "+ii"), ii = 24) {
  if (!is.na(length.out)) stop("'length.out' makes no sense with event tables", call. = FALSE)
  if (!is.na(each))       stop("'each' makes no sense with event tables", call. = FALSE)
  if (!is.null(n)) times <- n
  if (is.rxEt(x)) {
    .xEnv <- .rxEtEnv(x)
    if (is.environment(.xEnv) && isFALSE(.xEnv$canResize)) {
      warning("event table has been expanded; rep may produce unexpected results", call. = FALSE)
    }
  }
  # Convert units wait to numeric in event table's time units
  if (inherits(wait, "units")) {
    if (!requireNamespace("units", quietly = TRUE)) {
      warning("'wait' has units but 'units' package not available; using numeric value", call. = FALSE)
      wait <- as.numeric(wait)
    } else {
      .timeU <- .rxEtEnv(x)$units["time"]
      if (!is.na(.timeU) && nchar(.timeU) > 0) {
        wait <- as.numeric(units::set_units(wait, .timeU, mode = "standard"))
      } else {
        warning("'wait' has units but event table has no time units; using numeric value", call. = FALSE)
        wait <- as.numeric(wait)
      }
    }
  }
  # Build alternating list: et, wait, et, wait, ...
  .lst <- vector("list", times * 2L)
  for (.i in seq_len(times)) {
    .lst[[.i * 2L - 1L]] <- x
    .lst[[.i * 2L]]      <- wait
  }
  do.call(etSeq, c(.lst, list(samples = samples, waitII = waitII, ii = ii)))
}

#' @rdname etRep
#' @export
rep.rxEt <- function(x, ...) {
  do.call(etRep, list(x = x, ...))
}
#' Coerce object to data.frame
#'
#' @param x Object to coerce to et.
#' @param ... Other parameters
#' @return An event table
#' @export
as.et <- function(x, ...) {
  UseMethod("as.et")
}
#' @rdname as.et
#' @export
as.et.default <- function(x, ...) {
  .e <- et()
  .e$importEventTable(as.data.frame(x))
  return(.e)
}

#' @export
as.data.frame.rxEt <- function(x, row.names = NULL, optional = FALSE, ...) {
  .env <- tryCatch(.rxEtEnv(x), error = function(e) NULL)
  .lst <- list(...)
  if (!is.environment(.env)) {
    # Old-style EventTable: strip rxEt class and delegate
    if (is.data.frame(x)) {
      .cls <- class(x)
      class(x) <- .cls[.cls != "rxEt"]
      return(as.data.frame(x, ...))
    }
    return(data.frame())
  }
  .show <- .env$show
  .full <- .etMaterialize(x)
  if (isTRUE(.lst$all)) {
    .full
  } else {
    .showCols <- names(.show)[.show]
    .showCols <- intersect(.showCols, names(.full))
    .full[, .showCols, drop = FALSE]
  }

}

.datatable.aware <- TRUE
#' Convert an event table to a data.table
#'
#' @inheritParams data.table::as.data.table
#'
#' @return data.table of event table
#'
#' @export
as.data.table.rxEt <- function(x, keep.rownames = FALSE, ...) {
  rxReq("data.table")
  data.table::as.data.table(as.data.frame.rxEt(x, ...), keep.rownames = keep.rownames)
}

#' Convert to tbl
#'
#' @param x rxode2 event table
#'
#' @param ... Other arguments to `as_tibble`
#'
#' @return tibble of event table
#'
#' @export
as_tibble.rxEt <- function(x, ...) {
  rxReq("tibble")
  tibble::as_tibble(as.data.frame.rxEt(x, ...), ...)
}


#' Expand additional doses
#'
#' @param et Event table to expand additional doses for.
#' @return New event table with `addl` doses expanded
#' @author Matthew Fidler
#' @examples
#' ev <- et(amt = 3, ii = 24, until = 240)
#' print(ev)
#' etExpand(ev) # expands event table, but doesn't modify it
#'
#' print(ev)
#'
#' ev$expand() ## Expands the current event table and saves it in ev
#' @export
etExpand <- function(et) {
  .mat      <- .etMaterialize(et)
  .expanded <- .etExpandAddl(.mat, .rxEtEnv(et))
  .env      <- .rxEtEnv(et)
  .newEnv <- new.env(parent = emptyenv())
  .newEnv$chunks <- list()
  if (nrow(.expanded) > 0L) {
    .ids <- unique(as.integer(.expanded$id))
    for (.i in .ids) {
      .newEnv$chunks[[.i]] <- .expanded[.expanded$id == .i, , drop = FALSE]
    }
  }
  .newEnv$units      <- .env$units
  .newEnv$show       <- .env$show
  .newEnv$IDs        <- .env$IDs
  .newEnv$nobs       <- sum(.expanded$evid == 0L)
  .newEnv$ndose      <- sum(.expanded$evid != 0L)
  .newEnv$randomType <- NA_integer_
  .newEnv$canResize  <- FALSE
  structure(c(list(.env = .newEnv), .etBuildMethods(.newEnv)), class = "rxEt")
}

#' EVID formatting for tibble and other places.
#'
#' This is to make an EVID more readable by non
#' pharmacometricians. It displays what each means and allows it to
#' be displayed in a tibble.
#'
#' @param x Item to be converted to a rxode2 EVID specification.
#'
#' @param ... Other parameters
#'
#' @return rxEvid specification
#'
#' @examples
#'
#' rxEvid(1:7)
#' @export
rxEvid <- function(x) {
  return(structure(x, class = "rxEvid"))
}

#' @rdname rxEvid
#' @export
as.rxEvid <- rxEvid

#' @rdname rxEvid
#' @export
c.rxEvid <- function(x, ...) {
  return(as.rxEvid(NextMethod()))
}

#' @rdname rxEvid
#' @export
`[.rxEvid` <- function(x, ...) {
  return(as.rxEvid(NextMethod()))
}
.colorFmt.rxEvid <- function(x, ...) {
  .x <- unclass(x)
  if (is.numeric(.x)) {
    .x <-
      data.table::fcase(
        .x == 0, paste0(crayon::blue$bold("0"), ":", crayon::white("Observation")),
        .x == 1, paste0(crayon::blue$bold("1"), ":", crayon::yellow("Dose (Add)")),
        .x == 2, paste0(crayon::blue$bold("2"), ":", crayon::yellow("Other")),
        .x == 3, paste0(crayon::blue$bold("3"), ":", crayon::red("Reset")),
        .x == 4, paste0(crayon::blue$bold("4"), ":", crayon::red("Reset"), "&", crayon::yellow("Dose")),
        .x == 5, paste0(crayon::blue$bold("5"), ":", crayon::red("Replace")),
        .x == 6, paste0(crayon::blue$bold("6"), ":", crayon::yellow("Multiply")),
        .x == 7, paste0(crayon::blue$bold("7"), ":", crayon::yellow("Transit")),
        default=paste0(crayon::blue$red(.x), ":", crayon::red("Invalid"))
      )
  } else {
    .x <- paste0(crayon::blue$red(.x), ":", crayon::red("Invalid"))
  }
  format(.x, justify = "left")
}

#' @rdname rxEvid
#' @export
as.character.rxEvid <- function(x, ...) {
  .x <- unclass(x)
  if (is.numeric(.x)) {
    .x <-
      data.table::fcase(
        .x == 0, "0:Observation",
        .x == 1, "1:Dose (Add)",
        .x == 2, "2:Other",
        .x == 3, "3:Reset",
        .x == 4, "4:Reset&Dose",
        .x == 5, "5:Replace",
        .x == 6, "6:Multiply",
        .x == 7, "7:Transit",
        default = paste0(.x, ":Invalid")
      )
  } else {
    .x <- paste0(.x, ":Invalid")
  }
  .x
}



#' @rdname rxEvid
#' @export
`[[.rxEvid` <- function(x, ...) {
  as.rxEvid(NextMethod())
}

#' @rdname rxEvid
#' @param value It will be an error to set units for evid
#' @export
`units<-.rxEvid` <- function(x, value) {
  stop("'evid' is unitless", call. = FALSE)
}

#' @export
`[<-.rxEvid` <- function(x, i, value) {
  as.rxEvid(NextMethod())
}

# registered in .onLoad()
type_sum.rxEvid <- function(x) {
  "evid"
}

# registered in .onLoad()
pillar_shaft.rxEvid <- function(x, ...) {
  .x <- .colorFmt.rxEvid(x)
  pillar::new_pillar_shaft_simple(.x)
}

#' @export
as.data.frame.rxEvid <- base::as.data.frame.difftime

#' Creates a rxRateDur object
#'
#' This is primarily to display information about rate
#'
#' @param x rxRateDur data
#' @param ... Other parameters
#'
#' @return rxRateDur object
#'
#' @export
rxRateDur <- function(x) {
  return(structure(x, class = "rxRateDur"))
}

#' @rdname rxRateDur
#' @export
`[.rxRateDur` <- function(x, ...) {
  return(as.rxRateDur(NextMethod()))
}

#' @rdname rxRateDur
#' @export
as.rxRateDur <- rxRateDur
#' @rdname rxEvid
#' @export
c.rxRateDur <- function(x, ...) {
  return(as.rxRateDur(NextMethod()))
}

#' @rdname rxRateDur
#' @export
as.character.rxRateDur <- function(x, ...) {
  .x <- unclass(x)
  .x <-
    ifelse(.x == -1, "-1:rate",
      ifelse(.x == -2, "-2:dur",
        ifelse(.x < 0, paste0(as.character(.x), ":Invalid"),
          sprintf(" %-8g", .x)
        )
      )
    )
  return(.x)
}

.fmt <- function(x, width = 9) {
  .g <- sprintf(paste0(" %-", width - 1, "g"), unclass(x))
  .f <- sprintf(paste0(" %-", width - 1, "f"), unclass(x))
  .ncg <- nchar(.g)
  .ncf <- nchar(.f)
  .ret <- ifelse(.ncg == width, .g,
    ifelse(.ncf == width, .f, .g)
  )
  return(.ret)
}


.colorFmt.rxRateDur <- function(x, ...) {
  .x <- unclass(x)
  .x <-
    ifelse(.x == -1, paste0(crayon::red("-1"), ":", crayon::yellow("rate")),
      ifelse(.x == -2, paste0(crayon::red("-2"), ":", crayon::yellow("dur")),
        ifelse(.x < 0, paste0(crayon::red(as.character(.x)), ":", crayon::red("Invalid")),
          .fmt(.x)
        )
      )
    )
  return(.x)
}

#' @rdname rxRateDur
#' @export
`[[.rxRateDur` <- function(x, ...) {
  as.rxRateDur(NextMethod())
}

#' @export
`[<-.rxRateDur` <- function(x, i, value) {
  as.rxRateDur(NextMethod())
}

# registered in .onLoad()
type_sum.rxRateDur <- function(x) {
  .unit <- attr(x, "units")
  if (!is.null(.unit)) {
    .tmp <- x
    class(.tmp) <- "units"
    return(pillar::type_sum(.tmp))
  } else {
    return("rate/dur")
  }
}

# registered in .onLoad()
pillar_shaft.rxRateDur <- function(x, ...) {
  .x <- .colorFmt.rxRateDur(x)
  pillar::new_pillar_shaft_simple(.x, align = "left", width = 10)
}

#' @export
as.data.frame.rxRateDur <- base::as.data.frame.difftime

set_units.rxRateDur <- function(x, value, ..., mode = .setUnitsMode()) {
  if (is.null(mode)) {
    stop("requires package 'units'", call. = FALSE)
  }
  if (inherits(x, "units")) {
    .ret <- x
    .ret0 <- unclass(x)
    .w1 <- which(.ret0 == -1)
    .w2 <- which(.ret0 == -2)
    .lst <- as.list(match.call())[-1]
    class(.ret0) <- "units"
    .lst[[1]] <- .ret0
    .ret <- do.call(units::set_units, .lst)
    if (length(.w1) > 0) .ret[.w1] <- -1
    if (length(.w2) > 0) .ret[.w2] <- -2
    class(.ret) <- c("rxRateDur", "units")
    return(.ret)
  } else {
    .lst <- as.list(match.call())[-1]
    .lst[[1]] <- unclass(x)
    .ret <- do.call(units::set_units, .lst)
    class(.ret) <- c("rxRateDur", "units")
    return(.ret)
  }
}
