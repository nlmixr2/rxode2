#' This asserts arguments that should not be used together and returns the resolved ID
#'
#' @param dotArgs The list of the ... arguments passed to the main
#'   et() function, used to check for deprecated or alias arguments
#'
#' @param id the id argument passed to et(), which may be modified if
#'   an uppercase "ID" is found in dotArgs
#'
#' @param ii the ii argument passed to et()
#'
#' @param amt the amt argument passed to et()
#'
#' @param timeUnits the timeUnits argument passed to et()
#'
#' @param time the time argument passed to et()
#'
#' @param dur the dur argument passed to et()
#'
#' @param idMissing a logical indicating whether the id argument was
#'   missing
#'
#' @param iiMissing a logical indicating whether the ii argument was
#'   missing
#'
#' @param amtMissing a logical indicating whether the amt argument was
#'   missing
#'
#' @param cmtMissing a logical indicating whether the cmt argument was
#'   missing
#'
#' @param timeUnitsMissing a logical indicating whether the timeUnits
#'   argument was missing
#'
#' @param timeMissing a logical indicating whether the time argument
#'   was missing
#'
#' @param durMissing a logical indicating whether the dur argument was
#'   missing
#'
#' @return the resolved id value after checking for aliases and conflicts
#' @noRd
.etAssertArgsAndReturnId <- function(dotArgs, id, ii, amt, cmt, timeUnits, time, dur,
                                     idMissing, iiMissing, amtMissing, cmtMissing,
                                     timeUnitsMissing, timeMissing, durMissing) {
  # Uppercase ID alias -> id
  if (!is.null(dotArgs[["ID"]]) && idMissing) {
    id <- dotArgs[["ID"]]
  }
  if (!is.null(dotArgs[["dosing.interval"]]) && !iiMissing)
    stop("cannot specify both 'ii' and 'dosing.interval'", call. = FALSE)
  if (!is.null(dotArgs[["dose"]]) && !amtMissing)
    stop("cannot specify both 'amt' and 'dose'", call. = FALSE)
  if (!is.null(dotArgs[["dosing.to"]]) && !cmtMissing)
    stop("cannot specify both 'cmt' and 'dosing.to'", call. = FALSE)
  if (!is.null(dotArgs[["dose.to"]]) && !cmtMissing)
    stop("cannot specify both 'cmt' and 'dose.to'", call. = FALSE)
  if (!is.null(dotArgs[["state"]]) && !cmtMissing)
    stop("cannot specify both 'cmt' and 'state'", call. = FALSE)
  if (!is.null(dotArgs[["amt.units"]]) && !is.null(dotArgs[["dose.units"]]))
    stop("cannot specify both 'amt.units' and 'dose.units'", call. = FALSE)
  if (!is.null(dotArgs[["time.units"]]) && !timeUnitsMissing)
    stop("cannot specify both 'time.units' and 'timeUnits'", call. = FALSE)
  if (!is.null(dotArgs[["start.time"]]) && !timeMissing)
    stop("cannot specify both 'time' and 'start.time'", call. = FALSE)
  if (!is.null(dotArgs[["nbr.doses"]]) && !is.null(dotArgs[["nbrDoses"]]))
    stop("cannot specify both 'nbr.doses' and 'nbrDoses'", call. = FALSE)
  if (!is.null(dotArgs[["duration"]]) && !durMissing)
    stop("cannot specify both 'dur' and 'duration'", call. = FALSE)
  id
}
#' This function handles the sequence type of arguments
#'
#' @param by the by argument passed to et(), used to specify the
#'   increment for a sequence of observation times.
#'
#' @param length.out the length.out argument passed to et(), used to specify the number
#'  of points in a sequence of observation times.
#'
#' @param xIsRxEt a logical indicating whether the x argument is an
#'   rxEt object, which affects how the from/to values for the
#'   sequence are resolved.
#'
#' @param envRef a reference to the internal environment of the rxEt
#'   object being constructed, used to access and modify the current
#'   state of the event table as new chunks are added.
#'
#' @param x the x argument passed to et(), which may be used to
#'   resolve the from/to values for the sequence if xIsRxEt is FALSE
#'
#' @param ... the ... arguments passed to et(), which may contain
#'   named or unnamed arguments used to resolve the from/to values for
#'   the sequence, such as from/to or the first two unnamed numeric
#'   arguments.
#'
#' @param envir the environment in which to evaluate any expressions needed to
#'  resolve the from/to values for the sequence.
#'
#' @param time the time argument passed to et(), which is used to
#'   check for conflicts with the sequence arguments.
#'
#' @param et the current state of the event table being constructed,
#'   which may be modified by adding a new chunk of observation times
#'   if the sequence arguments are successfully resolved.
#'
#' @param xMissing a logical indicating whether the x argument was
#'   missing, which affects how the from/to values for the sequence
#'   are resolved.
#'
#' @param timeMissing a logical indicating whether the time argument
#'   was missing, which is used to check for conflicts with the
#'   sequence arguments.
#'
#' @return a list with components done (a logical indicating whether
#'   the sequence was successfully handled and the main et() function
#'   should return immediately) and et (the possibly modified event
#'   table after handling the sequence arguments)
#'
#' @noRd
#'
#' @author Matthew L. Fidler
#'
.etHandleSeq <- function(by, length.out, xIsRxEt, envRef, x, ..., envir,
                         time, et, xMissing, timeMissing) {
  if (!is.null(by) && !is.null(length.out))
    stop("cannot specify both 'by' and 'length.out'", call. = FALSE)
  if (!is.null(by) || !is.null(length.out)) {
    if (xIsRxEt && length(envRef$ids) > 0L) {
      .seqTargetIds <- envRef$ids
    } else {
      .seqTargetIds <- NULL
    }
    .fromVal <- NULL
    .toVal <- NULL
    .seqDots <- list(...)

    if (!xMissing && !xIsRxEt) {
      .xVal <- x
    } else {
      .xVal <- NULL
    }
    .dotsNum <- Filter(function(.v) {
      is.numeric(.v) || is.integer(.v)
    }, .seqDots)

    # Also check named from/to in ...
    if (!is.null(.seqDots[["from"]])) {
      .fromVal <- .seqDots[["from"]]
    } else if (!is.null(.xVal) && (is.numeric(.xVal) || is.integer(.xVal))) {
      .fromVal <- .xVal
    } else if (xIsRxEt && length(.dotsNum) >= 1L) {
      .fromVal <- .dotsNum[[1L]]
    }

    if (!is.null(.seqDots[["to"]])) {
      .toVal <- .seqDots[["to"]]
    } else if (!xMissing && !xIsRxEt && length(.dotsNum) >= 1L) {
      .toVal <- .dotsNum[[1L]]
    } else if (xIsRxEt && length(.dotsNum) >= 2L) {
      .toVal <- .dotsNum[[2L]]
    }

    if (!is.null(.fromVal) &&
          (is.numeric(.fromVal) || is.integer(.fromVal))) {
      .from <- as.numeric(.fromVal)
      if (length(.from) != 1L) {
        stop("'from' must be scalar", call. = FALSE)
      }
      if (!is.null(.toVal)) {
        .to <- as.numeric(.toVal)
        if (length(.to) != 1L) {
          stop("'to' must be scalar", call. = FALSE)
        }
        if (!is.null(by)) {
          .resolvedTime <- seq(from = .from, to = .to, by = by)
        } else {
          .resolvedTime <- seq(from = .from, to = .to, length.out = length.out)
        }
      } else if (!is.null(by)) {
        .resolvedTime <- seq(from = .from, by = by)
      } else {
        .resolvedTime <- seq(from = .from, length.out = length.out)
      }
    } else if (!is.null(time)) {
      stop("'by'/'length.out' requires both a 'from' and 'to' value", call. = FALSE)
    } else {
      .resolvedTime <- numeric(0)
    }
    .df <- .etObsChunk(.resolvedTime)
    .etAddChunk(envRef, .df, .seqTargetIds)
    envRef$nobs <- envRef$nobs + length(.resolvedTime) * max(1L, length(.seqTargetIds))
    return(list(done = TRUE, et = et))
  }
  list(done = FALSE)
}
#' Handle the position arguments (ie ...)
#'
#'
#' @param x the x argument passed to et(), which may be used to
#'   resolve the from/to values for a sequence of observation times if
#'   xIsRxEt is FALSE
#'
#' @param ... the ... arguments passed to et(), to be resolved
#'
#' @param time the time argument passed to et()
#'
#' @param xIsRxEt a logical indicating whether the x argument is an
#'   rxEt object
#'
#' @param envir the environment in which to evaluate any expressions
#'   needed to
#'
#' @param envRef the reference to the internal environment of the rxEt
#'   object being constructed
#'
#' @param et the current state of the event table being constructed,
#'   which may be modified by adding a new chunk of observation times
#'   if the sequence arguments are successfully resolved.
#'
#' @param xMissing Is the x argument missing?
#'
#' @param timeMissing Is the time argument missing?
#'
#' @return list with components done (a logical indicating whether the
#'   position arguments were successfully handled and the main et()
#'   function should return immediately), posCmt (the resolved cmt
#'   value from the position arguments, if any), listObs (a list of
#'   observation times from the position arguments, if any), time (the
#'   resolved time value from the position arguments, if any), and et
#'   (the possibly modified event table after handling the position
#'   arguments)
#' @noRd
#' @author Matthew L. Fidler
.etHandlePositional <- function(x, ..., time, xIsRxEt, envir, envRef, et,
                                xMissing, timeMissing) {
  .posCmt <- NULL
  .listObs <- NULL
  if (is.null(time) && !xMissing && !xIsRxEt) {
    .xVal <- x
    .dots <- list(...)
    .dotsPos <- Filter(function(x) is.null(names(x)) || all(names(x) == ""), .dots)
    if (length(.dots) >= 2L) {
      # possibly sequence of event tables or waiting times
      if (any(vapply(.dots, is.rxEt, logical(1L))) || any(vapply(.dots, is.numeric, logical(1L)))) {
         # defer to etSeq handled in HandlePiping
         return(list(done = FALSE, posCmt = NULL, listObs = NULL, time = NULL))
      }
      stop("unused positional arguments", call. = FALSE)
    }
    if (length(.dots) == 1L && !is.null(names(.dots)) && any(names(.dots) != "")) {
       # Has named dots, let standard handler take it
    } else if (length(.dots) == 1 &&
                 !is.null(.xVal) &&
                 length(.xVal) == 1 &&
                 (is.numeric(.xVal) || is.integer(.xVal)) &&
                 (is.numeric(.dots[[1]]) || is.integer(.dots[[1]]))) {
      #et(1, 20) add obs seq(1, 20)
      .resolvedTime <- seq(from=as.numeric(.xVal), to=as.numeric(.dots[[1]]))
      .df <- .etObsChunk(.resolvedTime)
      .etAddChunk(envRef, .df, NULL)
      envRef$nobs <- envRef$nobs + length(.resolvedTime)
      return(list(done = TRUE, et = et))
    } else if (is.data.frame(.xVal)) {
      # Import data.frame as event table
      .df <- .xVal
      # Convert deSolve-style (var/value/method) to canonical rxEt format
      if (!is.null(.df$var) && !is.null(.df$value) && is.null(.df$amt) && is.null(.df$evid)) {
        .df$cmt   <- .df$var
        .df$var   <- NULL
        .df$amt   <- .df$value
        .df$value <- NULL
        if (!is.null(.df$method)) {
          .df$evid <- ifelse(.df$method == "rep", 5L,
                      ifelse(.df$method == "mult",
                             6L, 1L))
          .df$method <- NULL
        } else {
          .df$evid <- 1
        }
      }
      if (is.null(.df$evid)) {
        if (!is.null(.df$amt)) {
          .df$evid <- ifelse(!is.na(.df$amt) & as.numeric(.df$amt) != 0, 1L, 0L)
        } else {
          .df$evid <- 0L
        }
      }
      .df$evid <- as.integer(.df$evid)
      if (is.null(.df$id)) .df$id <- 1L
      .df$id <- as.integer(.df$id)
      .obsIdx <- .df$evid == 0L
      if (any(.obsIdx)) {
        if (!is.null(.df$rate)) {
          .df$rate[.obsIdx] <- NA_real_
        }
        if (!is.null(.df$amt))  {
          .df$amt[.obsIdx]  <- NA_real_
        }
      }
      envRef$ids    <- sort(unique(.df$id))
      envRef$nobs   <- envRef$nobs  + sum(.obsIdx)
      envRef$ndose  <- envRef$ndose + sum(!.obsIdx)
      if (length(envRef$ids) > 1L) {
        envRef$show["id"] <- TRUE
      }
      if (sum(!.obsIdx) > 0L) {
        envRef$show["amt"] <- TRUE
      }
      if (!is.null(.df$rate) &&
            any(.df$rate[!.obsIdx] != 0, na.rm = TRUE)) {
        envRef$show["rate"] <- TRUE
      }
      if (!is.null(.df$ii) && any(.df$ii != 0, na.rm = TRUE)) {
        envRef$show["ii"]   <- TRUE
        envRef$show["addl"] <- TRUE
      }
      envRef$chunks <- .addRowsToChunks(envRef$chunks, .df)
      return(list(done = TRUE, et = et))
    } else if (is.list(.xVal) &&
                 !is.data.frame(.xVal)) {
      # deferred: process after cmt/id resolved
      .listObs <- .xVal
    } else if (!is.null(.xVal)) {
      # single positional arg becomes time
      time <- .xVal
      # Second positional arg in ... treated as compartment (old API compat)
      .dots2 <- list(...)
      if (length(.dots2) == 1L && is.character(.dots2[[1]])) {
        # This only occurs et(1, "matt") but not with et(1, 2)
        .posCmt <- .dots2[[1]]
      }
    }
  }
  list(done = FALSE, posCmt = .posCmt, listObs = .listObs, time = time)
}
#' This function handles any units sent to the et() function.
#'
#'
#' @param envRef this is the reference to the internal environment of
#'   the rxEt object being constructed
#'
#' @param amountUnits the amountUnits argument passed to et()
#'
#' @param timeUnits the timeUnits argument passed to et()
#'
#' @param dotArgs The .. arguments passed to et(), which may contain
#'   named or unnamed arguments
#'
#' @param amountUnitsMissing a logical indicating whether the
#'   amountUnits argument was missing
#'
#' @param timeUnitsMissing a logical indicating whether the timeUnits
#'   argument was missing
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.etHandleUnits <- function(envRef, amountUnits, timeUnits, dotArgs,
                           amountUnitsMissing, timeUnitsMissing) {
  if (!is.null(amountUnits)) envRef$units["dosing"] <- amountUnits
  if (!is.null(timeUnits))   envRef$units["time"]   <- timeUnits
  # Handle dot-style and underscore aliases from ...
  if (!is.null(dotArgs[["amount.units"]]) &&
        is.null(amountUnits))
    envRef$units["dosing"] <- dotArgs[["amount.units"]]
  if (!is.null(dotArgs[["time.units"]]) &&
        is.null(timeUnits))
    envRef$units["time"] <- dotArgs[["time.units"]]
  if (!is.null(dotArgs[["time_units"]]) &&
        is.null(timeUnits) &&
        is.null(dotArgs[["time.units"]]))
    envRef$units["time"] <- dotArgs[["time_units"]]
  if (!is.null(dotArgs[["amount_units"]]) &&
        is.null(amountUnits) &&
        is.null(dotArgs[["amount.units"]]))
    envRef$units["dosing"] <- dotArgs[["amount_units"]]
}

.etHandleId <- function(id, .envRef, .xIsRxEt, envir) {
  .resolvedId  <- NULL
  .addedIds    <- integer(0)
  .removedIds  <- integer(0)
  .existingIds <- integer(0)
  .doResize    <- FALSE
  if (!is.null(id)) {
    .idVal       <- as.integer(id)
    .posIds      <- .idVal[.idVal > 0L]
    .negIds      <- abs(.idVal[.idVal < 0L])
    .existingIds <- .envRef$ids
    if (length(.posIds) > 0L && .xIsRxEt && .envRef$canResize) {
      # canResize mode: positive IDs define the exact target set, replacing existing
      .removedIds <- setdiff(.existingIds, .posIds)
      .addedIds   <- setdiff(.posIds, .existingIds)
      .envRef$ids <- sort(.posIds)
      .doResize   <- length(.addedIds) > 0L || length(.removedIds) > 0L
    } else {
      .addedIds   <- setdiff(.posIds, .existingIds)
      .removedIds <- intersect(.negIds, .existingIds)
      if (length(.posIds) > 0L) .envRef$ids <- sort(unique(c(.envRef$ids, .posIds)))
      if (length(.negIds) > 0L) .envRef$ids <- setdiff(.envRef$ids, .negIds)
      .doResize   <- .xIsRxEt && (length(.addedIds) > 0L || length(.removedIds) > 0L)
    }
    .envRef$show["id"] <- TRUE
    .resolvedId <- .posIds
  }
  .targetIds <- .resolvedId
  if (is.null(.targetIds) && .xIsRxEt && length(.envRef$ids) > 0L) {
    .targetIds <- .envRef$ids
  }
  list(resolvedId = .resolvedId, targetIds = .targetIds, doResize = .doResize,
       addedIds = .addedIds, removedIds = .removedIds, existingIds = .existingIds)
}

.etHandleEvid <- function(evidExpr, .evidSym, envir, .evidMissing) {
  .evidVal <- NULL
  if (!.evidMissing) {
    .evidVal <- switch(.evidSym,
      obs       = 0L,
      `0`       = 0L,
      dose      = 1L,
      `1`       = 1L,
      other     = 2L,
      `2`       = 2L,
      reset     = 3L,
      `3`       = 3L,
      doseReset = 4L,
      resetDose = 4L,
      `4`       = 4L,
      as.integer(tryCatch(eval(evidExpr, envir = envir),
                          error = function(e) as.integer(.evidSym)))
    )
  }
  .evidVal
}

.etHandleCmt <- function(cmtExpr, .cmtSym, .posCmt, envir, .cmtMissing) {
  .cmtVal <- .posCmt
  if (!.cmtMissing) {
    .cmtTry <- tryCatch(eval(cmtExpr, envir = envir), error = function(e) .cmtSym)
    .cmtVal <- if (is.character(.cmtTry) || is.numeric(.cmtTry)) .cmtTry else .cmtSym
  }
  .cmtVal
}

.etHandleDose <- function(amt, amtExpr, .dotArgs, .envRef, envir, time,
                          timeExpr, iiExpr, addlExpr, ssExpr, rateExpr,
                          durExpr, untilExpr, .evidVal, .cmtVal, .resolvedId,
                          .targetIds, addSampling, .et, .rateSym, .amtMissing,
                          .timeMissing, .iiMissing, .addlMissing, .ssMissing,
                          .rateMissing, .durMissing, .untilMissing,
                          .addSamplingMissing) {
  if (!is.null(amt) || !is.null(.dotArgs[["dose"]])) {
    .amtVal  <- if (!is.null(amt)) amt else .dotArgs[["dose"]]
    # Convert units amt to table's dosing units if applicable
    if (inherits(.amtVal, "units") && requireNamespace("units", quietly = TRUE)) {
      .doseU <- .envRef$units["dosing"]
      if (!is.na(.doseU) && nchar(.doseU) > 0) {
        .amtVal <- as.numeric(units::set_units(.amtVal, .doseU, mode = "standard"))
      } else {
        .amtVal <- as.numeric(.amtVal)
      }
    }
    .timeVal <- if (!is.null(time)) time else 0.0
    .iiVal <- if (!.iiMissing) {
      .iv <- eval(iiExpr, envir = envir)
      if (inherits(.iv, "units") && requireNamespace("units", quietly = TRUE)) {
        .tu <- .envRef$units["time"]
        if (!is.na(.tu) && nchar(.tu) > 0) as.numeric(units::set_units(.iv, .tu, mode = "standard"))
        else as.numeric(.iv)
      } else as.numeric(.iv)
    } else 0.0
    .addlVal <- if (!.addlMissing) as.integer(eval(addlExpr, envir = envir)) else 0L
    .ssVal   <- if (!.ssMissing)   as.integer(eval(ssExpr, envir = envir))   else 0L
    .rateVal <- if (!.rateMissing) {
      if (.rateSym == "model") -1.0
      else if (.rateSym == "dur") -2.0
      else {
        .rv <- eval(rateExpr, envir = envir)
        if (inherits(.rv, "units") && requireNamespace("units", quietly = TRUE)) {
          .du <- .envRef$units["dosing"]
          .tu <- .envRef$units["time"]
          if (!is.na(.du) && nchar(.du) > 0 && !is.na(.tu) && nchar(.tu) > 0) {
            as.numeric(units::set_units(.rv, paste0(.du, "/", .tu), mode = "standard"))
          } else as.numeric(.rv)
        } else as.numeric(.rv)
      }
    } else 0.0
    .durVal  <- if (!.durMissing)  as.numeric(eval(durExpr, envir = envir))  else 0.0
    .untilVal <- if (!.untilMissing) {
      .uv <- eval(untilExpr, envir = envir)
      if (inherits(.uv, "units") && requireNamespace("units", quietly = TRUE)) {
        .tu <- .envRef$units["time"]
        if (!is.na(.tu) && nchar(.tu) > 0) as.numeric(units::set_units(.uv, .tu, mode = "standard"))
        else as.numeric(.uv)
      } else as.numeric(.uv)
    } else NULL

    .df <- .etDoseChunk(
      time = .timeVal, amt = .amtVal,
      evid = if (!is.null(.evidVal)) .evidVal else 1L,
      cmt  = if (!is.null(.cmtVal))  .cmtVal  else "(default)",
      ii   = .iiVal, addl = .addlVal, ss = .ssVal,
      rate = .rateVal, dur = .durVal,
      until = .untilVal
    )
    if (!.durMissing && .rateMissing && any(.durVal > 0, na.rm = TRUE)) {
      .df$rate <- rep_len(0.0, nrow(.df))
      .df$dur <- rep_len(.durVal, nrow(.df))
    }
    .pairDoseIds <- !is.null(.resolvedId) &&
      nrow(.df) > 1L &&
      nrow(.df) == length(.resolvedId)
    if (.pairDoseIds) {
      .df$id <- as.integer(.resolvedId)
      .envRef$chunks <- .addRowsToChunks(.envRef$chunks, .df)
    } else {
      .etAddChunk(.envRef, .df, .targetIds)
    }
    .envRef$ndose  <- .envRef$ndose + max(1L, nrow(.df)) * if (.pairDoseIds) 1L else max(1L, length(.targetIds))
    .envRef$show["amt"] <- TRUE
    if (!is.null(.df$ii) && any(.df$ii > 0, na.rm = TRUE)) .envRef$show["ii"] <- TRUE
    if (!is.null(.df$addl) && any(.df$addl > 0L, na.rm = TRUE)) .envRef$show["addl"] <- TRUE
    if (!is.null(.ssVal) && .ssVal > 0L)   .envRef$show["ss"]   <- TRUE
    if (!is.null(.df$rate) && any(.df$rate != 0, na.rm = TRUE)) .envRef$show["rate"] <- TRUE
    if (!is.null(.df$dur) && any(.df$dur != 0, na.rm = TRUE)) .envRef$show["dur"] <- TRUE
    if (!is.null(.cmtVal) && .cmtVal != "(default)") .envRef$show["cmt"] <- TRUE

    if (!.addSamplingMissing && isTRUE(addSampling)) {
      .obsChunk <- .etObsChunk(.timeVal)
      .etAddChunk(.envRef, .obsChunk, .targetIds)
      .envRef$nobs   <- .envRef$nobs + length(.obsChunk$time) * max(1L, length(.targetIds))
    }
    return(list(done = TRUE, et = .et))
  }
  list(done = FALSE)
}

.etHandleInfusionNoAmt <- function(amt, amtExpr, rateExpr, ssExpr, .envRef, envir,
                                   time, timeExpr, iiExpr, durExpr, .evidVal,
                                   .cmtVal, .targetIds, .et, .rateSym,
                                   .amtMissing, .rateMissing, .ssMissing,
                                   .timeMissing, .iiMissing, .durMissing) {
  if (is.null(amt) && (!.rateMissing || !.ssMissing)) {
    .timeVal <- if (!is.null(time)) time else 0.0
    .iiVal   <- if (!.iiMissing)   as.numeric(eval(iiExpr, envir = envir))   else 0.0
    .ssVal   <- if (!.ssMissing)   as.integer(eval(ssExpr, envir = envir))   else 0L
    .rateVal <- if (!.rateMissing) {
      if (.rateSym == "model") -1.0
      else if (.rateSym == "dur") -2.0
      else as.numeric(eval(rateExpr, envir = envir))
    } else 0.0
    .durVal  <- if (!.durMissing) as.numeric(eval(durExpr, envir = envir)) else 0.0
    .df <- .etDoseChunk(
      time = .timeVal, amt = 0.0,
      evid = if (!is.null(.evidVal)) .evidVal else 1L,
      cmt  = if (!is.null(.cmtVal))  .cmtVal  else "(default)",
      ii = .iiVal, addl = 0L, ss = .ssVal,
      rate = .rateVal, dur = .durVal
    )
    .etAddChunk(.envRef, .df, .targetIds)
    .envRef$ndose  <- .envRef$ndose + max(1L, nrow(.df)) * max(1L, length(.targetIds))
    .envRef$show["amt"]  <- TRUE
    if (!is.null(.ssVal) && .ssVal > 0L)   .envRef$show["ss"]   <- TRUE
    if (!is.null(.df$rate) && any(.df$rate != 0, na.rm = TRUE)) .envRef$show["rate"] <- TRUE
    return(list(done = TRUE, et = .et))
  }
  list(done = FALSE)
}

.etHandleObs <- function(time, timeExpr, envir, .envRef, .evidVal, .cmtVal, .targetIds, .et, .timeMissing) {
  if (!is.null(time) || !.timeMissing) {
    .timeVal <- if (!is.null(time)) time else eval(timeExpr, envir = envir)
    if (!is.list(.timeVal) && inherits(.timeVal, "units") && requireNamespace("units", quietly = TRUE)) {
      .tu2 <- .envRef$units["time"]
      if (!is.na(.tu2) && nchar(.tu2) > 0) {
        .timeVal <- as.numeric(units::set_units(.timeVal, .tu2, mode = "standard"))
      } else {
        .timeVal <- as.numeric(.timeVal)
      }
    }
    .evid2 <- if (!is.null(.evidVal)) .evidVal else 0L
    .df <- .etObsChunk(.timeVal, cmt = .cmtVal)
    if (.evid2 != 0L) .df$evid <- as.integer(.evid2)
    .etAddChunk(.envRef, .df, .targetIds)
    .envRef$nobs <- .envRef$nobs + length(.df$time) * max(1L, length(.targetIds))
    if (!is.null(.cmtVal)) .envRef$show["cmt"] <- TRUE
    return(list(done = TRUE, et = .et))
  }
  list(done = FALSE)
}

.etHandlePiping <- function(.xIsRxEt, x, time, timeExpr, amt, amtExpr,
                            .dotArgs, .cmtVal, .targetIds, .evidVal,
                            .envRef, .et, .timeMissing, .amtMissing, envir) {
  if (.xIsRxEt && is.null(time) && is.null(amt) && .timeMissing && .amtMissing) {
    if (length(.dotArgs) >= 1L) {
      .firstDot <- .dotArgs[[1L]]
      if (is.rxEt(.firstDot) || any(vapply(.dotArgs, is.rxEt, logical(1L)))) {
        # Sequence of event tables
        .ret <- do.call(etSeq, c(list(x), .dotArgs))
        return(list(done = TRUE, et = .ret))
      }
      if (length(.dotArgs) > 2L) {
        stop("unused positional arguments", call. = FALSE)
      }
      if (is.list(.firstDot) && !is.data.frame(.firstDot)) {
        .df <- .etObsChunk(.firstDot, cmt = .cmtVal)
        .etAddChunk(.envRef, .df, .targetIds)
        .envRef$nobs <- .envRef$nobs + length(.df$time) * max(1L, length(.targetIds))
        if (!is.null(.cmtVal)) .envRef$show["cmt"] <- TRUE
        return(list(done = TRUE, et = .et))
      } else if (is.numeric(.firstDot) || is.integer(.firstDot)) {
        if (length(.dotArgs) >= 2L &&
            length(.firstDot) == 1L &&
            (is.numeric(.dotArgs[[2L]]) || is.integer(.dotArgs[[2L]])) &&
            length(.dotArgs[[2L]]) == 1L) {
          # et(0, 10) add obs at times 0, 1, ..., 10
          .timeVec <- seq(from = as.numeric(.firstDot), to = as.numeric(.dotArgs[[2L]]))
        } else if (inherits(.firstDot, "units") && requireNamespace("units", quietly = TRUE)) {
          .tu2 <- .envRef$units["time"]
          if (!is.na(.tu2) && nchar(.tu2) > 0) {
            .timeVec <- as.numeric(units::set_units(.firstDot, .tu2, mode = "standard"))
          } else {
            .timeVec <- as.numeric(.firstDot)
          }
        } else {
          .timeVec <- as.numeric(.firstDot)
        }
        .df <- .etObsChunk(.timeVec, cmt = .cmtVal)
        if (!is.null(.evidVal) && .evidVal != 0L) .df$evid <- as.integer(.evidVal)
        .etAddChunk(.envRef, .df, .targetIds)
        .envRef$nobs <- .envRef$nobs + length(.timeVec) * max(1L, length(.targetIds))
        return(list(done = TRUE, et = .et))
      }
    }
  }
  list(done = FALSE)
}

.etSeqHandleRxEt <- function(item, units, show, ids, timeDelta, samples, chunks, nobs, ndose, explicitIi, ii) {
  env <- .rxEtEnv(item)
  if (is.null(units)) {
    units <- env$units
    show  <- env$show
  } else {
    show <- show | env$show
  }
  ids <- sort(unique(c(ids, env$ids)))
  mat <- .etMaterialize(item)
  # Strip units for arithmetic; units preserved in env$units
  if (requireNamespace("units", quietly = TRUE)) {
    for (tmCol in c("time", "low", "high", "ii")) {
      if (!is.null(mat[[tmCol]]) && inherits(mat[[tmCol]], "units"))
        mat[[tmCol]] <- as.numeric(mat[[tmCol]])
    }
  }
  mat$time <- mat$time + timeDelta
  if (!is.null(mat$low)  && any(!is.na(mat$low)))
    mat$low[!is.na(mat$low)]   <- mat$low[!is.na(mat$low)]   + timeDelta
  if (!is.null(mat$high) && any(!is.na(mat$high)))
    mat$high[!is.na(mat$high)] <- mat$high[!is.na(mat$high)] + timeDelta
  if (!is.null(mat$ii)) mat$ii[is.na(mat$ii)] <- 0.0

  lastIi <- 0.0
  lastDose <- 0.0
  doseRows <- mat[mat$evid != 0L, , drop = FALSE]
  if (nrow(doseRows) > 0L) {
    lastDoseRow <- doseRows[nrow(doseRows), ]
    addlVal <- if (is.null(lastDoseRow$addl) || is.na(lastDoseRow$addl)) 0L else as.integer(lastDoseRow$addl)
    if (addlVal > 0L) lastIi <- lastDoseRow$ii
    lastDose <- lastDoseRow$time + addlVal * lastIi
  }
  maxTime <- max(mat$time, na.rm = TRUE)
  if (samples == "use") {
    chunks <- .addRowsToChunks(chunks, mat)
    nobs   <- nobs + env$nobs
  } else {
    doseOnly <- mat[mat$evid != 0L, , drop = FALSE]
    chunks <- .addRowsToChunks(chunks, doseOnly)
  }
  ndose <- ndose + env$ndose
  # Advance past last dose period; also respect max obs time (for samples=\"use\")
  if (explicitIi && identical(ii, 0)) {
    timeDelta <- timeDelta
  } else {
    effectiveIi <- if (lastIi > 0) lastIi else ii
    timeDelta   <- max(maxTime, lastDose + effectiveIi)
  }
  list(units = units, show = show, ids = ids, chunks = chunks,
       nobs = nobs, ndose = ndose, timeDelta = timeDelta,
       lastIi = lastIi, lastDose = lastDose)
}

.etSeqHandleWait <- function(item, waitType, lastDose, lastIi, ii, timeDelta) {
  wait        <- as.numeric(item)
  effectiveIi <- if (lastIi > 0) lastIi else ii
  if (waitType == "+ii") {
    timeDelta <- lastDose + effectiveIi + wait
  } else {  # smart
    if (wait < effectiveIi) {
      timeDelta <- lastDose + effectiveIi
    } else {
      timeDelta <- lastDose + wait
    }
  }
  timeDelta
}
