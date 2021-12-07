.etAddCls <- function(x) {
  if (inherits(x, "rxEt")) {
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
#' @inheritParams rxSolve
#' @return A new event table
#'
#' @template etExamples
#'
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
.pipelineNStud <- NULL

#' Clear/Set pipeline
#'
#' @inheritParams rxControl
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
et.default <- function(x, ..., time, amt, evid, cmt, ii, addl,
                       ss, rate, dur, until, id,
                       amountUnits, timeUnits, addSampling,
                       envir = parent.frame(),
                       by = NULL, length.out = NULL) {
  .lst <- as.list(match.call()[-1])

  .isPipe <- as.character(substitute(x))
  if (length(.isPipe) == 1) {
    .isPipe <- (.isPipe == ".")
  } else {
    .isPipe <- FALSE
  }
  if (!missing(x)) {
    names(.lst)[1] <- ""
  }
  if (!missing(by)) {
    force(by)
    checkmate::assertNumeric(by, finite = TRUE, max.len = 1, any.missing = FALSE, min = 0)
    if (!missing(length.out)) {
      stop("cannot supply both 'by' and 'length.out'", call. = FALSE)
    }
    .lst <- .lst[names(.lst) != "by"]
    .lst <- .lst[names(.lst) != "envir"]
    if (.isPipe) {
      if (length(.lst) == 3) {
        .from <- eval(.lst[[2]], envir = envir)
        .to <- eval(.lst[[3]], envir = envir)
        .lst <- .lst[-3]
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        checkmate::assertNumeric(.to, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "to")
        .lst[[2]] <- seq(from = .from, to = .to, by = by)
        return(do.call(et.default, .lst, envir = envir))
      } else {
        .from <- eval(.lst[[2]], envir = envir)
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        .lst[[2]] <- seq(from = .from, by = by)
        return(do.call(et.default, .lst, envir = envir))
      }
    } else {
      if (length(.lst) == 2) {
        .from <- eval(.lst[[1]], envir = envir)
        .to <- eval(.lst[[2]], envir = envir)
        .lst <- .lst[-2]
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        checkmate::assertNumeric(.to, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "to")
        .lst[[1]] <- seq(from = .from, to = .to, by = by)
        return(do.call(et.default, .lst, envir = envir))
      } else {
        .from <- eval(.lst[[1]], envir = envir)
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        .lst[[1]] <- seq(from = .from, by = by)
        return(do.call(et.default, .lst, envir = envir))
      }
    }
  }
  if (!missing(length.out)) {
    checkmate::assertCount(length.out)
    .lst <- .lst[names(.lst) != "length.out"]
    .lst <- .lst[names(.lst) != "envir"]
    if (.isPipe) {
      if (length(.lst) == 3) {
        .from <- eval(.lst[[2]], envir = envir)
        .to <- eval(.lst[[3]], envir = envir)
        .lst <- .lst[-3]
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        checkmate::assertNumeric(.to, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "to")
        .lst[[2]] <- seq(from = .from, to = .to, length.out = length.out)
        return(do.call(et.default, .lst, envir = envir))
      } else {
        .from <- eval(.lst[[2]], envir = envir)
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        .lst[[2]] <- seq(from = .from, length.out = length.out)
        return(do.call(et.default, .lst, envir = envir))
      }
    } else {
      if (length(.lst) == 2) {
        .from <- eval(.lst[[1]], envir = envir)
        .to <- eval(.lst[[2]], envir = envir)
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        checkmate::assertNumeric(.to, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "to")
        .lst <- eval(.lst[-2], envir = envir)
        .lst[[1]] <- seq(from = .from, to = .to, length.out = length.out)
        return(do.call(et.default, .lst, envir = envir))
      } else {
        .from <- eval(.lst[[1]], envir = envir)
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        .lst[[1]] <- seq(from = .from, length.out = length.out)
        return(do.call(et.default, .lst, envir = envir))
      }
    }
  }
  if (!.isPipe) {
    if (all(names(.lst) == "") && length(.lst) == 2) {
      if ((is(.lst[[1]], "numeric") || is(.lst[[1]], "integer")) &&
        (is(.lst[[2]], "numeric") || is(.lst[[2]], "integer"))) {
        .from <- eval(.lst[[1]], envir = envir)
        .to <- eval(.lst[[2]], envir = envir)
        .lst <- .lst[-2]
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        checkmate::assertNumeric(.to, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "to")
        .lst[[1]] <- seq(from = .from, to = .to)
        return(do.call(et.default, .lst, envir = envir))
      }
    }
    .len <- sum(names(.lst) == "")
    if (.len == 2 && is(.lst[[2]], "character")) {
    } else if (.len > 1) {
      stop("improper arguments to 'et'", call. = FALSE)
    }
  } else {
    if (all(names(.lst)[-1] == "") && length(.lst) == 3) {
      if ((is(.lst[[2]], "numeric") || is(.lst[[2]], "integer")) &&
        (is(.lst[[3]], "numeric") || is(.lst[[3]], "integer"))) {
        .from <- eval(.lst[[2]], envir = envir)
        .to <- eval(.lst[[3]], envir = envir)
        checkmate::assertNumeric(.from, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "from")
        checkmate::assertNumeric(.to, finite = TRUE, max.len = 1, any.missing = FALSE, .var.name = "to")
        .lst <- .lst[-3]
        .lst[[2]] <- seq(from = .from, to = .to)
        return(do.call(et.default, .lst, envir = envir))
      }
    }
    .len <- sum(names(.lst)[-1] == "")
    if (.len == 2 && is(.lst[[3]], "character")) {
    } else if (.len > 1) {
      if (sum(names(.lst)[-1] == "") > 1) {
        stop("improper arguments to 'et'", call. = FALSE)
      }
    }
  }
  if (!missing(amt)) {
    if (length(amt) > 1) {
      if (missing(time)) {
        time <- 0
      } else if (length(time) != length(amt)) {
        if (length(time) != 1) {
          stop("when supplying vectors of 'time', 'amt' they need to be the same size", call. = FALSE)
        }
      }
      .df <- data.frame(time = time, amt = amt)
      ##
      if (!missing(id)) {
        .df$id <- id
      }
      if (missing(cmt)) {
        .df$cmt <- "(default)"
      } else {
        .df$cmt <- cmt
      }
      .df$amt <- amt
      if (missing(rate)) {
        .df$rate <- 0.0
      } else {
        .df$rate <- rate
      }
      if (missing(ii)) {
        .df$ii <- 0.0
      } else {
        .df$ii <- ii
      }
      if (missing(addl)) {
        .df$addl <- 0L
      } else {
        .df$addl <- addl
      }
      if (missing(evid)) {
        .df$evid <- 1L
      } else {
        .df$evid <- evid
      }
      if (missing(ss)) {
        .df$ss <- 0L
      } else {
        .df$ss <- ss
      }
      if (missing(dur)) {
        .df$dur <- 0.0
      } else {
        .df$dur <- dur
      }
      .et <- et()
      .et$import.EventTable(.df)
      if (.isPipe) {
        .tmp <- eval(.lst[[1]], envir = envir)
        if (nrow(.et) == 0) {
          return(.tmp)
        } else if (nrow(.tmp) == 0) {
          return(.et)
        } else {
          return(etRbind(.tmp, .et))
        }
      } else {
        return(.et)
      }
    }
  }
  if (!missing(time)) {
    if (inherits(time, "list")) {
      checkmate::assertList(time,
        any.missing = FALSE,
        unique = FALSE,
        names = "unnamed"
      )
    } else {
      checkmate::assertNumeric(time,
        finite = TRUE,
        any.missing = FALSE,
        unique = TRUE,
        names = "unnamed"
      )
    }
    .lst$time <- time
  }
  if (!missing(amt)) {
    checkmate::assertNumeric(amt,
      finite = TRUE,
      any.missing = FALSE,
      max.len = 1,
      names = "unnamed"
    )
    .lst$amt <- amt
  }
  if (!missing(evid)) {
    .evid <- as.character(substitute(evid))
    if (length(.evid) != 1) {
      if (all(.evid == .evid[1])) {
        .evid <- .evid[1]
      } else {
        .evid0 <- suppressWarnings(try(as.numeric(evid), silent = TRUE))
        if (inherits(.evid, "try-error")) {
          stop(sprintf(
            gettext("only a single evid 'evid' can be specified ('%s')"),
            paste(.evid, collapse = "', '")
          ), call. = FALSE)
        } else {
          .evid <- .evid0
        }
      }
    }
    if (.evid == "obs" || .evid == "0") {
      .tmp <- try(eval(evid, envir = envir), silent = TRUE)
      if (inherits(.tmp, "try-error")) {
        .lst$evid <- 0L
      } else {
        .lst$evid <- as.integer(.tmp)
      }
    } else if (.evid == "dose" || .evid == "1") {
      .tmp <- try(eval(evid, envir = envir), silent = TRUE)
      if (inherits(.tmp, "try-error")) {
        .lst$evid <- 1L
      } else {
        .lst$evid <- as.integer(.tmp)
      }
    } else if (.evid == "other" || .evid == "2") {
      .tmp <- try(eval(evid, envir = envir), silent = TRUE)
      if (inherits(.tmp, "try-error")) {
        .lst$evid <- 2L
      } else {
        .lst$evid <- as.integer(.tmp)
      }
    } else if (.evid == "reset" || .evid == "3") {
      .tmp <- try(eval(evid, envir = envir), silent = TRUE)
      if (inherits(.tmp, "try-error")) {
        .lst$evid <- 3L
      } else {
        .lst$evid <- as.integer(.tmp)
      }
    } else if (.evid == "doseReset" || .evid == "resetDose" || .evid == "4") {
      .tmp <- try(eval(evid, envir = envir), silent = TRUE)
      if (inherits(.tmp, "try-error")) {
        .lst$evid <- 4L
      } else {
        .lst$evid <- as.integer(.tmp)
      }
    } else {
      .lst$evid <- as.integer(evid)
    }
  }
  if (!missing(cmt)) {
    .cmt <- as.character(substitute(cmt))
    if (length(.cmt) != 1) {
      if (all(.cmt == .cmt[1])) {
        .cmt <- .cmt[1]
      } else {
        .cmt0 <- suppressWarnings(try(as.numeric(cmt), silent = TRUE))
        if (inherits(.cmt, "try-error")) {
          stop(sprintf(
            gettext("only a single compartment 'cmt' can be specified ('%s')"),
            paste(.cmt, collapse = "', '")
          ), call. = FALSE)
        } else {
          .cmt <- .cmt0
        }
      }
    }
    .cmt1 <- try(suppressWarnings(as.integer(cmt)), silent = TRUE)
    if (inherits(.cmt1, "try-error")) {
      .lst$cmt <- .cmt
    } else {
      if (is.na(.cmt1)) {
        .lst$cmt <- .cmt
      } else {
        .lst$cmt <- .cmt1
      }
    }
  }
  if (!missing(rate)) {
    .rate <- as.character(substitute(rate))
    if (length(.rate) != 1) {
      if (all(.rate == .rate[1])) {
        .rate <- .rate[1]
      } else {
        .rate0 <- suppressWarnings(try(as.numeric(rate), silent = TRUE))
        if (inherits(.rate, "try-error")) {
          stop(sprintf(
            gettext("only a single rate 'rate' can be specified ('%s')"),
            paste(.rate, collapse = "', '")
          ), call. = FALSE)
        } else {
          .rate <- .rate0
        }
      }
    }
    .lst$rate <- rate
  }
  if (!missing(dur)) {
    .dur <- as.character(substitute(dur))
    if (length(.dur) != 1) {
      if (all(.dur == .dur[1])) {
        .dur <- .dur[1]
      } else {
        .dur0 <- suppressWarnings(try(as.numeric(dur), silent = TRUE))
        if (inherits(.dur, "try-error")) {
          stop(sprintf(
            gettext("only a single duration 'dur' can be specified ('%s')"),
            paste(.dur, collapse = "', '")
          ), call. = FALSE)
        } else {
          .dur <- .dur0
        }
      }
    }
    .lst$dur <- dur
  }
  .unitNames <- names(.lst)
  .unitNames <- .unitNames[regexpr("^(amount|time)", .unitNames) != -1]
  .unitNames <- .unitNames[.unitNames != "time"]
  for (.u in .unitNames) {
    if (inherits(.lst[[.u]], "name")) {
      .tmp <- .lst[[.u]]
      .tmp <- deparse(substitute(.tmp))
      .lst[[.u]] <- .tmp
    }
  }
  .lst <- lapply(.lst, function(x) {
    eval(x, envir)
  })
  .Call(`_rxode2_et_`, .lst, list())
}

#' @export
`$.rxEt` <- function(obj, arg, exact = FALSE) {
  return(.Call(`_rxode2_etUpdate`, obj, arg, NULL, exact))
}

#' @export
simulate.rxEt <- # nolint
  function(object, nsim = 1, seed = NULL, ...) {
    .name <- as.character(substitute(object))
    if (is.null(.pipelineRx) || .name != ".") {
      if (!missing(nsim)) warning("'nsim' is ignored when simulating event tables", call. = FALSE)
      if (!is.null(seed)) set.seed(seed)
      return(.Call(`_rxode2_et_`, list(simulate = TRUE), object))
    } else {
      return(rxSolve(object, ..., seed = seed, nsim = nsim))
    }
  }

drop_units.rxEt <- function(x) {
  if (requireNamespace("units", quietly = TRUE)) {
    stop("requires package 'units'", call. = FALSE)
  }
  .Call(`_rxode2_et_`, list(amountUnits = NA_character_, timeUnits = NA_character_), x)
}

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
    return(suppressWarnings({
      .Call(`_rxode2_et_`, list(amountUnits = "", timeUnits = ""), x)
    }))
  } else {
    if (!rxIs(value, "character")) value <- deparse(value)
    .tUnit <- units::set_units(1, "sec", mode = "standard")
    .isTime <- try(units::set_units(units::set_units(1, value, mode = "standard"), "sec"), silent = TRUE)
    if (inherits(.isTime, "try-error")) {
      ## Amount
      return(.Call(`_rxode2_et_`, list(amountUnits = value), x))
    } else {
      ##
      return(.Call(`_rxode2_et_`, list(timeUnits = value), x))
    }
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
# nolint start
add.dosing <- function(eventTable, dose, nbr.doses = 1L,
                       dosing.interval = 24, dosing.to = 1L,
                       rate = NULL, amount.units = NA_character_,
                       start.time = 0.0, do.sampling = FALSE,
                       time.units = NA_character_, ...) {
  checkmate::assertDouble(dose, any.missing = FALSE, finite = TRUE, max.len = 1)
  checkmate::assertDouble(dosing.interval, lower = 0, any.missing = FALSE, finite = TRUE, max.len = 1)
  checkmate::assertDouble(start.time, any.missing = FALSE, finite = TRUE, max.len = 1)
  .lst <- list(
    dose = dose,
    nbr.doses = nbr.doses,
    start.time = start.time,
    do.sampling = do.sampling,
    ...
  )
  if (!is.na(amount.units)) .lst$amount.units <- amount.units
  if (!is.na(time.units)) .lst$time.units <- time.units
  if (dosing.to != 1) .lst$dosing.to <- dosing.to
  if (!is.null(rate)) .lst$rate <- rate
  if (nbr.doses > 1) {
    .lst$dosing.interval <- dosing.interval
  } else {
    .lst$dosing.interval <- 0.0
  }
  checkmate::assertIntegerish(nbr.doses, lower = 1L, any.missing = FALSE, max.len = 1)
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
add.sampling <- function(eventTable, time, time.units = NA) {
  .lst <- list(time = time)
  if (!is.na(time.units)) .lst$time.units <- time.units
  return(.Call(`_rxode2_et_`, .lst, eventTable))
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
#' @seealso [et()], [rxode2()]
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
#' qd2 <- eventTable(amount.units = "mg", time.units = "days") %>%
#'   add.dosing(dose = 50, nbr.doses = 5, dosing.interval = 1, do.sampling = FALSE) %>%
#'   add.sampling(seq(from = 0, to = 1, by = 1 / 24)) %>%
#'   add.sampling(seq(from = 1, to = 5, by = 12 / 24))
#' # print(qd2$get.dosing())     # table of dosing records
#' print(qd2$get.nobs()) # number of observation (not dosing) records
#'
#' # Note that piping with %>% will update the original table.
#'
#' qd3 <- qd2 %>% add.sampling(seq(from = 5, to = 10, by = 6 / 24))
#' print(qd2$get.nobs())
#' print(qd3$get.nobs())
#' @keywords models data
#' @concept ordinary differential equations
#' @concept Nonlinear regression
#' @concept Pharmacokinetics (PK)
#' @concept Pharmacodynamics (PD)
#' @export
eventTable <- function(amount.units = NA, time.units = NA) {
  .lst <- list()
  if (!missing(amount.units)) {
    checkmate::assertCharacter(amount.units, max.len = 1)
    .lst$amount.units <- amount.units
  }
  if (!missing(time.units)) {
    checkmate::assertCharacter(time.units, max.len = 1)
    .lst$time.units <- time.units
  }
  .Call(`_rxode2_et_`, .lst, list())
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
etSeq <- function(..., samples = c("clear", "use"), waitII = c("smart", "+ii"), ii = 24) {
  ## etSeq_(List ets, bool clearSampling=clearSampling);
  .sampleIx <- c(clear = 0L, use = 1L)
  .waitIx <- c(smart = 0L, `+ii` = 1L)
  .collectWarnings(.Call(
    `_rxode2_etSeq_`, list(...), setNames(.sampleIx[match.arg(samples)], NULL),
    setNames(.waitIx[match.arg(waitII)], NULL), as.double(ii), FALSE, 0L,
    0L, TRUE, character(0), logical(0), FALSE
  ))
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
etRbind <- function(..., samples = c("use", "clear"), waitII = c("smart", "+ii"),
                    id = c("merge", "unique")) {
  .sampleIx <- c(clear = 0L, use = 1L)
  .waitIx <- c(smart = 0L, `+ii` = 1L)
  .idIx <- c(merge = 0L, unique = 1L)
  .collectWarnings(.Call(
    `_rxode2_etSeq_`, list(...), setNames(.sampleIx[match.arg(samples)], NULL),
    setNames(.waitIx[match.arg(waitII)], NULL), as.double(0), TRUE,
    setNames(.idIx[match.arg(id)], NULL),
    0L, TRUE, character(0), logical(0), FALSE
  ))
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
  if (!is.null(n)) {
    times <- n
  }
  .sampleIx <- c(clear = 0L, use = 1L)
  .waitIx <- c(smart = 0L, `+ii` = 1L)
  if (!is.na(length.out)) stop("'length.out' makes no sense with event tables", call. = FALSE)
  if (!is.na(each)) stop("'each' makes no sense with event tables", call. = FALSE)
  .collectWarnings(.Call(
    `_rxode2_etRep_`, x, as.integer(times),
    wait, as.integer(id), setNames(.sampleIx[match.arg(samples)], NULL),
    setNames(.waitIx[match.arg(waitII)], NULL), as.double(ii)
  ))
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
  .e$import.EventTable(as.data.frame(x))
  return(.e)
}
#' @export
as.data.frame.rxEt <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (rxIs(x, "rxEt")) {
    .x <- x
    .tmp <- .x[, .x$show, drop = FALSE]
    class(.tmp) <- c("rxEt2", "data.frame")
    return(as.data.frame(.tmp, row.names = NULL, optional = FALSE, ...))
  } else {
    return(as.data.frame(x, row.names = NULL, optional = FALSE, ...))
  }
}

.datatable.aware <- TRUE
#' Convert an event table to a data.table
#'
#' @inheritParams data.table::as.data.table
#'
#' @return data.table of event table
#'
#' @noRd
as.data.table.rxEt <- function(x, keep.rownames = FALSE, ...) {
  rxReq("data.table")
  return(data.table::as.data.table(as.data.frame.rxEt(x, ...), keep.rownames = keep.rownames, ...))
}

#' Convert to tbl
#'
#' @param x rxode2 event table
#'
#' @param ... Other arguments to `as_tibble`
#'
#' @return tibble of event table
#'
#' @noRd
as_tibble.rxEt <- function(x, ...) {
  rxReq("tibble")
  if (rxIs(x, "rxEt")) {
    .x <- x
    .show <- .x$show
    class(.x) <- "data.frame"
    .tmp <- .x[, .show, drop = FALSE]
    return(tibble::as_tibble(.tmp, ...))
  } else {
    return(tibble::as_tibble(x, ...))
  }
}

#' Check to see if this is an rxEt object.
#'
#' @param x object to check to see if it is rxEt
#'
#' If this is an rxEt object that has expired strip all rxEt
#' information.
#'
#' @return Boolean indicating if this is a rxode2 event table
#'
#' @author Matthew L.Fidler
#' @export
#' @keywords internal
is.rxEt <- function(x) {
  .Call(`_rxode2_rxIs`, x, "rxEt")
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
  .Call(`_rxode2_et_`, list(expand = TRUE), et)
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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
  .x <-
    ifelse(.x == 0, paste0(crayon::blue$bold("0"), ":", crayon::white("Observation")),
      ifelse(.x == 1, paste0(crayon::blue$bold("1"), ":", crayon::yellow("Dose (Add)")),
        ifelse(.x == 2, paste0(crayon::blue$bold("2"), ":", crayon::yellow("Other")),
          ifelse(.x == 3, paste0(crayon::blue$bold("3"), ":", crayon::red("Reset")),
            ifelse(.x == 4, paste0(crayon::blue$bold("4"), ":", crayon::red("Reset"), "&", crayon::yellow("Dose")),
              ifelse(.x == 5, paste0(crayon::blue$bold("5"), ":", crayon::red("Replace")),
                ifelse(.x == 6, paste0(crayon::blue$bold("6"), ":", crayon::yellow("Multiply")),
                  paste0(crayon::blue$red(.x), ":", crayon::red("Invalid"))
                )
              )
            )
          )
        )
      )
    )
  return(format(.x, justify = "left"))
}

#' @rdname rxEvid
#' @export
as.character.rxEvid <- function(x, ...) {
  .x <- unclass(x)
  .x <-
    ifelse(.x == 0, "0:Observation",
      ifelse(.x == 1, "1:Dose (Add)",
        ifelse(.x == 2, "2:Other",
          ifelse(.x == 3, "3:Reset",
            ifelse(.x == 4, "4:Reset&Dose",
              ifelse(.x == 5, "5:Replace",
                ifelse(.x == 6, "6:Multiply",
                  paste0(.x, ":Invalid")
                )
              )
            )
          )
        )
      )
    )
  return(.x)
}



#' @rdname rxEvid
#' @export
`[[.rxEvid` <- function(x, ...) {
  as.rxEvid(NextMethod())
}

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
