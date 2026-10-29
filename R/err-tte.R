# Time-to-event (tte) endpoints: `ev ~ tte(hazard, eventType=, maxEventNumber=,
# rightCensoringTime=, intervalLength=, dropout=)`.  Like ordinal endpoints the
# options are not stored in $predDf/$iniDf; they are re-read from the endpoint's
# expression in `lstExpr` with `.rxTteOptions()`.

.rxTteOptionNames <- c(
  "eventType",
  "maxEventNumber",
  "rightCensoringTime",
  "intervalLength",
  "dropout"
)

.rxTteEventTypes <- c("exact", "intervalCensored")

#' Parse and validate a tte() call
#'
#' @param expr the `tte(...)` call
#' @return list with `hazard` (quoted), the options (NA when unspecified,
#'   `eventType`/`maxEventNumber`/`dropout` filled with their defaults) and
#'   `err`, a character vector of problems (empty when valid)
#' @author Matthew L. Fidler
#' @noRd
.rxTteParseCall <- function(expr) {
  .ret <- list(
    hazard = NULL,
    eventType = "exact",
    maxEventNumber = Inf,
    rightCensoringTime = NA_real_,
    intervalLength = NA_real_,
    dropout = FALSE,
    err = character(0)
  )
  .args <- as.list(expr)[-1]
  .n <- names(.args)
  if (is.null(.n)) {
    .n <- rep("", length(.args))
  }
  .pos <- which(.n == "")
  if (length(.pos) == 0L) {
    .ret$err <- "'tte()' requires the hazard as its first argument (i.e. ev ~ tte(haz))"
    return(.ret)
  }
  if (length(.pos) > 1L || .pos[1] != 1L) {
    .ret$err <- "'tte()' takes the hazard as its only unnamed (first) argument; options must be named"
    return(.ret)
  }
  .ret$hazard <- .args[[1]]
  .bad <- setdiff(.n[-1], .rxTteOptionNames)
  if (length(.bad) > 0L) {
    .ret$err <- c(
      .ret$err,
      paste0(
        "unknown 'tte()' option(s): ",
        paste(.bad, collapse = ", "),
        "; supported: ",
        paste(.rxTteOptionNames, collapse = ", ")
      )
    )
  }
  if (anyDuplicated(.n[-1])) {
    .ret$err <- c(.ret$err, "duplicated 'tte()' option(s)")
  }
  .num <- function(.name, .allowInf = FALSE) {
    .v <- .args[[.name]]
    if (is.call(.v) && identical(.v[[1]], quote(`-`)) && length(.v) == 2L && is.numeric(.v[[2]])) {
      .v <- -.v[[2]]
    }
    if (is.name(.v) && identical(.v, quote(Inf))) {
      .v <- Inf
    }
    if (!is.numeric(.v) || length(.v) != 1L || is.na(.v) || (!.allowInf && !is.finite(.v)) || .v <= 0) {
      .ret$err <<- c(.ret$err, paste0("'tte()' option '", .name, "' must be a positive number"))
      return(NA_real_)
    }
    as.numeric(.v)
  }
  if (!is.null(.args$eventType)) {
    .v <- .args$eventType
    if (is.name(.v)) .v <- as.character(.v)
    if (!is.character(.v) || length(.v) != 1L || !(.v %in% .rxTteEventTypes)) {
      .ret$err <- c(
        .ret$err,
        paste0("'tte()' option 'eventType' must be one of: ", paste0("\"", .rxTteEventTypes, "\"", collapse = ", "))
      )
    } else {
      .ret$eventType <- .v
    }
  }
  if (!is.null(.args$maxEventNumber)) {
    .v <- .num("maxEventNumber", .allowInf = TRUE)
    if (!is.na(.v) && is.finite(.v) && .v != round(.v)) {
      .ret$err <- c(.ret$err, "'tte()' option 'maxEventNumber' must be a whole number (or Inf)")
    } else {
      .ret$maxEventNumber <- .v
    }
  }
  if (!is.null(.args$rightCensoringTime)) {
    .ret$rightCensoringTime <- .num("rightCensoringTime")
  }
  if (!is.null(.args$intervalLength)) {
    .ret$intervalLength <- .num("intervalLength")
  }
  if (!is.null(.args$dropout)) {
    .v <- .args$dropout
    if (is.name(.v)) .v <- c(T = TRUE, F = FALSE)[as.character(.v)]
    if (!is.logical(.v) || length(.v) != 1L || is.na(.v)) {
      .ret$err <- c(.ret$err, "'tte()' option 'dropout' must be TRUE or FALSE")
    } else {
      .ret$dropout <- .v
    }
  }
  if (isTRUE(.ret$dropout) && !identical(.ret$maxEventNumber, 1)) {
    .ret$err <- c(.ret$err, "'tte()' option 'dropout=TRUE' requires 'maxEventNumber=1'")
  }
  .ret
}

#' Handle the tte() error structure while parsing the model
#'
#' @param expression the `tte(...)` call
#' @param env parsing environment
#' @return nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.errHandleTte <- function(expression, env) {
  env$distribution <- "tte"
  .p <- .rxTteParseCall(expression)
  if (length(.p$err) > 0L) {
    env$err <- c(env$err, .p$err)
  }
  env$errTypeInfo <- rxErrTypeCombine(env$errTypeInfo, "tte")
  env$needsToBeAnErrorExpression <- TRUE
  invisible()
}

#' Get the tte() options of an endpoint
#'
#' @param env parsed ui environment (holds `lstExpr`)
#' @param pred1 single `$predDf` row of a tte endpoint
#' @return the list from `.rxTteParseCall()`
#' @author Matthew L. Fidler
#' @noRd
.rxTteOptions <- function(env, pred1) {
  .rhs <- env$lstExpr[[pred1$line[1]]][[3]]
  if (is.call(.rhs) && identical(.rhs[[1]], quote(`|`))) {
    .rhs <- .rhs[[2]]
  }
  .rxTteParseCall(.rhs)
}

#' Which tte endpoint (1-based, in $predDf order) is this endpoint?
#'
#' @param predDf the ui's `$predDf`
#' @param cond endpoint condition
#' @return integer tte index, or NA when `cond` is not a tte endpoint
#' @author Matthew L. Fidler
#' @noRd
.rxTteIndex <- function(predDf, cond) {
  .tte <- predDf$cond[predDf$distribution == "tte"]
  match(cond, .tte)
}

#' Name of the hidden cumulative-hazard state of a tte endpoint
#'
#' @param var endpoint variable
#' @return state name
#' @noRd
.rxTteStateName <- function(var) {
  paste0("rx.tteH.", var)
}

#' Hidden cumulative-hazard ODE lines for every tte endpoint
#'
#' @param ui rxode2 ui
#' @return list of `d/dt(rx.tteH.<var>) <- <hazard>` lines (NULL when the model
#'   has no tte endpoint)
#' @author Matthew L. Fidler
#' @noRd
.rxTteStateLines <- function(ui) {
  .predDf <- ui$predDf
  if (is.null(.predDf)) {
    return(NULL)
  }
  .w <- which(.predDf$distribution == "tte")
  if (length(.w) == 0L) {
    return(NULL)
  }
  lapply(.w, function(.i) {
    .o <- .rxTteOptions(ui, .predDf[.i, ])
    .h <- .rxTteRewriteLines(list(.o$hazard), .predDf)[[1]]
    bquote(d / dt(.(str2lang(.rxTteStateName(.predDf$var[.i])))) <- .(.h))
  })
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.tte <- function(line) {
  .env <- line[[1]]
  .pred1 <- line[[2]]
  .k <- as.numeric(.rxTteIndex(.env$predDf, .pred1$cond))
  .h <- str2lang(.rxTteStateName(.pred1$var))
  list(
    bquote(ipredSim <- exp(-(.(.h) - rxTteH0(.(.k))))),
    bquote(sim <- rxTteSim(.(.k)))
  )
}

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

#' Find a column of a data frame ignoring case
#'
#' @param d data frame
#' @param name lower-case column name
#' @return the column name or NULL
#' @noRd
.rxTteCol <- function(d, name) {
  .w <- which(tolower(names(d)) == name)
  if (length(.w) == 0L) {
    return(NULL)
  }
  names(d)[.w[1]]
}

#' Add the tte() records to a simulation event table
#'
#' For every subject and tte endpoint the endpoint's own records only define
#' the observation window (first to last record) and are replaced by a start
#' record, a right-censoring record (or the interval ends for
#' `eventType="intervalCensored"`) and free event-slot records that the solver
#' re-times to the simulated event times (unused slots are dropped from the
#' output).  Each record's role is
#' encoded in its `dv` (see `src/tte.h`).
#'
#' @param ui rxode2 ui
#' @param events event table
#' @param maxSlots event slots per subject and endpoint when `maxEventNumber`
#'   is unlimited
#' @return data frame of events
#' @author Matthew L. Fidler
#' @noRd
.rxTteEvents <- function(ui, events, maxSlots = getOption("rxode2.tteMaxEvents", 100L)) {
  if (is.null(events)) {
    stop("simulating a tte() endpoint requires an event table", call. = FALSE)
  }
  .d <- as.data.frame(events)
  for (.i in seq_along(.d)) {
    if (is.factor(.d[[.i]])) .d[[.i]] <- as.character(.d[[.i]])
  }
  .time <- .rxTteCol(.d, "time")
  if (is.null(.time)) {
    stop("simulating a tte() endpoint requires a 'time' column", call. = FALSE)
  }
  .id <- .rxTteCol(.d, "id")
  if (is.null(.id)) {
    .id <- "id"
    .d$id <- 1L
  }
  .dv <- .rxTteCol(.d, "dv")
  if (is.null(.dv)) {
    .dv <- "dv"
    .d$dv <- NA_real_
  }
  .amt <- .rxTteCol(.d, "amt")
  if (is.null(.amt)) {
    .amt <- "amt"
    .d$amt <- 0
  }
  .evid <- .rxTteCol(.d, "evid")
  if (is.null(.evid)) {
    .evid <- "evid"
    .d$evid <- ifelse(!is.na(.d[[.amt]]) & .d[[.amt]] != 0, 1L, 0L)
  }
  .cmt <- .rxTteCol(.d, "cmt")
  .predDf <- ui$predDf
  if (is.null(.cmt) && nrow(.predDf) > 1L) {
    stop("simulating a tte() endpoint in a model with several endpoints requires a 'cmt' column", call. = FALSE)
  }
  .w <- which(.predDf$distribution == "tte")
  .keep <- rep(TRUE, nrow(.d))
  .add <- list()
  .ids <- unique(.d[[.id]])
  for (.j in seq_along(.w)) {
    .pred1 <- .predDf[.w[.j], ]
    .o <- .rxTteOptions(ui, .pred1)
    # without a cmt column the single (tte) endpoint owns every observation
    .isTte <- .d[[.evid]] %in% c(0L, 2L)
    .cmtVal <- NULL
    if (!is.null(.cmt)) {
      .c <- .d[[.cmt]]
      if (is.character(.c)) {
        .isTte <- !is.na(.c) & .c == .pred1$cond
        .cmtVal <- .pred1$cond
      } else {
        .isTte <- !is.na(.c) & .c == .pred1$cmt
        .cmtVal <- .pred1$cmt
      }
      .isTte <- .isTte & .d[[.evid]] %in% c(0L, 2L)
    }
    .nSlot <- as.integer(min(.o$maxEventNumber, maxSlots))
    for (.cid in .ids) {
      .r <- .d[[.id]] == .cid
      .tAll <- .d[[.time]][.r]
      .tTte <- .d[[.time]][.r & .isTte]
      .start <- if (length(.tTte) > 0L) min(.tTte) else min(.tAll)
      .end <- .o$rightCensoringTime
      if (is.na(.end)) {
        .end <- if (length(.tTte) > 0L) max(.tTte) else max(.tAll)
      }
      if (!(.end > .start)) next
      .t <- .start
      .role <- 1L
      if (.o$eventType == "intervalCensored") {
        .l <- .o$intervalLength
        if (is.na(.l)) .l <- (.end - .start) / 10
        .g <- seq(.start + .l, .end, by = .l)
        if (length(.g) == 0L || .end - .g[length(.g)] > sqrt(.Machine$double.eps) * max(1, abs(.end))) {
          .g <- c(.g, .end)
        }
        .t <- c(.t, .g)
        .role <- c(.role, rep(3L, length(.g)))
      } else {
        .t <- c(.t, .end)
        .role <- c(.role, 2L)
      }
      if (.nSlot > 0L) {
        .t <- c(.t, rep(.end, .nSlot))
        .role <- c(.role, rep(4L, .nSlot))
      }
      .ev <- rep(0L, length(.t))
      .a <- .d[rep(which(.r)[1], length(.t)), , drop = FALSE]
      .a[] <- lapply(.a, function(x) {
        x[] <- NA
        x
      })
      .a[[.id]] <- .cid
      .a[[.time]] <- .t
      .a[[.evid]] <- .ev
      .a[[.dv]] <- -(1e6 + 10 * .j + .role)
      if (!is.null(.cmt)) .a[[.cmt]] <- .cmtVal
      .add[[length(.add) + 1L]] <- .a
    }
    .keep <- .keep & !.isTte
  }
  .d <- do.call(rbind, c(list(.d[.keep, , drop = FALSE]), .add))
  .d <- .d[order(.d[[.id]], .d[[.time]]), , drop = FALSE]
  rownames(.d) <- NULL
  .d
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
  .events <- lst$events
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

#' Rewrite `tteTime(ev)`/`tteCount(ev)` to the endpoint's tte index
#'
#' @param expr quoted model expression
#' @param tte names of the model's tte endpoints
#' @return the rewritten expression
#' @author Matthew L. Fidler
#' @noRd
.rxTteRewrite <- function(expr, tte) {
  if (!is.call(expr)) {
    return(expr)
  }
  if ((identical(expr[[1]], quote(`tteTime`)) || identical(expr[[1]], quote(`tteCount`)))) {
    .f <- as.character(expr[[1]])
    if (length(expr) != 2L || !is.name(expr[[2]])) {
      stop("'", .f, "()' takes one tte() endpoint name, e.g. ", .f, "(ev)", call. = FALSE)
    }
    .k <- match(as.character(expr[[2]]), tte)
    if (is.na(.k)) {
      stop("'", .f, "(", as.character(expr[[2]]), ")' does not name a tte() endpoint", call. = FALSE)
    }
    expr[[2]] <- as.numeric(.k)
    return(expr)
  }
  as.call(lapply(as.list(expr), .rxTteRewrite, tte = tte))
}

#' Rewrite the tteTime()/tteCount() calls of a list of model lines
#'
#' @param lines list of quoted lines, or character lines
#' @param predDf the ui's `$predDf`
#' @return the rewritten lines (same type)
#' @noRd
.rxTteRewriteLines <- function(lines, predDf) {
  .tte <- if (is.null(predDf)) character(0) else predDf$cond[predDf$distribution == "tte"]
  if (is.character(lines)) {
    .w <- grepl("tte(Time|Count)", lines)
    if (!any(.w)) {
      return(lines)
    }
    lines[.w] <- vapply(
      lines[.w],
      function(.l) deparse1(.rxTteRewrite(str2lang(.l), .tte)),
      character(1),
      USE.NAMES = FALSE
    )
    return(lines)
  }
  lapply(lines, .rxTteRewrite, tte = .tte)
}
