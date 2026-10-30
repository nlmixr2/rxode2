# tte() simulation event table: the records added for each endpoint (their
# roles are encoded in dv, see src/tte.h).

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
#' the observation window (first to last record) and covariate values; they
#' are kept as non-output records, and a start record, a right-censoring
#' record (or the interval ends for `eventType="intervalCensored"`) and free
#' event-slot records that the solver re-times to the simulated event times
#' (unused slots are dropped from the output) are added.  The added records
#' have no covariate values, so the solver takes the ones in effect at their
#' (possibly re-timed) times.  Each record's role is encoded in its `dv` (see
#' `src/tte.h`).
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
        .g <- if (.start + .l <= .end) seq(.start + .l, .end, by = .l) else numeric(0)
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
    # the endpoint's own records stay for their covariates, out of the output
    .d[[.evid]][.isTte] <- 0L
    .d[[.dv]][.isTte] <- -(1e6 + 10 * .j + 7)
  }
  .d <- do.call(rbind, c(list(.d), .add))
  .d <- .d[order(.d[[.id]], .d[[.time]]), , drop = FALSE]
  rownames(.d) <- NULL
  .d
}
