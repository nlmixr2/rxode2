# Default show flags (controls which columns print/as.data.frame expose)
.etDefaultShow <- function() {
  c(id = FALSE, low = FALSE, time = TRUE, high = FALSE, cmt = FALSE,
    amt = FALSE, rate = FALSE, ii = FALSE, addl = FALSE, evid = TRUE,
    ss = FALSE, dur = FALSE)
}

#' Attach method closures to an rxEt's list structure
#'
#' Each method captures .env by reference so mutations are shared.
#' @param .env environment (mutable state)
#' @return named list of closures
#' @noRd
.etBuildMethods <- function(.env) {
  list(
    add.dosing = function(dose, nbr.doses = 1L, dosing.interval = 24,
                           dosing.to = 1L, rate = NULL,
                           amount.units = NA_character_,
                           start.time = 0.0, do.sampling = FALSE,
                           time.units = NA_character_, ...) {
      .cmt <- if (dosing.to == 1L) "(default)" else as.integer(dosing.to)
      .chunk <- .etDoseChunk(
        time = start.time, amt = dose, cmt = .cmt,
        ii   = if (nbr.doses > 1L) dosing.interval else 0.0,
        addl = as.integer(nbr.doses) - 1L,
        rate = if (!is.null(rate)) rate else 0.0,
        ...
      )
      .env$chunks <- c(.env$chunks, list(.chunk))
      .env$ndose  <- .env$ndose + 1L
      .env$show["amt"] <- TRUE
      if (as.integer(nbr.doses) > 1L) {
        .env$show["ii"]   <- TRUE
        .env$show["addl"] <- TRUE
      }
      if (!is.na(amount.units)) .env$units["dosing"] <- amount.units
      if (!is.na(time.units))   .env$units["time"]   <- time.units
      invisible(NULL)
    },

    add.sampling = function(time, time.units = NA_character_) {
      .chunk <- .etObsChunk(time)
      .env$chunks <- c(.env$chunks, list(.chunk))
      .env$nobs   <- .env$nobs + length(time)
      if (!is.na(time.units)) .env$units["time"] <- time.units
      invisible(NULL)
    },

    get.units = function() .env$units,
    getUnits  = function() .env$units,
    get_units = function() .env$units,

    get.nobs  = function() .env$nobs,
    get.EventTable = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      .show <- .env$show
      .mat[, names(.show)[.show], drop = FALSE]
    },
    get.obs.rec = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      .mat$evid == 0L
    },
    get.dosing = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      .mat[.mat$evid != 0L, , drop = FALSE]
    },
    get.sampling = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      .mat[.mat$evid == 0L, , drop = FALSE]
    },
    clear.sampling = function() {
      .obsIdx <- vapply(.env$chunks, function(.c) {
        .evid <- .c$evid
        if (is.null(.evid)) return(TRUE)   # obs chunk (evid defaults to 0)
        all(.evid == 0L)
      }, logical(1))
      .env$chunks <- .env$chunks[!.obsIdx]
      .env$nobs   <- 0L
      invisible(NULL)
    },
    clear.dosing = function() {
      .doseIdx <- vapply(.env$chunks, function(.c) {
        .evid <- .c$evid
        if (is.null(.evid)) return(FALSE)
        any(.evid != 0L)
      }, logical(1))
      .env$chunks <- .env$chunks[!.doseIdx]
      .env$ndose  <- 0L
      invisible(NULL)
    },
    copy = function() {
      .newEnv <- new.env(parent = emptyenv())
      .newEnv$chunks     <- .env$chunks
      .newEnv$units      <- .env$units
      .newEnv$show       <- .env$show
      .newEnv$IDs        <- .env$IDs
      .newEnv$nobs       <- .env$nobs
      .newEnv$ndose      <- .env$ndose
      .newEnv$randomType <- .env$randomType
      .newEnv$canResize  <- .env$canResize
      structure(c(list(.env = .newEnv), .etBuildMethods(.newEnv)), class = "rxEt")
    },
    expand = function() {
      .et <- structure(list(.env = .env), class = "rxEt")
      .mat <- .etMaterialize(.et)
      .expanded <- .etExpandAddlR(.mat)
      .env$chunks <- list(.expanded)
      invisible(NULL)
    },
    simulate = function(object, nsim = 1, seed = NULL, ...) {
      .simulate.rxEt(structure(list(.env = .env), class = "rxEt"), seed = seed)
    }
  )
}

#' Create a new empty rxEt object
#' @param amountUnits character dose unit, e.g. "mg"
#' @param timeUnits character time unit, e.g. "hours"
#' @return rxEt object
#' @noRd
.newRxEt <- function(amountUnits = NA_character_, timeUnits = NA_character_) {
  .env <- new.env(parent = emptyenv())
  .env$chunks     <- list()
  .env$units      <- c(dosing = amountUnits, time = timeUnits)
  .env$show       <- .etDefaultShow()
  .env$IDs        <- 1L
  .env$nobs       <- 0L
  .env$ndose      <- 0L
  .env$randomType <- NA_integer_
  .env$canResize  <- TRUE
  structure(c(list(.env = .env), .etBuildMethods(.env)), class = "rxEt")
}

# Canonical column order matching C++ etEmpty()
.etColOrder <- c("id", "low", "time", "high", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur")

#' Empty data.frame skeleton matching materialized format
#' @noRd
.etEmptyDf <- function() {
  data.frame(
    id    = integer(0),
    low   = numeric(0),
    time  = numeric(0),
    high  = numeric(0),
    cmt   = character(0),
    amt   = numeric(0),
    rate  = numeric(0),
    ii    = numeric(0),
    addl  = integer(0),
    evid  = integer(0),
    ss    = integer(0),
    dur   = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Materialize all chunks into a sorted data.frame
#' @param et rxEt object
#' @return data.frame with canonical columns, sorted by (id, time, evid-adjusted)
#' @noRd
.etMaterialize <- function(et) {
  .env <- .subset2(et, ".env")
  .chunks <- .env$chunks

  # Empty case — return skeleton
  if (length(.chunks) == 0L) {
    return(.etEmptyDf())
  }

  # Bind: data.table::rbindlist is ~10x faster than do.call(rbind, ...)
  # fill=TRUE handles sparse chunks (obs chunks lack amt/rate/ii/addl/ss/dur)
  .dt <- data.table::rbindlist(.chunks, fill = TRUE, use.names = TRUE)

  # ---- Fill column defaults ----
  if (is.null(.dt[["id"]]))   data.table::set(.dt, j = "id",   value = 1L)
  if (is.null(.dt[["evid"]])) data.table::set(.dt, j = "evid", value = 0L)
  if (is.null(.dt[["low"]]))  data.table::set(.dt, j = "low",  value = NA_real_)
  if (is.null(.dt[["high"]])) data.table::set(.dt, j = "high", value = NA_real_)
  if (is.null(.dt[["cmt"]]))  data.table::set(.dt, j = "cmt",  value = "(default)")

  # Dose-specific defaults — NA for obs records, 0 for dose records
  .isDose <- .dt$evid != 0L
  if (is.null(.dt[["amt"]]))  data.table::set(.dt, j = "amt",  value = NA_real_)
  if (is.null(.dt[["rate"]])) data.table::set(.dt, j = "rate", value = ifelse(.isDose, 0.0, NA_real_))
  if (is.null(.dt[["ii"]]))   data.table::set(.dt, j = "ii",   value = ifelse(.isDose, 0.0, NA_real_))
  if (is.null(.dt[["addl"]])) data.table::set(.dt, j = "addl", value = ifelse(.isDose, 0L,  NA_integer_))
  if (is.null(.dt[["ss"]]))   data.table::set(.dt, j = "ss",   value = ifelse(.isDose, 0L,  NA_integer_))
  if (is.null(.dt[["dur"]]))  data.table::set(.dt, j = "dur",  value = ifelse(.isDose, 0.0, NA_real_))

  # ---- Patch NA cells in dose rows for numeric dose fields ----
  # rbindlist fills obs-chunk rows with NA in dose-only columns.
  # That's correct for obs rows. But if any DOSE rows ended up with NA
  # (e.g., from a sparse dose chunk), fill with 0.
  .doseIdx <- which(.dt$evid != 0L)
  if (length(.doseIdx) > 0L) {
    for (.col in c("rate", "ii", "dur")) {
      .vals <- .dt[[.col]]
      .naInDose <- is.na(.vals[.doseIdx])
      if (any(.naInDose)) {
        .vals[.doseIdx[.naInDose]] <- 0.0
        data.table::set(.dt, j = .col, value = .vals)
      }
    }
    for (.col in c("addl", "ss")) {
      .vals <- .dt[[.col]]
      .naInDose <- is.na(.vals[.doseIdx])
      if (any(.naInDose)) {
        .vals[.doseIdx[.naInDose]] <- 0L
        data.table::set(.dt, j = .col, value = .vals)
      }
    }
  }

  # ---- Sort: id ASC, time ASC, evid=3 before others at same time ----
  # Map evid=3 -> -1 so it sorts first within same (id, time)
  .evidSort <- ifelse(.dt$evid == 3L, -1L, as.integer(.dt$evid))
  .ord <- .order3(.dt$id, .dt$time, .evidSort)
  .dt <- .dt[.ord, ]

  # ---- Ensure canonical column order ----
  .missing <- setdiff(.etColOrder, names(.dt))
  for (.col in .missing) data.table::set(.dt, j = .col, value = NA)
  data.table::setcolorder(.dt, .etColOrder)

  as.data.frame(.dt)
}

#' Check if object is an rxEt event table
#' @param x object to test
#' @return logical
#' @export
#' Build an observation chunk list
#'
#' @param time numeric vector of sample times, OR a list of c(low,high)
#'   or c(low,mid,high) windows.
#' @param cmt compartment name or number (optional)
#' @param id integer vector of subject IDs (optional)
#' @return named list (sparse — only columns that matter)
#' @noRd
.etObsChunk <- function(time, cmt = NULL, id = NULL) {
  .chunk <- list(evid = 0L)

  if (inherits(time, "list")) {
    # Window specification: each element is c(low,high) or c(low,mid,high)
    .nw <- length(time)
    .low  <- numeric(.nw)
    .mid  <- numeric(.nw)
    .high <- numeric(.nw)
    for (.i in seq_len(.nw)) {
      .w <- time[[.i]]
      if (length(.w) == 2L) {
        if (.w[1] > .w[2]) stop("window bounds must be ordered c(low, high)", call. = FALSE)
        .low[.i]  <- .w[1]
        .mid[.i]  <- (.w[1] + .w[2]) / 2
        .high[.i] <- .w[2]
      } else if (length(.w) == 3L) {
        if (.w[1] > .w[2] || .w[2] > .w[3]) stop("window bounds must be ordered c(low, mid, high)", call. = FALSE)
        .low[.i]  <- .w[1]
        .mid[.i]  <- .w[2]
        .high[.i] <- .w[3]
      } else {
        stop("each window must be c(low, high) or c(low, mid, high)", call. = FALSE)
      }
    }
    .chunk$time <- .mid
    .chunk$low  <- .low
    .chunk$high <- .high
  } else {
    .chunk$time <- as.numeric(time)
  }

  if (!is.null(cmt)) .chunk$cmt <- cmt
  if (!is.null(id))  .chunk$id  <- as.integer(id)

  .chunk
}

#' Build a dosing chunk list
#'
#' Handles both the amt/time interface and the legacy
#' nbr.doses/dosing.interval/start.time interface.
#'
#' @param time numeric start time(s)
#' @param amt numeric dose amount(s)
#' @param evid integer event ID (default 1L)
#' @param cmt compartment name/number (default "(default)")
#' @param ii inter-dose interval (default 0)
#' @param addl additional doses (default 0L)
#' @param ss steady-state flag (default 0L)
#' @param rate infusion rate; -1=modeled rate, -2=modeled duration (default 0)
#' @param dur infusion duration; if >0 and rate==0, rate is computed as amt/dur
#' @param until time of last dose; overrides addl
#' @param nbr.doses number of doses (legacy); sets addl = nbr.doses - 1
#' @param dosing.interval inter-dose interval (legacy alias for ii)
#' @param id integer subject IDs (optional)
#' @return named list, or data.frame if multiple rows needed
#' @noRd
.etDoseChunk <- function(time = 0, amt, evid = 1L, cmt = "(default)",
                          ii = 0.0, addl = 0L, ss = 0L, rate = 0.0, dur = 0.0,
                          until = NULL, nbr.doses = NULL, dosing.interval = NULL,
                          id = NULL) {
  # Legacy nbr.doses/dosing.interval interface
  if (!is.null(dosing.interval)) ii <- as.numeric(dosing.interval)
  if (!is.null(nbr.doses)) {
    addl <- as.integer(nbr.doses) - 1L
  }

  # until → compute addl from (until - time) / ii
  if (!is.null(until)) {
    if (ii <= 0) stop("'until' requires a positive 'ii'", call. = FALSE)
    addl <- as.integer(round((until - time) / ii))
  }

  # dur → convert to rate (amt/dur); clear dur
  if (dur > 0 && rate == 0.0) {
    rate <- amt / dur
    dur  <- 0.0
  }

  # Handle vector amt/time (multiple dose records) → return data.frame
  if (length(amt) > 1L || length(time) > 1L) {
    if (length(time) == 1L) time <- rep(time, length(amt))
    if (length(amt)  == 1L) amt  <- rep(amt,  length(time))
    if (length(time) != length(amt)) {
      stop("'time' and 'amt' must have the same length", call. = FALSE)
    }
    .chunk <- data.frame(
      time = as.numeric(time), amt  = as.numeric(amt),
      evid = as.integer(evid), cmt  = as.character(cmt),
      ii   = as.numeric(ii),   addl = as.integer(addl),
      ss   = as.integer(ss),   rate = as.numeric(rate),
      dur  = as.numeric(dur),
      stringsAsFactors = FALSE
    )
    if (!is.null(id)) .chunk$id <- as.integer(id)
    return(.chunk)
  }

  # Single dose record — sparse named list
  .chunk <- list(
    time = as.numeric(time),
    amt  = as.numeric(amt),
    evid = as.integer(evid),
    cmt  = as.character(cmt),
    ii   = as.numeric(ii),
    addl = as.integer(addl),
    ss   = as.integer(ss),
    rate = as.numeric(rate),
    dur  = as.numeric(dur)
  )
  if (!is.null(id)) .chunk$id <- as.integer(id)
  .chunk
}

is.rxEt <- function(x) {
  if (!inherits(x, "rxEt")) return(FALSE)
  # New-style: list with .env environment (pure-R rewrite)
  .env <- .subset2(x, ".env")
  if (is.environment(.env)) return(TRUE)
  # Old-style: data.frame-based rxEt (C++ et_() output, has .rxode2.lst in class attr)
  !is.null(attr(class(x), ".rxode2.lst"))
}
