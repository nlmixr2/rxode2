
#' Extract mutable .env from either new-style (data.frame subclass + .rxEtEnv attr)
#' or internal mini-rxEt (1-element named list where [[1L]] is the env).
#' @noRd
.rxEtEnv <- function(x) {
  .e <- attr(x, ".rxEtEnv", exact = TRUE)
  if (!is.null(.e)) return(.e)
  .ux <- unclass(x)
  if (is.list(.ux) && ".env" %in% names(.ux)) return(.ux[[".env"]])
  if (length(.ux) >= 1L && is.environment(.ux[[1L]])) return(.ux[[1L]])
  NULL
}

# Default show flags (controls which columns print/as.data.frame expose)
.etDefaultShow <- function() {
  c(id = FALSE, low = FALSE, time = TRUE, high = FALSE, cmt = FALSE,
    amt = FALSE, rate = FALSE, ii = FALSE, addl = FALSE, evid = TRUE,
    ss = FALSE, dur = FALSE)
}

#' Add rows of a data.frame to the ID-indexed chunks list
#'
#' Assigns id column and appends to \code{chunks[[id]]} for each ID in
#' .ids.
#' @param .envRef mutable env from rxEt
#' @param .df data.frame of rows to add (no 'id' column yet)
#' @param .ids integer vector of IDs; NULL/empty defaults to 1L
#' @noRd
.etAddChunk <- function(.envRef, .df, .ids = NULL) {
  .rt <- attr(.df, ".randomType")
  if (!is.null(.rt) && !is.na(.rt)) {
    if (is.na(.envRef$randomType) || .rt > .envRef$randomType)
      .envRef$randomType <- .rt
    if (!isTRUE(.envRef$show["low"])) {
      .envRef$show["low"]  <- TRUE
      .envRef$show["high"] <- TRUE
    }
  }
  if (!is.data.frame(.df)) .df <- .etExpandObsChunk(.df)
  if (nrow(.df) == 0L) return(invisible(NULL))
  .ids <- if (is.null(.ids) || length(.ids) == 0L) 1L else as.integer(.ids)
  .posIds <- .ids[.ids > 0L]
  if (length(.posIds) == 0L) return(invisible(NULL))
  for (.i in .posIds) {
    .row <- .df
    .row$id <- .i
    .existing <- if (.i <= length(.envRef$chunks)) .envRef$chunks[[.i]] else NULL
    .envRef$chunks[[.i]] <- as.data.frame(data.table::rbindlist(list(.existing, .row), fill = TRUE))
  }
  invisible(NULL)
}

#' Merge a data.frame (with id column) into an ID-indexed chunks list
#'
#' Used by etSeq/etRbind when accumulating materialized data.frames.
#' @param .chunks list indexed by ID integer value
#' @param .df data.frame with 'id' column already set
#' @return updated .chunks
#' @noRd
.addRowsToChunks <- function(.chunks, .df) {
  if (nrow(.df) == 0L) return(.chunks)
  .ids <- unique(as.integer(.df$id))
  for (.i in .ids) {
    .rows <- .df[.df$id == .i, , drop = FALSE]
    .existing <- if (.i <= length(.chunks)) .chunks[[.i]] else NULL
    .chunks[[.i]] <- as.data.frame(data.table::rbindlist(list(.existing, .rows), fill = TRUE))
  }
  .chunks
}

#' Attach method closures to an rxEt's list structure
#'
#' Each method captures .env by reference so mutations are shared.
#' @param .env environment (mutable state)
#' @return named list of closures
#' @noRd
.etBuildMethods <- function(.env) {
  .lst <- list(
    add.dosing = function(dose, nbr.doses = 1L, dosing.interval = 24,
                           dosing.to = 1L, rate = NULL,
                           amount.units = NA_character_,
                           start.time = 0.0, do.sampling = FALSE,
                           time.units = NA_character_,
                           evid = NULL, strt.time = NULL) {
      if (!is.null(strt.time)) start.time <- strt.time
      .cmt <- if (dosing.to == 1L) "(default)" else as.integer(dosing.to)
      .evidVal <- if (!is.null(evid)) as.integer(evid) else 1L
      .df <- .etDoseChunk(
        time = start.time, amt = dose, cmt = .cmt,
        evid = .evidVal,
        ii   = if (nbr.doses > 1L) dosing.interval else 0.0,
        addl = as.integer(nbr.doses) - 1L,
        rate = if (!is.null(rate)) rate else 0.0
      )
      .etAddChunk(.env, .df, .env$IDs)
      .env$ndose  <- .env$ndose + length(.env$IDs)
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
      .df <- .etObsChunk(time)
      .etAddChunk(.env, .df, .env$IDs)
      .env$nobs   <- .env$nobs + length(.df$time) * length(.env$IDs)
      if (!is.na(time.units)) .env$units["time"] <- time.units
      invisible(NULL)
    },

    get.units = function() .env$units,
    getUnits  = function() .env$units,

    get.nobs  = function() .env$nobs,
    get.EventTable = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      if (nrow(.mat) == 0L) return(NULL)
      .show <- .env$show
      .mat[, names(.show)[.show], drop = FALSE]
    },
    get.obs.rec = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      .mat$evid == 0L
    },
    get.dosing = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      if (nrow(.mat) == 0L) return(NULL)
      .d <- .mat[.mat$evid != 0L, , drop = FALSE]
      if (nrow(.d) == 0L) NULL else .d
    },
    get.sampling = function() {
      .mat <- .etMaterialize(structure(list(.env = .env), class = "rxEt"))
      if (nrow(.mat) == 0L) return(NULL)
      .s <- .mat[.mat$evid == 0L, , drop = FALSE]
      if (nrow(.s) == 0L) NULL else .s
    },
    clear.sampling = function() {
      for (.i in seq_along(.env$chunks)) {
        if (!is.null(.env$chunks[[.i]])) {
          .df <- .env$chunks[[.i]]
          .df <- .df[.df$evid != 0L, , drop = FALSE]
          if (nrow(.df) == 0L) .env$chunks[.i] <- list(NULL) else .env$chunks[[.i]] <- .df
        }
      }
      .env$nobs <- 0L
      invisible(NULL)
    },
    clear.dosing = function() {
      for (.i in seq_along(.env$chunks)) {
        if (!is.null(.env$chunks[[.i]])) {
          .df <- .env$chunks[[.i]]
          .df <- .df[.df$evid == 0L, , drop = FALSE]
          if (nrow(.df) == 0L) .env$chunks[.i] <- list(NULL) else .env$chunks[[.i]] <- .df
        }
      }
      .env$ndose <- 0L
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
      .newEnv$methods <- .etBuildMethods(.newEnv)
      .cp <- list()
      attr(.cp, "names")     <- character(0)
      attr(.cp, "class")     <- c("rxEt", "data.frame")
      attr(.cp, "row.names") <- integer(0)
      attr(.cp, ".rxEtEnv")  <- .newEnv
      .cp
    },
    import.EventTable = function(df) {
      if (!is.data.frame(df)) stop("'df' must be a data.frame", call. = FALSE)
      .tu <- .env$units["time"]
      .du <- .env$units["dosing"]
      .hasTimeU <- !is.na(.tu) && nchar(.tu) > 0
      .hasDoseU <- !is.na(.du) && nchar(.du) > 0
      # Save prior-units state: only convert when et already had units set
      .priorTimeU <- .hasTimeU
      .priorDoseU <- .hasDoseU
      # Auto-detect units from columns if not already set
      if (!.hasTimeU && requireNamespace("units", quietly = TRUE)) {
        for (.nmCheck in c("time", "ii")) {
          if (!is.null(df[[.nmCheck]]) && inherits(df[[.nmCheck]], "units")) {
            .tu <- units::deparse_unit(df[[.nmCheck]])
            .env$units["time"] <- .tu
            .hasTimeU <- TRUE
            break
          }
        }
      }
      if (!.hasDoseU && requireNamespace("units", quietly = TRUE)) {
        if (!is.null(df[["amt"]]) && inherits(df[["amt"]], "units")) {
          .du <- units::deparse_unit(df[["amt"]])
          .env$units["dosing"] <- .du
          .hasDoseU <- TRUE
        } else if (!is.null(df[["rate"]]) && inherits(df[["rate"]], "units")) {
          # udunits format: "ug s-1" — amount unit has no digit exponent
          .rateStr <- units::deparse_unit(df[["rate"]])
          .parts <- strsplit(trimws(.rateStr), "\\s+")[[1L]]
          .amtParts <- .parts[!grepl("[0-9]", .parts)]
          if (length(.amtParts) >= 1L) {
            .du <- .amtParts[[1L]]
            .env$units["dosing"] <- .du
            .hasDoseU <- TRUE
          }
        }
      }
      .cols <- lapply(names(df), function(.nm) {
        .col <- df[[.nm]]
        if (!inherits(.col, "units")) return(.col)
        if (requireNamespace("units", quietly = TRUE)) {
          # Only convert when et had prior units; auto-detected units just strip label
          if (.nm %in% c("time", "ii") && .hasTimeU)
            return(as.numeric(units::set_units(.col, .tu, mode = "standard")))
          if (.nm == "amt" && .hasDoseU)
            return(as.numeric(units::set_units(.col, .du, mode = "standard")))
          if (.nm == "rate" && .hasDoseU && .hasTimeU) {
            .rateU <- paste0(.du, "/", .tu)
            return(as.numeric(units::set_units(.col, .rateU, mode = "standard")))
          }
        }
        as.numeric(.col)
      })
      names(.cols) <- names(df)
      df <- as.data.frame(.cols, stringsAsFactors = FALSE)
      if (is.null(df$evid)) {
        df$evid <- if (!is.null(df$amt)) ifelse(!is.na(df$amt) & as.numeric(df$amt) != 0, 1L, 0L) else 0L
      }
      df$evid <- as.integer(df$evid)
      if (is.null(df$id)) df$id <- 1L
      df$id <- as.integer(df$id)
      .env$IDs  <- sort(unique(df$id))
      .env$nobs  <- .env$nobs  + sum(df$evid == 0L, na.rm = TRUE)
      .env$ndose <- .env$ndose + sum(df$evid != 0L, na.rm = TRUE)
      .env$chunks <- .addRowsToChunks(.env$chunks, df)
      if (length(.env$IDs) > 1L) .env$show["id"] <- TRUE
      if (sum(df$evid != 0L, na.rm = TRUE) > 0L) .env$show["amt"] <- TRUE
      if (!is.null(df$rate) && any(df$rate[df$evid != 0L] != 0, na.rm = TRUE))
        .env$show["rate"] <- TRUE
      if (!is.null(df$dur) && any(df$dur[df$evid != 0L] != 0, na.rm = TRUE))
        .env$show["dur"] <- TRUE
      if (!is.null(df$ii) && any(df$ii != 0, na.rm = TRUE)) {
        .env$show["ii"] <- TRUE
        .env$show["addl"] <- TRUE
      }
      invisible(NULL)
    },

    importEventTable = function(df, ...) {
      if (!is.data.frame(df)) stop("'df' must be a data.frame", call. = FALSE)
      # Normalize UPPERCASE/mixed-case NONMEM-style column names to lowercase
      .colMap <- c(ID="id", TIME="time", CMT="cmt", AMT="amt", EVID="evid",
                   RATE="rate", II="ii", ADDL="addl", SS="ss", DUR="dur",
                   LOW="low", HIGH="high")
      .nms <- names(df)
      .upper <- toupper(.nms)
      for (.i in seq_along(.nms)) {
        if (.upper[.i] %in% names(.colMap) && .nms[.i] != .colMap[.upper[.i]]) {
          names(df)[.i] <- .colMap[.upper[.i]]
        }
      }
      # Convert character ID to sequential integers (silently)
      if (!is.null(df$id) && !is.numeric(df$id) && !is.integer(df$id)) {
        .uniq <- unique(df$id)
        df$id <- as.integer(factor(df$id, levels = .uniq))
      }
      # Drop rows with NA time (warn)
      if (!is.null(df$time) && any(is.na(as.numeric(df$time)))) {
        .nDrop <- sum(is.na(as.numeric(df$time)))
        warning(sprintf("dropping %d row(s) with NA time", .nDrop), call. = FALSE)
        df <- df[!is.na(as.numeric(df$time)), , drop = FALSE]
      }
      # Delegate to import.EventTable for units handling + storage
      .etBuildMethods(.env)$import.EventTable(df)
      invisible(NULL)
    },

    expand = function() {
      .et <- structure(list(.env = .env), class = "rxEt")
      .mat <- .etMaterialize(.et)
      .expanded <- .etExpandAddlR(.mat)
      .env$chunks <- list()
      if (nrow(.expanded) > 0L) {
        .ids <- unique(as.integer(.expanded$id))
        for (.i in .ids) {
          .env$chunks[[.i]] <- .expanded[.expanded$id == .i, , drop = FALSE]
        }
      }
      invisible(NULL)
    },
    simulate = function(object, nsim = 1, seed = NULL, ...) {
      if (!is.null(seed)) set.seed(seed)
      .et <- structure(list(.env = .env), class = "rxEt")
      .mat <- .etMaterialize(.et)
      .hasWin <- !is.na(.mat$low) & !is.na(.mat$high) & .mat$evid == 0L
      if (!any(.hasWin)) {
        warning("simulating event table without windows returns identical event table", call. = FALSE)
      } else {
        .mat$time[.hasWin] <- stats::runif(sum(.hasWin), .mat$low[.hasWin], .mat$high[.hasWin])
        .env$chunks <- list()
        if (nrow(.mat) > 0L) {
          .ids <- unique(as.integer(.mat$id))
          for (.i in .ids) .env$chunks[[.i]] <- .mat[.mat$id == .i, , drop = FALSE]
        }
      }
      invisible(NULL)
    }
  )
  .lst$addDosing      <- .lst[["add.dosing"]]
  .lst$add_dosing     <- .lst[["add.dosing"]]
  .lst$addSampling    <- .lst[["add.sampling"]]
  .lst$add_sampling   <- .lst[["add.sampling"]]
  .lst$getDosing      <- .lst[["get.dosing"]]
  .lst$getSampling    <- .lst[["get.sampling"]]
  .lst$getEventTable  <- .lst[["get.EventTable"]]
  .lst$clearDosing    <- .lst[["clear.dosing"]]
  .lst$clear_dosing   <- .lst[["clear.dosing"]]
  .lst$clearSampling  <- .lst[["clear.sampling"]]
  .lst$clear_sampling <- .lst[["clear.sampling"]]
  .lst$get_units      <- .lst[["get.units"]]
  .lst
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
  .env$methods <- .etBuildMethods(.env)
  .obj <- list(
    id = integer(0), low = numeric(0), time = numeric(0), high = numeric(0),
    cmt = integer(0), amt = numeric(0), rate = numeric(0), ii = numeric(0),
    addl = integer(0), evid = integer(0), ss = integer(0), dur = numeric(0)
  )
  attr(.obj, "class")     <- c("rxEt", "data.frame")
  attr(.obj, "row.names") <- integer(0)
  attr(.obj, ".rxEtEnv")  <- .env
  .obj
}

#' Sync the materialized data.frame shell with the mutable rxEt environment
#' @noRd
.rxEtSyncData <- function(x) {
  .env <- .rxEtEnv(x)
  if (!is.environment(.env)) return(x)
  .ret <- .etMaterialize(x)
  attr(.ret, ".rxEtEnv") <- .env
  .rt <- .env$randomType
  .cls <- c("rxEt", "data.frame")
  if (!is.null(.rt) && !is.na(.rt)) {
    .cls <- structure(.cls, ".rxode2.lst" = list(randomType = as.integer(.rt)))
  }
  class(.ret) <- .cls
  .ret
}

#' Stamp randomType into attr(class(et), ".rxode2.lst") before returning
#' @noRd
.rxEtFinalize <- function(et) {
  .rxEtSyncData(et)
}

#' Expand addl doses into individual records (pure R)
#' @param df materialized data.frame from .etMaterialize()
#' @return data.frame with addl expanded into individual dose records
#' @noRd
.etExpandAddlR <- function(df) {
  .doseRows <- df[df$evid != 0L & df$addl > 0L, , drop = FALSE]
  if (nrow(.doseRows) == 0L) return(df)

  .extras <- vector("list", nrow(.doseRows))
  for (.i in seq_len(nrow(.doseRows))) {
    .row   <- .doseRows[.i, ]
    .n     <- .row$addl
    .extra <- .row[rep(1L, .n), , drop = FALSE]
    .extra$time <- .row$time + seq_len(.n) * .row$ii
    .extra$addl <- 0L
    .extra$ii   <- 0.0
    .extras[[.i]] <- .extra
  }
  df$addl[df$evid != 0L] <- 0L
  df$ii[df$evid   != 0L] <- 0.0
  .combined <- do.call(rbind, c(list(df), .extras))
  .evidSort <- ifelse(.combined$evid == 3L, -1L, as.integer(.combined$evid))
  .combined[.order3(.combined$id, .combined$time, .evidSort), ]
}

# Canonical column order matching C++ etEmpty()
.etColOrder <- c("id", "low", "time", "high", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur")

#' Convert character cmt column to integer for C++ solver
#'
#' The C++ etTran code handles integer cmt as direct compartment indices,
#' but character cmt values are looked up as compartment NAMES. Numeric
#' strings like "2" do NOT match compartment names ("amt2"), so they get
#' assigned as extra compartments beyond state.size(). This function converts
#' all-convertible character cmt columns to integer before passing to C++.
#'
#' "(default)" and "(obs)" map to compartment 1; NA stays NA_integer_.
#' If any value is a non-numeric, non-sentinel string (a compartment name),
#' the column stays character so C++ name lookup works normally.
#' @param .df materialized data.frame from .etMaterialize()
#' @return .df with cmt column possibly converted to integer
#' @noRd
.etFixCmtForSolve <- function(.df) {
  .cmt <- .df[["cmt"]]
  if (is.null(.cmt) || !is.character(.cmt)) return(.df)
  .isNA      <- is.na(.cmt)
  .isSentinel <- !.isNA & (.cmt == "(default)" | .cmt == "(obs)")
  .numericOk  <- !.isNA & !.isSentinel & suppressWarnings(!is.na(as.integer(.cmt)))
  if (all(.isNA | .isSentinel | .numericOk)) {
    .int <- integer(length(.cmt))
    .int[.isNA]       <- NA_integer_
    .int[.isSentinel] <- 1L
    .int[.numericOk]  <- as.integer(.cmt[.numericOk])
    .df[["cmt"]] <- .int
  }
  .df
}

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
#'
#' chunks is a list indexed by ID integer value; \code{chunks[[i]]} is
#' a data.frame for ID i (with 'id' column set), or NULL if no records
#' for that ID.
#' @param et rxEt object
#' @return data.frame with canonical columns, sorted by (id, time,
#'   evid-adjusted)
#' @noRd
.etMaterialize <- function(et) {
  .env <- .rxEtEnv(et)
  .chunks <- .env$chunks

  if (length(.chunks) == 0L) return(.etEmptyDf())

  # Each element is a data.frame for one ID; filter NULL entries
  .nonNull <- Filter(Negate(is.null), .chunks)
  if (length(.nonNull) == 0L) return(.etEmptyDf())

  # rbindlist: fast sparse bind across ID data.frames
  .dt <- data.table::rbindlist(.nonNull, fill = TRUE, use.names = TRUE)

  # ---- Fill column defaults ----
  if (is.null(.dt[["id"]]))   data.table::set(.dt, j = "id",   value = 1L)
  if (is.null(.dt[["evid"]])) data.table::set(.dt, j = "evid", value = 0L)
  if (is.null(.dt[["low"]]))  data.table::set(.dt, j = "low",  value = NA_real_)
  if (is.null(.dt[["high"]])) data.table::set(.dt, j = "high", value = NA_real_)
  if (is.null(.dt[["cmt"]]))  data.table::set(.dt, j = "cmt",  value = "(default)")

  .isDose <- .dt$evid != 0L
  if (is.null(.dt[["amt"]]))  data.table::set(.dt, j = "amt",  value = NA_real_)
  if (is.null(.dt[["rate"]])) data.table::set(.dt, j = "rate", value = ifelse(.isDose, 0.0, NA_real_))
  if (is.null(.dt[["ii"]]))   data.table::set(.dt, j = "ii",   value = ifelse(.isDose, 0.0, NA_real_))
  if (is.null(.dt[["addl"]])) data.table::set(.dt, j = "addl", value = ifelse(.isDose, 0L,  NA_integer_))
  if (is.null(.dt[["ss"]]))   data.table::set(.dt, j = "ss",   value = ifelse(.isDose, 0L,  NA_integer_))
  if (is.null(.dt[["dur"]]))  data.table::set(.dt, j = "dur",  value = ifelse(.isDose, 0.0, NA_real_))

  # Patch NA cells in dose rows for numeric dose fields
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

  # Sort: id ASC, time ASC, evid=3 sorts before others at same (id, time)
  .evidSort <- ifelse(.dt$evid == 3L, -1L, as.integer(.dt$evid))
  .ord <- .order3(.dt$id, .dt$time, .evidSort)
  .dt <- .dt[.ord, ]

  # Ensure canonical column order
  .missing <- setdiff(.etColOrder, names(.dt))
  for (.col in .missing) data.table::set(.dt, j = .col, value = NA)
  data.table::setcolorder(.dt, .etColOrder)

  .df <- as.data.frame(.dt)

  # Apply units to columns so C++ solver can propagate them to output
  .tu <- .env$units["time"]
  .du <- .env$units["dosing"]
  .hasTimeU <- !is.na(.tu) && nchar(.tu) > 0
  .hasDoseU <- !is.na(.du) && nchar(.du) > 0
  if (.hasTimeU || .hasDoseU) {
    if (.hasTimeU) {
      for (.col in c("time", "ii", "low", "high", "dur")) {
        .df[[.col]] <- units::set_units(.df[[.col]], .tu, mode = "standard")
      }
    }
    if (.hasDoseU) {
      .df[["amt"]] <- units::set_units(.df[["amt"]], .du, mode = "standard")
      if (.hasTimeU) {
        .df[["rate"]] <- units::set_units(.df[["rate"]], paste0(.du, "/", .tu), mode = "standard")
      }
    }
  }

  .df
}

#' Check if object is an rxEt event table
#' @param x object to test
#' @return logical
#' @export
is.rxEt <- function(x) {
  if (!inherits(x, "rxEt")) return(FALSE)
  .env <- .rxEtEnv(x)
  is.environment(.env)
}

#' Build an observation chunk sparse list (no id column; caller uses .etAddChunk)
#'
#' Returns a sparse named list where scalar fields (evid) are stored once and
#' vector fields (time) hold per-row values. Callers use length(.df$time) for
#' row count, not nrow().
#'
#' @param time numeric vector of sample times, OR a list of c(low,high)
#'   or c(low,mid,high) windows.
#' @param cmt compartment name or number (optional scalar)
#' @param id integer vector of IDs to attach (optional)
#' @return sparse named list with evid=0L scalar and time vector
#' @noRd
.etObsChunk <- function(time, cmt = NULL, id = NULL) {
  .randomType <- NA_integer_
  if (inherits(time, "list")) {
    .nw <- length(time)
    .low  <- numeric(.nw)
    .mid  <- numeric(.nw)
    .high <- numeric(.nw)
    .hasNaHw <- any(vapply(time, function(.w) length(.w) == 3L && is.na(.w[3L]), logical(1L)))
    .has3    <- FALSE
    for (.i in seq_len(.nw)) {
      .w <- time[[.i]]
      if (length(.w) == 2L) {
        if (.hasNaHw) {
          if (.w[2L] < 0) stop("window half-width must be non-negative", call. = FALSE)
          .low[.i]  <- .w[1L] - .w[2L]
          .mid[.i]  <- .w[1L]
          .high[.i] <- .w[1L] + .w[2L]
        } else {
          if (.w[1] > .w[2]) stop("window bounds must be ordered c(low, high)", call. = FALSE)
          .low[.i]  <- .w[1]
          .mid[.i]  <- (.w[1] + .w[2]) / 2
          .high[.i] <- .w[2]
        }
      } else if (length(.w) == 3L) {
        if (is.na(.w[3L])) {
          if (.w[2L] < 0) stop("window half-width must be non-negative", call. = FALSE)
          .low[.i]  <- .w[1L] - .w[2L]
          .mid[.i]  <- .w[1L]
          .high[.i] <- .w[1L] + .w[2L]
        } else {
          .has3 <- TRUE
          if (.w[1] > .w[2] || .w[2] > .w[3]) stop("window bounds must be ordered c(low, mid, high)", call. = FALSE)
          .low[.i]  <- .w[1]
          .mid[.i]  <- .w[2]
          .high[.i] <- .w[3]
        }
      } else {
        stop("each window must be c(low, high) or c(low, mid, high)", call. = FALSE)
      }
    }
    .randomType <- if (.hasNaHw) 3L else if (.has3) 1L else 2L
    .df <- list(evid = 0L, time = .mid, low = .low, high = .high)
  } else {
    .df <- list(evid = 0L, time = as.numeric(time))
  }
  if (!is.null(cmt)) .df$cmt <- cmt
  if (!is.null(id))  .df$id  <- as.integer(id)
  attr(.df, ".randomType") <- .randomType
  .df
}

#' Expand a sparse obs chunk list to a full data.frame
#'
#' Replicates scalar fields to match the length of the time vector.
#' @param .sparse sparse list from .etObsChunk
#' @return data.frame with all columns same length
#' @noRd
.etExpandObsChunk <- function(.sparse) {
  .n <- length(.sparse$time)
  as.data.frame(
    lapply(.sparse, function(.v) if (length(.v) == 1L) rep(.v, .n) else .v),
    stringsAsFactors = FALSE
  )
}

#' Build a dosing chunk data.frame (no id column; caller uses .etAddChunk)
#'
#' @param time numeric start time(s)
#' @param amt numeric dose amount(s)
#' @param evid integer event ID (default 1L)
#' @param cmt compartment name/number (default "(default)")
#' @param ii inter-dose interval (default 0)
#' @param addl additional doses (default 0L)
#' @param ss steady-state flag (default 0L)
#' @param rate infusion rate (default 0)
#' @param dur infusion duration (default 0.0)
#' @param until time of last dose; overrides addl
#' @param nbr.doses number of doses (legacy); sets addl = nbr.doses - 1
#' @param dosing.interval inter-dose interval (legacy alias for ii)
#' @return data.frame
#' @noRd
.etDoseChunk <- function(time = 0, amt, evid = 1L, cmt = "(default)",
                          ii = 0.0, addl = 0L, ss = 0L, rate = 0.0, dur = 0.0,
                          until = NULL, nbr.doses = NULL, dosing.interval = NULL) {
  if (!is.null(dosing.interval)) ii <- as.numeric(dosing.interval)
  if (!is.null(nbr.doses))       addl <- as.integer(nbr.doses) - 1L

  if (!is.null(until)) {
    if (ii <= 0) stop("'until' requires a positive 'ii'", call. = FALSE)
    addl <- as.integer(floor((until - time) / ii))
    if (addl < 0L) {
      warning("'until' is before 'time'; setting addl=0", call. = FALSE)
      addl <- 0L
    }
  }

  if (addl > 0L && ii == 0.0)
    stop("'addl' > 0 requires a positive inter-dose interval ('ii')", call. = FALSE)

  if (ss > 0L) {
    if (rate < -1.0 && ii == 0.0)
      stop("cannot use duration flag (rate=-2) with steady-state dosing", call. = FALSE)
    if (ss == 2L && ii == 0.0)
      stop("ss=2 requires a positive inter-dose interval ('ii')", call. = FALSE)
    if (rate > 0 && ii > 0 && amt == 0)
      stop("cannot combine constant infusion (rate>0) with dose interval (ii>0) for steady-state; use ii=0 for constant infusion SS", call. = FALSE)
  }

  if (is.list(time)) time <- vapply(time, function(.w) as.numeric(.w[1L]), numeric(1L))

  if (length(amt) > 1L || length(time) > 1L) {
    if (length(time) == 1L) time <- rep(time, length(amt))
    if (length(amt)  == 1L) amt  <- rep(amt,  length(time))
    if (length(time) != length(amt)) stop("'time' and 'amt' must have the same length", call. = FALSE)
  }

  data.frame(
    time = as.numeric(time), amt  = as.numeric(amt),
    evid = as.integer(evid), cmt  = as.character(cmt),
    ii   = as.numeric(ii),   addl = as.integer(addl),
    ss   = as.integer(ss),   rate = as.numeric(rate),
    dur  = as.numeric(dur),
    stringsAsFactors = FALSE
  )
}

#' Rebuild an rxEt shell after data-frame style mutation
#' @noRd
.rxEtRebuildShell <- function(x, df) {
  .env0 <- .rxEtEnv(x)
  .et <- .newRxEt(amountUnits = .env0$units["dosing"], timeUnits = .env0$units["time"])
  .env <- .rxEtEnv(.et)
  if (nrow(df) > 0L) {
    .env$methods$import.EventTable(df)
  }
  .env$show[names(.env0$show)] <- .env$show[names(.env0$show)] | .env0$show
  .env$randomType <- .env0$randomType
  .env$canResize <- .env0$canResize
  .et
}
