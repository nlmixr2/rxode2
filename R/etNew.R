#' Extract mutable .env from either new-style (data.frame subclass + .rxEtEnv attr)
#' or internal mini-rxEt (1-element named list where [[1L]] is the env).
#'
#' @param x object to extract data from
#'
#' @return environment or NULL if not found
#'
#' @noRd
.rxEtEnv <- function(x) {
  .e <- attr(x, ".rxEtEnv", exact = TRUE)
  if (!is.null(.e)) return(.e)
  .ux <- unclass(x)
  if (is.list(.ux) && ".env" %in% names(.ux)) return(.ux[[".env"]])
  if (length(.ux) >= 1L && is.environment(.ux[[1L]])) return(.ux[[1L]])
  NULL
}

#' Default show flags (controls which columns print/as.data.frame expose)
#'
#' This is kept here so that it can be specified once and referred to
#' by multiple locations.
#'
#' @return named logical vector of default show flags
#'
#' @noRd
.etDefaultShow <- function() {
  c(id = FALSE, low = FALSE, time = TRUE, high = FALSE, cmt = FALSE,
    amt = FALSE, rate = FALSE, ii = FALSE, addl = FALSE, evid = TRUE,
    ss = FALSE, dur = FALSE)
}
#' Drop the units for the check
#'
#' @param df NULL, data.frame or non-data frame object.
#'   from for internal use
#'
#' @return this function returns NULL or the non-materialized data, or
#'   the data.frame with units columns converted to numeric for
#'   internal use
#'
#' @noRd
#' @author Matthew L. Fidler
.etDropUnitsForChunk <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }
  for (.nm in names(df)) {
    if (inherits(df[[.nm]], "units")) {
      df[[.nm]] <- as.numeric(df[[.nm]])
    }
  }
  df
}

#' Add rows of a data.frame to the ID-indexed chunks list
#'
#' Assigns id column and appends to \code{chunks[[id]]} for each ID in
#' .ids.
#'
#' @param .envRef mutable env from rxEt
#'
#' @param .df data.frame of rows to add (no 'id' column yet)
#'
#' @param .ids integer vector of ids; NULL/empty defaults to 1L
#'
#' @noRd
.etAddChunk <- function(envRef, df, ids = NULL) {
  .rt <- attr(df, ".randomType")
  if (!is.null(.rt) && !is.na(.rt)) {
    if (is.na(envRef$randomType) || .rt > envRef$randomType)
      envRef$randomType <- .rt
    if (!isTRUE(envRef$show["low"])) {
      envRef$show["low"]  <- TRUE
      envRef$show["high"] <- TRUE
    }
  }
  if (!is.data.frame(df)) {
    df <- .etExpandObsChunk(df)
  }
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }
  if (is.null(ids) || length(ids) == 0L) {
    ids <-  1L
  } else {
    ids <- as.integer(ids)
  }
  .posIds <- ids[ids > 0L]
  if (length(.posIds) == 0L) return(invisible(NULL))
  for (.i in .posIds) {
    .row <- .etDropUnitsForChunk(df)
    .row$id <- .i
    if (.i <= length(envRef$chunks)) {
      .cur <- envRef$chunks[[.i]] # units already dropped
      envRef$chunks[[.i]] <- as.data.frame(data.table::rbindlist(list(.cur, .row), fill = TRUE))
    } else {
      envRef$chunks[[.i]] <- .row
    }
  }
  invisible(NULL)
}

#' Merge a data.frame (with id column) into an ID-indexed chunks list
#'
#' Used by etSeq/etRbind when accumulating materialized data.frames.
#'
#' @param chunks list indexed by ID integer value
#'
#' @param df data.frame with 'id' column already set
#'
#' @return updated chunks
#'
#' @noRd
.addRowsToChunks <- function(chunks, df) {
  if (nrow(df) == 0L) return(chunks)
  .ids <- unique(as.integer(df$id))
  for (.i in .ids) {
    .rows <- .etDropUnitsForChunk(df[df$id == .i, , drop = FALSE])
    if (.i <= length(chunks)) {
      .cur <- chunks[[.i]] # units already dropped
      chunks[[.i]] <- as.data.frame(data.table::rbindlist(list(.cur, .rows), fill = TRUE))
    } else {
      chunks[[.i]] <- .rows
    }
  }
  chunks
}
#' This is the $add.dosing type of argument to the event tables
#'
#' @param env The metadata environment for the event table, which is
#'   modified by reference
#'
#' @param dose numeric dose amount
#'
#' @param nbr.doses number of doses (1 means just the initial dose at
#'   start.time)
#'
#' @param dosing.interval dosing interval in time units (ignored if
#'   nbr.doses is 1)
#'
#' @param dosing.to compartment number to dose to (default 1); ignored
#'   if cmt column is provided in ...
#'
#' @param rate numeric infusion rate (amount/time); if provided, dur is ignored and
#'  dur is calculated as amt/rate
#'
#' @param amount.units character dose unit, e.g. "mg"; if provided,
#'   overrides existing amount unit
#'
#' @param start.time numeric time of first dose (default 0)
#'
#' @param do.sampling logical; if TRUE, adds sampling records at each
#'   dosing time (default FALSE)
#'
#' @param time.units character time unit, e.g. "hours"; if provided,
#'   overrides existing time unit
#'
#' @param evid integer event ID to use for dosing records; if NULL,
#'   defaults to 1 for non-zero dose and 0 for zero dose
#' @param strt.time deprecated alias for start.time
#' @param ... additional arguments passed to et() when creating the dosing records
#'
#' @return nothing, called for side effect of modifying env by reference
#' @noRd
#' @author Matthew L. Fidler
.etMethodAddDosing <- function(env, dose, nbr.doses = 1L, dosing.interval = 24,
                               dosing.to = 1L, rate = NULL,
                               amount.units = NA_character_,
                               start.time = 0.0, do.sampling = FALSE,
                               time.units = NA_character_,
                               evid = NULL, strt.time = NULL, ...) {
  if (!is.null(strt.time)) start.time <- strt.time
  .et <- structure(list(env = env), class = "rxEt")
  .args <- list(
    x = .et,
    amt = dose,
    time = start.time,
    ii = if (nbr.doses > 1L) dosing.interval else 0.0,
    addl = as.integer(nbr.doses) - 1L,
    addSampling = do.sampling
  )
  if (!is.null(rate)) .args$rate <- rate
  if (!is.na(amount.units)) .args$amountUnits <- amount.units
  if (!is.na(time.units)) .args$timeUnits <- time.units
  if (!is.null(evid)) .args$evid <- evid
  .extra <- list(...)
  if (is.null(.extra$cmt) && is.null(.extra$dosing.to) &&
        !identical(dosing.to, 1L)) {
    .args$cmt <- dosing.to
  }
  .ret <- do.call(et, c(.args, .extra)) # nolint
  .retEnv <- .rxEtEnv(.ret)
  env$chunks <- .retEnv$chunks
  env$units <- .retEnv$units
  env$show <- .retEnv$show
  env$ids <- .retEnv$ids
  env$nobs <- .retEnv$nobs
  env$ndose <- .retEnv$ndose
  env$randomType <- .retEnv$randomType
  env$canResize <- .retEnv$canResize
  invisible(NULL)
}
#' $get.EventTable()
#'
#' @param env environment to get the event table
#' @return event table
#' @noRd
#' @author Matthew L. Fidler
.etMethodGetEventTable <- function(env) {
  .mat <- .etMaterialize(structure(list(env = env), class = "rxEt"))
  if (nrow(.mat) == 0L) return(NULL)
  .show <- env$show
  .ret <- .mat[, names(.show)[.show], drop = FALSE]
  rownames(.ret) <- seq_len(nrow(.ret))
  .ret
}
#' Clear dosing method
#'
#' @param env environment to clear dosing from
#' @return data frame with dosing cleared
#' @noRd
#' @author Matthew L. Fidler
.etMethodGetDosing <- function(env) {
  .mat <- .etMaterialize(structure(list(env = env), class = "rxEt"))
  if (nrow(.mat) == 0L) return(NULL)
  .d <- .etDropUnitsForChunk(.mat[.mat$evid != 0L, , drop = FALSE])
  if (nrow(.d) == 0L) {
    NULL
  } else {
    rownames(.d) <- seq_len(nrow(.d))
    .d
  }
}
#' This clears the sampling in the attached event table
#'
#' Called with $clear.sampling()
#'
#'
#' @param env environment for clearing the samples
#' @return nothing, called for side effect of modifying env by reference
#' @noRd
#' @author Matthew L. Fidler
.etMethodClearSampling <- function(env) {
  for (.i in seq_along(env$chunks)) {
    if (!is.null(env$chunks[[.i]])) {
      .df <- env$chunks[[.i]]
      .df <- .df[.df$evid != 0L, , drop = FALSE]
      if (nrow(.df) == 0L) env$chunks[.i] <- list(NULL) else env$chunks[[.i]] <- .df
    }
  }
  env$nobs <- 0L
  invisible(NULL)
}
#' Auto-detect time and dosing units from columns with units class
#'
#' @param env environment to update with detected units (modified by reference)
#' @param df data.frame to check for units columns
#'
#' @return list with detected time and dosing units and flags
#'   indicating if they were already set in env or auto-detected from
#'   columns
#' @noRd
#' @author Matthew L. Fidler
.etImportAutoUnits <- function(env, df) {
  .tu <- env$units["time"]
  .du <- env$units["dosing"]
  .hasTimeU <- !is.na(.tu) && nchar(.tu) > 0
  .hasDoseU <- !is.na(.du) && nchar(.du) > 0
  # Auto-detect units from columns if not already set
  if (requireNamespace("units", quietly = TRUE)) {
    if (!.hasTimeU) {
      for (.nmCheck in c("time", "ii")) {
        if (!is.null(df[[.nmCheck]]) && inherits(df[[.nmCheck]], "units")) {
          .tu <- units::deparse_unit(df[[.nmCheck]])
          env$units["time"] <- .tu
          .hasTimeU <- TRUE
          break
        }
      }
    }
    if (!.hasDoseU) {
      if (!is.null(df[["amt"]]) && inherits(df[["amt"]], "units")) {
        .du <- units::deparse_unit(df[["amt"]])
        env$units["dosing"] <- .du
        .hasDoseU <- TRUE
      } else if (!is.null(df[["rate"]]) && inherits(df[["rate"]], "units")) {
        # udunits format: "ug s-1" — amount unit has no digit exponent
        .rateStr <- units::deparse_unit(df[["rate"]])
        .parts <- strsplit(trimws(.rateStr), "\\s+")[[1L]]
        .amtParts <- .parts[!grepl("[0-9]", .parts)]
        if (length(.amtParts) >= 1L) {
          .du <- .amtParts[[1L]]
          env$units["dosing"] <- .du
          .hasDoseU <- TRUE
        }
      }
    }
  }
  list(tu = .tu, du = .du, hasTimeU = .hasTimeU, hasDoseU = .hasDoseU)
}
#' Import an event table from a data.frame, converting units if needed
#'
#' @param df data.frame to import; may have units class on time, ii,
#'   amt, and/or rate columns
#'
#' @param tu time unit to convert to (if df has time/ii columns with units class)
#'
#' @param du dose unit to convert to (if df has amt and/or rate columns with units class)
#'
#' @param hasTimeU logical indicating if time unit was already set in
#'   env or auto-detected from df
#'
#' @param hasDoseU logical indicating if dose unit was already set in
#'   env or auto-detected from df
#'
#' @return data.frame with units columns converted to numeric and time/amt/rate
#'
#' @noRd
#' @author Matthew L. Fidler
.etImportConvertUnits <- function(df, tu, du, hasTimeU, hasDoseU) {
  .cols <- lapply(names(df), function(.nm) {
    .col <- df[[.nm]]
    if (!inherits(.col, "units")) return(.col)
    if (requireNamespace("units", quietly = TRUE)) {
      # Only convert when et had prior units; auto-detected units just strip label
      if (.nm %in% c("time", "ii") && hasTimeU) {
        return(as.numeric(units::set_units(.col, tu, mode = "standard")))
      }
      if (.nm == "amt" && hasDoseU) {
        return(as.numeric(units::set_units(.col, du, mode = "standard")))
      }
      if (.nm == "rate" && hasDoseU && hasTimeU) {
        .rateU <- paste0(du, "/", tu)
        return(as.numeric(units::set_units(.col, .rateU, mode = "standard")))
      }
    }
    as.numeric(.col)
  })
  names(.cols) <- names(df)
  as.data.frame(.cols, stringsAsFactors = FALSE)
}
#' This standardizes the id and evid columns in the imported data.frame
#'
#' @param df input data.frame
#' @return data.frame with standardized id and evid columns
#' @noRd
#' @author Matthew L. Fidler
.etImportStandardizeIdEvid <- function(df) {
  if (is.null(df$evid)) {
    df$evid <- if (!is.null(df$amt)) ifelse(!is.na(df$amt) & as.numeric(df$amt) != 0, 1L, 0L) else 0L
  }
  df$evid <- as.integer(df$evid)
  if (is.null(df$id)) df$id <- 1L
  df$id <- as.integer(df$id)
  df
}
#' Import, updating which columns are shown
#'
#' @param env environment to update with new data and show flags (modified by reference)
#'
#' @param df data.frame to import, with standardized id and evid columns
#'
#' @return nothing, called for side effect of modifying env by reference
#'
#' @noRd
#'
#' @author Matthew L. Fidler
#'
.etImportUpdateShow <- function(env, df) {
  if (length(env$ids) > 1L) env$show["id"] <- TRUE
  if (sum(df$evid != 0L, na.rm = TRUE) > 0L) env$show["amt"] <- TRUE
  if (!is.null(df$rate) && any(df$rate[df$evid != 0L] != 0, na.rm = TRUE))
    env$show["rate"] <- TRUE
  if (!is.null(df$dur) && any(df$dur[df$evid != 0L] != 0, na.rm = TRUE))
    env$show["dur"] <- TRUE
  if (!is.null(df$ii) && any(df$ii != 0, na.rm = TRUE)) {
    env$show["ii"] <- TRUE
    env$show["addl"] <- TRUE
  }
}
#' Import and update the environment
#'
#' @param env environment to update with new data and show flags
#'   (modified by reference)
#' @param df data.frame to import, with standardized id and evid
#'   columns and units converted to numeric
#' @return nothing, called for side effect of modifying env by reference
#' @noRd
#' @author Matthew L. Fidler
.etImportUpdateEnv <- function(env, df) {
  env$ids  <- sort(unique(df$id))
  env$nobs  <- env$nobs  + sum(df$evid == 0L, na.rm = TRUE)
  env$ndose <- env$ndose + sum(df$evid != 0L, na.rm = TRUE)
  env$chunks <- .addRowsToChunks(env$chunks, df)
  .etImportUpdateShow(env, df)
}

#' Import the event tables and normalize column names to lowercase,
#' converting NONMEM-style uppercase
#'
#'
#' @param df data.frame to normalize column names
#' @return normalized data frame
#' @noRd
#' @author Matthew L. Fidler
.etImportNormalizeNames <- function(df) {
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
  df
}
#' Import ID column as integer, converting from character if needed
#'
#'
#' @param df data.frame to import the ID column from
#' @return data.frame with ID column converted to integer if it was character
#' @noRd
#' @author Matthew L. Fidler
.etImportIdToInteger <- function(df) {
  # Convert character ID to sequential integers (silently)
  if (!is.null(df$id) && !is.numeric(df$id) && !is.integer(df$id)) {
    .uniq <- unique(df$id)
    df$id <- as.integer(factor(df$id, levels = .uniq))
  }
  df
}
#' Drop rows with NA time, warning about how many were dropped
#'
#'
#' @param df input data frame
#' @return data frame with NA time dropped
#' @noRd
#' @author Matthew L. Fidler
.etImportDropNaTime <- function(df) {
  # Drop rows with NA time (warn)
  if (!is.null(df$time) && any(is.na(as.numeric(df$time)))) {
    .nDrop <- sum(is.na(as.numeric(df$time)))
    warning(sprintf("dropping %d row(s) with NA time", .nDrop), call. = FALSE)
    df <- df[!is.na(as.numeric(df$time)), , drop = FALSE]
  }
  df
}
#' Import a data frame
#'
#' @param env environment to update with new data and show flags
#'   (modified by reference)
#'
#' @param df data.frame to import, with standardized id and evid
#'   columns and units converted to numeric
#'
#' @return nothing, called for side effects
#'
#' @noRd
#'
#' @author Matthew L. Fidler
.etMethodImportEventTable <- function(env, df) {
  if (!is.data.frame(df)) {
    stop("'df' must be a data.frame", call. = FALSE)
  }
  .u <- .etImportAutoUnits(env, df)
  df <- .etImportConvertUnits(df, .u$tu, .u$du, .u$hasTimeU, .u$hasDoseU)
  df <- .etImportStandardizeIdEvid(df)
  .etImportUpdateEnv(env, df)
  invisible(NULL)
}

#'  Import an event table from a data.frame.
#'
#' This method auto-detects units and normalization of column names
#' and id/evid columns; delegates to .etMethodImportEventTable after
#' processing
#'
#' @param env environment to update with new data and show flags
#'   (modified by reference)
#'
#' @param df data.frame to import
#'
#' @param ... additional arguments (ignored)
#'
#' @return nothing, called for side effects
#'
#' @noRd
#'
#' @author Matthew L. Fidler
.etMethodImportEventTable2 <- function(env, df, ...) {
  if (!is.data.frame(df)) stop("'df' must be a data.frame", call. = FALSE)
  df <- .etImportNormalizeNames(df)
  df <- .etImportIdToInteger(df)
  df <- .etImportDropNaTime(df)
  # Delegate to import.EventTable for units handling + storage
  .etMethodImportEventTable(env, df)
  invisible(NULL)
}
#' $expand() method
#'
#' @param env environment to update with expanded data and show flags
#'   (modified by reference)
#'
#' @return nothing, called for side effect of modifying env by reference
#'
#' @noRd
#'
#' @author Matthew L. Fidler
.etMethodExpand <- function(env) {
  .et <- structure(list(env = env), class = "rxEt")
  .mat <- .etMaterialize(.et)
  .expanded <- .etExpandAddl(.mat, env)
  env$chunks <- list()
  if (nrow(.expanded) > 0L) {
    .ids <- unique(as.integer(.expanded$id))
    for (.i in .ids) {
      env$chunks[[.i]] <- .expanded[.expanded$id == .i, , drop = FALSE]
    }
    env$ids <- sort(.ids)
  } else {
    env$ids <- 1L
  }
  env$nobs <- sum(.expanded$evid == 0L, na.rm = TRUE)
  env$ndose <- sum(.expanded$evid != 0L, na.rm = TRUE)
  env$show["id"] <- length(env$ids) > 1L
  env$show["addl"] <- !is.null(.expanded$addl) && any(.expanded$addl != 0L, na.rm = TRUE)
  env$randomType <- NA_integer_
  env$canResize <- FALSE
  invisible(NULL)
}

#' This get the observation records (evid == 0) from the event table
#'
#' @param env  environment to get the observation records from
#' @return logical of observation records (evid == 0)
#' @noRd
#' @author Matthew L. Fidler
.etMethodGetObsRec <- function(env) {
  .mat <- .etMaterialize(structure(list(env = env), class = "rxEt"))
  .mat$evid == 0L
}
#' $copy() method
#'
#' Creates a new rxEt object with a copy of the environment and methods, so
#' that mutations to the new copy do not affect the original.
#'
#' @param env environment to copy from
#' @return new rxEt object with copied environment and methods
#' @noRd
#' @author Matthew L. Fidler
.etMethodCopy <- function(env) {
  .newEnv <- new.env(parent = emptyenv())
  .newEnv$chunks     <- env$chunks
  .newEnv$units      <- env$units
  .newEnv$show       <- env$show
  .newEnv$ids        <- env$ids
  .newEnv$nobs       <- env$nobs
  .newEnv$ndose      <- env$ndose
  .newEnv$randomType <- env$randomType
  .newEnv$canResize  <- env$canResize
  .newEnv$methods <- .etBuildMethods(.newEnv)
  .cp <- list()
  attr(.cp, "names")     <- character(0)
  attr(.cp, "class")     <- c("rxEt", "data.frame")
  attr(.cp, "row.names") <- integer(0)
  attr(.cp, ".rxEtEnv")  <- .newEnv
  .cp
}
#'  $simulate() method
#'
#'
#' @param env environment to update with simulated data (modified by
#'   reference)
#'
#' @param seed random seed for reproducibility (currently ignored, as
#'   simulation is done in-place on the event table)
#'
#' @param ... additional arguments passed to the simulation method
#'   (currently ignored, as simulation is done in-place on the event
#'   table)
#'
#' @return nothing, called for side effects
#'
#' @noRd
#' @author Matthew L. Fidler
.etMethodSimulate <- function(env, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  .et <- structure(list(env = env), class = "rxEt")
  .mat <- .etMaterialize(.et)
  .hasWin <- !is.na(.mat$low) & !is.na(.mat$high)
  if (!any(.hasWin)) {
    warning("simulating event table without windows returns identical event table", call. = FALSE)
  } else {
    if (!is.na(env$randomType) && env$randomType == 3L) {
      .mat$time[.hasWin] <- stats::rnorm(sum(.hasWin), .mat$low[.hasWin], .mat$high[.hasWin])
    } else {
      .mat$time[.hasWin] <- stats::runif(sum(.hasWin), .mat$low[.hasWin], .mat$high[.hasWin])
    }
    env$chunks <- list()
    if (nrow(.mat) > 0L) {
      .ids <- unique(as.integer(.mat$id))
      for (.i in .ids) env$chunks[[.i]] <- .mat[.mat$id == .i, , drop = FALSE]
    }
  }
  invisible(NULL)
}

.etMethodClearDosing <- function(env) {
  for (.i in seq_along(env$chunks)) {
    if (!is.null(env$chunks[[.i]])) {
      .df <- env$chunks[[.i]]
      .df <- .df[.df$evid == 0L, , drop = FALSE]
      if (nrow(.df) == 0L) env$chunks[.i] <- list(NULL) else env$chunks[[.i]] <- .df
    }
  }
  env$ndose <- 0L
  invisible(NULL)
}

.etMethodGetSampling <- function(env) {
  .mat <- .etMaterialize(structure(list(env = env), class = "rxEt"))
  if (nrow(.mat) == 0L) return(NULL)
  .s <- .etDropUnitsForChunk(.mat[.mat$evid == 0L, , drop = FALSE])
  if (nrow(.s) == 0L) {
    NULL
  } else {
    rownames(.s) <- seq_len(nrow(.s))
    .s
  }
}

.etMethodAddSampling <- function(env, time, time.units = NA_character_) {
  .time <- time
  if (is.list(.time)) {
    .time <- lapply(.time, function(.window) {
      if (inherits(.window, "units") && requireNamespace("units", quietly = TRUE)) {
        .tu <- env$units["time"]
        if (!is.na(.tu) && nchar(.tu) > 0) {
          return(as.numeric(units::set_units(.window, .tu, mode = "standard")))
        }
        return(as.numeric(.window))
      }
      .window
    })
  } else if (inherits(.time, "units") && requireNamespace("units", quietly = TRUE)) {
    .tu <- env$units["time"]
    if (!is.na(.tu) && nchar(.tu) > 0) {
      .time <- as.numeric(units::set_units(.time, .tu, mode = "standard"))
    } else {
      .time <- as.numeric(.time)
    }
  }
  .df <- .etObsChunk(.time)
  .etAddChunk(env, .df, env$ids)
  env$nobs   <- env$nobs + length(.df$time) * length(env$ids)
  if (!is.na(time.units)) env$units["time"] <- time.units
  invisible(NULL)
}

#' Attach method functions to an rxEt\'s list structure
#'
#' Each method captures env by reference so mutations are shared.
#'
#' @param env environment
#'
#' @return named list of functions
#'
#' @noRd
.etBuildMethods <- function(env) {
  .lst <- list(
    add.dosing = function(dose, nbr.doses = 1L, dosing.interval = 24,
                          dosing.to = 1L, rate = NULL,
                          amount.units = NA_character_,
                          start.time = 0.0, do.sampling = FALSE,
                          time.units = NA_character_,
                          evid = NULL, strt.time = NULL, ...) {
      .etMethodAddDosing(env, dose, nbr.doses, dosing.interval, dosing.to, rate,
                         amount.units, start.time, do.sampling, time.units,
                         evid, strt.time, ...)
    },

    add.sampling = function(time, time.units = NA_character_) {
      .etMethodAddSampling(env, time, time.units)
    },

    get.units = function() env$units,
    getUnits  = function() env$units,

    get.nobs  = function() env$nobs,
    get.EventTable = function() {
      .etMethodGetEventTable(env)
    },
    get.obs.rec = function() {
      .etMethodGetObsRec(env)
    },
    get.dosing = function() {
      .etMethodGetDosing(env)
    },
    get.sampling = function() {
      .etMethodGetSampling(env)
    },
    clear.sampling = function() {
      .etMethodClearSampling(env)
    },
    clear.dosing = function() {
      .etMethodClearDosing(env)
    },
    copy = function() {
      .etMethodCopy(env)
    },
    import.EventTable = function(df) {
      .etMethodImportEventTable(env, df)
    },

    importEventTable = function(df, ...) {
      .etMethodImportEventTable2(env, df, ...)
    },

    expand = function() {
      .etMethodExpand(env)
    },
    simulate = function(object, nsim = 1, seed = NULL, ...) {
      .etMethodSimulate(env, seed, ...)
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
#'
#' @param amountUnits character dose unit, e.g. "mg"
#'
#' @param timeUnits character time unit, e.g. "hours"
#'
#' @return rxEt object
#'
#' @noRd
.newRxEt <- function(amountUnits = NA_character_, timeUnits = NA_character_) {
  .env <- new.env(parent = emptyenv())
  .env$chunks     <- list()
  .env$units      <- c(dosing = amountUnits, time = timeUnits)
  .env$show       <- .etDefaultShow()
  .env$ids        <- 1L
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

#' Sync the materialized data.frame shell with the rxEt environment
#'
#' @param x rxEt object
#'
#' @return materialized synced rxEt data frame
#'
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

#' Expand addl doses into individual records
#'
#' @param df materialized data.frame from .etMaterialize()
#'
#' @param env environment for randomType and show flags
#'
#' @param windows logical; if TRUE, only expand windows (low/high) by
#'   ii on each subsequent dose based on random type in env
#'
#' @return data.frame with addl expanded into individual dose records
#'
#' @noRd
.etExpandAddl <- function(df, env=NULL, windows=FALSE) {
  if (windows) {
    .doseRows <- df[df$evid != 0L & df$addl > 0L & !is.na(df$low), , drop = FALSE]
  } else {
    .doseRows <- df[df$evid != 0L & df$addl > 0L, , drop = FALSE]
  }
  if (nrow(.doseRows) == 0L) return(df)
  if (is.environment(env) &&
        exists("randomType", envir = env, inherits = FALSE)) {
    .rt <- env$randomType
    if (is.na(.rt)) .rt <- 1L
  } else {
    .rt <- 1
  }
  .extras <- vector("list", nrow(.doseRows))
  for (.i in seq_len(nrow(.doseRows))) {
    .row   <- .doseRows[.i, ]
    .n     <- .row$addl
    .extra <- .row[rep(1L, .n), , drop = FALSE]
    # This works for normal and uniform cases
    if (!is.na(.row$low)) {
      .extra$low <- .row$low + seq_len(.n) * .row$ii
    }
    if (.rt == 3) {
      # normal; sd remains the same on expansion
      .extra$time <- vapply(seq_len(.n), function(i) {
        stats::rnorm(1L, .extra$low[i], .extra$high[i])
      }, numeric(1), USE.NAMES = FALSE)
    } else if (!is.na(.row$high)) {
      # uniform or window expand window by ii on each subsequent dose
      .extra$high <- .row$high + seq_len(.n) * .row$ii
      .extra$time <- vapply(seq_len(.n), function(i) {
        stats::runif(1L, .extra$low[i], .extra$high[i])
      }, numeric(1), USE.NAMES = FALSE)
    } else {
      .extra$time <- .row$time + seq_len(.n) * .row$ii
    }


    .extra$high <- .row$high + seq_len(.n) * .row$ii
    .extra$addl <- 0L
    .extra$ii   <- 0.0
    .extras[[.i]] <- .extra
  }
  df$addl[df$evid != 0L] <- 0L
  df$ii[df$evid   != 0L] <- 0.0
  .combined <- do.call(rbind, c(list(df), .extras))
  .evidSort <- ifelse(.combined$evid == 3L, -1L, as.integer(.combined$evid))
  .combined[.order3(.combined$id, .combined$time, .evidSort), ] # nolint
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
#'
#' @param df materialized data.frame from .etMaterialize()
#'
#' @return df with cmt column possibly converted to integer
#'
#' @noRd
.etFixCmtForSolve <- function(df) {
  .cmt <- df[["cmt"]]
  if (is.null(.cmt) || !is.character(.cmt)) return(df)
  .isNA      <- is.na(.cmt)
  .isSentinel <- !.isNA & (.cmt == "(default)" | .cmt == "(obs)")
  .numericOk  <- !.isNA & !.isSentinel & suppressWarnings(!is.na(as.integer(.cmt)))
  if (all(.isNA | .isSentinel | .numericOk)) {
    .int <- integer(length(.cmt))
    .int[.isNA]       <- NA_integer_
    .int[.isSentinel] <- 1L
    .int[.numericOk]  <- as.integer(.cmt[.numericOk])
    df[["cmt"]] <- .int
  }
  df
}

#' Empty data.frame skeleton matching materialized format
#'
#' @return empty data.frame with canonical columns and types
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
#' Chunks is a list indexed by ID integer value; \code{chunks[[i]]} is
#' a data.frame for ID i (with 'id' column set), or NULL if no records
#' for that ID.
#'
#' @param et rxEt object
#'
#' @return data.frame with canonical columns, sorted by (id, time,
#'   evid-adjusted)
#'
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
      if (.col %in% names(.dt)) {
        .vals <- .dt[[.col]]
        .naInDose <- is.na(.vals[.doseIdx])
        if (any(.naInDose)) {
          .vals[.doseIdx[.naInDose]] <- 0.0
          data.table::set(.dt, j = .col, value = .vals)
        }
      }
    }
    for (.col in c("addl", "ss")) {
      if (.col %in% names(.dt)) {
        .vals <- .dt[[.col]]
        .naInDose <- is.na(.vals[.doseIdx])
        if (any(.naInDose)) {
          .vals[.doseIdx[.naInDose]] <- 0L
          data.table::set(.dt, j = .col, value = .vals)
        }
      }
    }
  }

  # Sort: id ASC, time ASC, evid=3 sorts before others at same (id, time)
  .evidSort <- ifelse(.dt$evid == 3L, -1L, as.integer(.dt$evid))
  .ord <- .order3(.dt$id, .dt$time, .evidSort) # nolint
  .dt <- .dt[.ord, ]

  # Ensure canonical column order
  .missing <- setdiff(.etColOrder, names(.dt))
  for (.col in .missing) data.table::set(.dt, j = .col, value = NA)
  data.table::setcolorder(.dt, .etColOrder)

  .df <- as.data.frame(.dt)
  .df <- .etExpandAddl(.df, env = .env, windows = TRUE)

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
#'
#' @param x object to test
#'
#' @return logical
#'
#' @export
is.rxEt <- function(x) {
  if (!inherits(x, "rxEt")) return(FALSE)
  .env <- .rxEtEnv(x)
  is.environment(.env)
}

#' @noRd
.etWindowTime <- function(time, allow1 = FALSE) {
  .randomType <- NA_integer_
  if (inherits(time, "list")) {
    .nw <- length(time)
    .low  <- numeric(.nw)
    .mid  <- numeric(.nw)
    .high <- numeric(.nw)
    # This is for the normal variability case that is list(c(4, 2, NA))
    .hasNormal <- any(vapply(time,
                             function(.w) {
                               length(.w) == 3L && is.na(.w[3L])
                             }, logical(1L)))
    .has3 <- any(vapply(time,
                        function(.w) {
                          length(.w) == 3L && !is.na(.w[3L])
                        }, logical(1L)))
    for (.i in seq_len(.nw)) {
      .w <- time[[.i]]
      if (length(.w) == 1L) {
        if (!allow1) stop("each window must be c(low, high) or c(low, mid, high)", call. = FALSE)
        .low[.i]  <- NA_real_
        .mid[.i]  <- as.numeric(.w)
        .high[.i] <- NA_real_
      } else if (length(.w) == 2L) {
        if (.hasNormal) {
          .low[.i]  <- .w[1]
          .mid[.i]  <- stats::rnorm(1, .w[1], .w[2])
          .high[.i] <- .w[2]
        } else if (.has3) {
          stop("Cannot mix 2 and 3 element windows", call. = FALSE)
        } else {
          if (.w[1] > .w[2]) {
            stop("window bounds must be ordered c(low, high)", call. = FALSE)
          }
          .low[.i]  <- .w[1]
          .mid[.i]  <- stats::runif(1, .w[1], .w[2])
          .high[.i] <- .w[2]
        }
      } else if (length(.w) == 3L) {
        if (is.na(.w[3L])) {
          .low[.i]  <- .w[1]
          .mid[.i]  <- stats::rnorm(1, .w[1], .w[2])
          .high[.i] <- .w[2]
        } else {
          if (.w[1] > .w[2] || .w[2] > .w[3]) stop("window bounds must be ordered c(low, mid, high)", call. = FALSE)
          .low[.i]  <- .w[1]
          .mid[.i]  <- .w[2]
          .high[.i] <- .w[3]
        }
      } else {
        stop("each window must be c(low, high) or c(low, mid, high)", call. = FALSE)
      }
    }
    if (.hasNormal) {
      .randomType <- 3L
    } else if (.has3) {
      .randomType <- 1L
    } else {
      .randomType <- 2L
    }
    return(list(time = .mid, low = .low, high = .high, randomType = .randomType))
  }
  list(time = as.numeric(time), low = NA_real_, high = NA_real_, randomType = NA_integer_)
}

#' @noRd
.etDoseUntil <- function(until, time, ii, addl, isList) {
  if (is.null(until)) return(addl)
  if (ii <= 0) stop("'until' requires a positive 'ii'", call. = FALSE)
  .tmp <- until - time - ii
  if (any(.tmp > 0, na.rm = TRUE)) {
    .ratio <- .tmp / ii
    .ceil <- ceiling(.ratio)
    addl <- ifelse(.ceil == .ratio, .ratio + 1, .ceil)
    addl <- as.integer(addl)
  } else {
    addl <- 0L
    if (!isList) {
      warning("'time'+'ii' is greater than 'until', no additional doses added", call. = FALSE)
    }
  }
  if (any(addl < 0L, na.rm = TRUE)) {
    warning("'until' is before 'time'; setting addl=0", call. = FALSE)
    addl[addl < 0L] <- 0L
  }
  addl
}

#' @noRd
.etDoseValidate <- function(amt, ii, addl, ss, rate) {
  if (ii > 0 && ss == 0L && all(addl == 0L, na.rm = TRUE)) {
    warning(sprintf(
      "'ii' requires non zero additional doses ('addl') or steady state dosing ('ii': %f, 'ss': %d; 'addl': %d), reset 'ii' to zero", # nolint
      ii, ss, max(addl, na.rm = TRUE)
    ), call. = FALSE)
    ii <- 0.0
  }

  if (any(addl > 0L, na.rm = TRUE) && ii == 0.0)
    stop("'addl' > 0 requires a positive inter-dose interval ('ii')", call. = FALSE)

  if (ss > 0L) {
    if (rate < -1.0 && ii == 0.0)
      stop("cannot use duration flag (rate=-2) with steady-state dosing", call. = FALSE)
    if (ss == 2L && ii == 0.0)
      stop("ss=2 requires a positive inter-dose interval ('ii')", call. = FALSE)
    if (rate > 0 && ii > 0 && amt == 0)
      stop("cannot combine constant infusion (rate>0) with dose interval (ii>0) for steady-state; use ii=0 for constant infusion SS", call. = FALSE) # nolint
  }
  ii
}

#' Build an observation chunk list (no id column; caller uses .etAddChunk)

#'
#' Returns a sparse named list where scalar fields (evid) are stored once and
#' vector fields (time) hold per-row values. Callers use length(.df$time) for
#' row count, not nrow().
#'
#' @param time numeric vector of sample times, OR a list of c(low,high)
#'   or c(low,mid,high) windows.
#'
#' @param cmt compartment name or number (optional scalar)
#'
#' @param id integer vector of ids to attach (optional)
#'
#' @return sparse named list with evid=0L scalar and time vector
#'
#' @noRd
.etObsChunk <- function(time, cmt = NULL, id = NULL) {
  .time <- .etWindowTime(time)
  .df <- list(evid = 0L, time = .time$time, low = .time$low, high = .time$high)
  if (!is.null(cmt)) .df$cmt <- cmt
  if (!is.null(id))  .df$id  <- as.integer(id)
  attr(.df, ".randomType") <- .time$randomType
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

  .time <- .etWindowTime(time, allow1 = TRUE)
  .timeDose <- .time$time
  .lowDose <- .time$low
  .highDose <- .time$high
  .randomType <- .time$randomType

  addl <- .etDoseUntil(until, .timeDose, ii, addl, inherits(time, "list"))
  ii <- .etDoseValidate(amt, ii, addl, ss, rate)

  if (dur > 0 && rate == 0.0) {
    rate <- amt / dur
    dur  <- 0.0
  }

  if (length(amt) > 1L || length(time) > 1L) {
    if (length(.timeDose) == 1L) .timeDose <- rep(.timeDose, length(amt))
    if (length(amt)  == 1L) amt  <- rep(amt,  length(.timeDose))
    if (length(.timeDose) != length(amt)) stop("'time' and 'amt' must have the same length", call. = FALSE)
  }

  .res <- data.frame(
    time = as.numeric(.timeDose), amt  = as.numeric(amt),
    evid = as.integer(evid), cmt  = as.character(cmt),
    ii   = as.numeric(ii),   addl = as.integer(addl),
    ss   = as.integer(ss),   rate = as.numeric(rate),
    dur  = as.numeric(dur),
    low  = as.numeric(.lowDose),
    high = as.numeric(.highDose),
    stringsAsFactors = FALSE
  )
  attr(.res, ".randomType") <- .randomType
  .res
}

#' Rebuild an rxEt shell after data-frame style mutation
#'
#' @param x rxEt object
#'
#' @param df materialized data.frame from .etMaterialize() after mutation
#'
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
