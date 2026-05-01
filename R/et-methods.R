# rxEt internal methods ---------------------------------------------------

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

# rxEt import helpers -----------------------------------------------------

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
        # udunits format: "ug s-1" - amount unit has no digit exponent
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

# rxEt query and mutation methods -----------------------------------------

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

# rxEt method registry ----------------------------------------------------

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
