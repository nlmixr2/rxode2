# rxEt environment and chunk helpers -------------------------------------

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

.etGroups <- function(envRef) {
  .groups <- envRef$groups
  if (is.null(.groups)) .groups <- list()
  .groups
}

.etGroupIdsEqual <- function(x, y) {
  identical(sort.int(as.integer(x), method = "quick"),
            sort.int(as.integer(y), method = "quick"))
}

.etGroupChunk <- function(df, ids = NULL) {
  if (!is.data.frame(df)) {
    df <- .etExpandObsChunk(df)
  }
  df <- .etDropUnitsForChunk(df)
  if (!is.null(ids) && "id" %in% names(df)) {
    .id <- unique(as.integer(df$id))
    if (length(.id) == 1L && length(ids) >= 1L) {
      df$id <- NULL
    }
  }
  df
}

.etGetGroups <- function(envRef) {
  .groups <- .etGroups(envRef)
  if (length(.groups) > 0L) return(.groups)

  .chunks <- envRef$chunks
  if (length(.chunks) == 0L) return(list())

  .ret <- vector("list", 0L)
  for (.i in seq_along(.chunks)) {
    .chunk <- .chunks[[.i]]
    if (is.null(.chunk)) next
    .ids <- if (!is.null(names(.chunk)) && "id" %in% names(.chunk)) {
      unique(as.integer(.chunk$id))
    } else if (length(envRef$ids) == 1L) {
      as.integer(envRef$ids)
    } else {
      as.integer(.i)
    }
    .df <- .etGroupChunk(.chunk, .ids)
    .ret[[length(.ret) + 1L]] <- list(ids = .ids, data = .df)
  }
  .ret
}

.etSetGroups <- function(envRef, groups) {
  envRef$groups <- groups
  if (length(groups) > 0L) {
    envRef$chunks <- list()
  }
  invisible(NULL)
}

.etPresentIds <- function(envRef) {
  .groups <- .etGroups(envRef)
  if (length(.groups) > 0L) {
    return(sort(unique(as.integer(unlist(lapply(.groups, `[[`, "ids"), use.names = FALSE)))))
  }
  .chunks <- envRef$chunks
  if (length(.chunks) == 0L) return(integer(0))
  .ids <- integer(0)
  for (.i in seq_along(.chunks)) {
    .chunk <- .chunks[[.i]]
    if (is.null(.chunk)) next
    if (!is.null(names(.chunk)) && "id" %in% names(.chunk)) {
      .ids <- c(.ids, unique(as.integer(.chunk$id)))
    } else if (length(envRef$ids) == 1L) {
      .ids <- c(.ids, as.integer(envRef$ids))
    } else {
      .ids <- c(.ids, as.integer(.i))
    }
  }
  sort(unique(.ids))
}

.etResetCountsFromGroups <- function(envRef) {
  .groups <- .etGetGroups(envRef)
  if (length(.groups) == 0L) {
    envRef$nobs <- 0L
    envRef$ndose <- 0L
    return(invisible(NULL))
  }
  envRef$nobs <- as.integer(sum(vapply(.groups, function(.g) {
    sum(.g$data$evid == 0L, na.rm = TRUE) * length(.g$ids)
  }, numeric(1))))
  envRef$ndose <- as.integer(sum(vapply(.groups, function(.g) {
    sum(.g$data$evid != 0L, na.rm = TRUE) * length(.g$ids)
  }, numeric(1))))
  invisible(NULL)
}

.etPreviewData <- function(envRef, subset = c("all", "dosing", "sampling")) {
  subset <- match.arg(subset)
  .groups <- .etGroups(envRef)
  if (length(.groups) == 0L) {
    .mat <- .etMaterialize(structure(list(env = envRef), class = "rxEt"))
    if (nrow(.mat) == 0L) return(NULL)
    if (subset == "dosing") {
      .mat <- .mat[.mat$evid != 0L, , drop = FALSE]
    } else if (subset == "sampling") {
      .mat <- .mat[.mat$evid == 0L, , drop = FALSE]
    }
    if (nrow(.mat) == 0L) return(NULL)
    .mat <- .etDropUnitsForChunk(.mat)
    rownames(.mat) <- seq_len(nrow(.mat))
    return(.mat)
  }

  .ret <- vector("list", 0L)
  .meta <- vector("list", 0L)
  for (.i in seq_along(.groups)) {
    .g <- .groups[[.i]]
    .df <- .etDropUnitsForChunk(.g$data)
    if (subset == "dosing") {
      .df <- .df[.df$evid != 0L, , drop = FALSE]
    } else if (subset == "sampling") {
      .df <- .df[.df$evid == 0L, , drop = FALSE]
    }
    if (nrow(.df) == 0L) next
    .ret[[length(.ret) + 1L]] <- .df
    .meta[[length(.meta) + 1L]] <- list(
      ids = as.integer(.g$ids),
      nSub = length(.g$ids)
    )
  }
  if (length(.ret) == 0L) return(NULL)
  .out <- as.data.frame(data.table::rbindlist(.ret, fill = TRUE))
  rownames(.out) <- seq_len(nrow(.out))
  attr(.out, "rxEtPreviewGroups") <- .meta
  class(.out) <- c("rxEtPreview", class(.out))
  .out
}

.etMaterializeGroup <- function(group) {
  .df <- .etDropUnitsForChunk(group$data)
  if (!is.data.frame(.df) || nrow(.df) == 0L || length(group$ids) == 0L) {
    return(.etEmptyDf())
  }
  .n <- nrow(.df)
  .idx <- rep(seq_len(.n), times = length(group$ids))
  .id <- rep(as.integer(group$ids), each = .n)
  .ret <- .df[.idx, , drop = FALSE]
  .ret$id <- .id
  .ret
}

.etShiftChunk <- function(df, timeDelta) {
  .df <- .etDropUnitsForChunk(df)
  if (!is.data.frame(.df) || nrow(.df) == 0L || identical(timeDelta, 0)) {
    return(.df)
  }
  .df$time <- .df$time + timeDelta
  if (!is.null(.df$low) && any(!is.na(.df$low))) {
    .df$low[!is.na(.df$low)] <- .df$low[!is.na(.df$low)] + timeDelta
  }
  if (!is.null(.df$high) && any(!is.na(.df$high))) {
    .df$high[!is.na(.df$high)] <- .df$high[!is.na(.df$high)] + timeDelta
  }
  if (!is.null(.df$ii)) .df$ii[is.na(.df$ii)] <- 0.0
  .df
}

.etExpandGroupData <- function(df, envRef) {
  .df <- .etDropUnitsForChunk(df)
  if (!is.data.frame(.df) || nrow(.df) == 0L) {
    return(.df)
  }
  .tmp <- .df
  .tmp$id <- 1L
  .expanded <- .etExpandAddl(.tmp, envRef)
  .expanded$id <- NULL
  .expanded
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
  if (!"id" %in% names(df)) {
    .groups <- .etGetGroups(envRef)
    .chunk <- .etGroupChunk(df, .posIds)
    if (length(.groups) > 0L) {
      .newGroups <- vector("list", 0L)
      .remainingIds <- sort.int(as.integer(.posIds), method = "quick")
      for (.g in .groups) {
        .overlap <- intersect(.g$ids, .remainingIds)
        if (length(.overlap) == 0L) {
          .newGroups[[length(.newGroups) + 1L]] <- .g
          next
        }
        .keepIds <- setdiff(.g$ids, .overlap)
        if (length(.keepIds) > 0L) {
          .newGroups[[length(.newGroups) + 1L]] <- list(ids = .keepIds, data = .g$data)
        }
        .newGroups[[length(.newGroups) + 1L]] <- list(
          ids = sort.int(as.integer(.overlap), method = "quick"),
          data = as.data.frame(data.table::rbindlist(list(.g$data, .chunk), fill = TRUE))
        )
        .remainingIds <- setdiff(.remainingIds, .overlap)
      }
      if (length(.remainingIds) > 0L) {
        .newGroups[[length(.newGroups) + 1L]] <- list(ids = .remainingIds, data = .chunk)
      }
      .etSetGroups(envRef, .newGroups)
      return(invisible(NULL))
    }
    if (length(.posIds) > 1L) {
      .match <- which(vapply(.groups, function(.g) .etGroupIdsEqual(.g$ids, .posIds), logical(1)))
      if (length(.match) == 1L) {
        .groups[[.match]]$data <- as.data.frame(
          data.table::rbindlist(list(.groups[[.match]]$data, .chunk), fill = TRUE)
        )
      } else {
        .groups[[length(.groups) + 1L]] <- list(ids = sort.int(as.integer(.posIds), method = "quick"),
                                                data = .chunk)
      }
      .etSetGroups(envRef, .groups)
      return(invisible(NULL))
    }
  }
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

# rxEt object construction ------------------------------------------------

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
  .env$groups     <- list()
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
  .nTot <- .env$nobs + .env$ndose
  .ret <- if (.nTot <= 1000L) .etMaterialize(x) else .etEmptyDf()
  attr(.ret, ".rxEtEnv") <- .env
  .rt <- .env$randomType
  .cls <- c("rxEt", "data.frame")
  if (!is.null(.rt) && !is.na(.rt)) {
    .cls <- structure(.cls, ".rxode2.lst" = list(randomType = as.integer(.rt)))
  }
  class(.ret) <- .cls
  .ret
}

# rxEt materialization and dose expansion --------------------------------

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

.etGroupedSolveData <- function(x) {
  .env <- .rxEtEnv(x)
  .groups <- .etGetGroups(.env)
  if (length(.groups) == 0L) {
    return(NULL)
  }
  .dat <- vector("list", 0L)
  .ids <- vector("list", 0L)
  for (.i in seq_along(.groups)) {
    .g <- .groups[[.i]]
    .df <- .etFixCmtForSolve(.etDropUnitsForChunk(.g$data))
    if (!is.data.frame(.df) || nrow(.df) == 0L || length(.g$ids) == 0L) next
    .df$id <- rep.int(.i, nrow(.df))
    .dat[[length(.dat) + 1L]] <- .df
    .ids[[length(.ids) + 1L]] <- as.integer(.g$ids)
  }
  if (length(.dat) == 0L) {
    return(.etEmptyDf())
  }
  .out <- as.data.frame(data.table::rbindlist(.dat, fill = TRUE, use.names = TRUE))
  attr(.out, "rxHomGroups") <- .ids
  attr(.out, "rxHomIdLevels") <- as.character(unlist(.ids, use.names = FALSE))
  .out
}

.etPrepareSolveEvents <- function(events, ctl = NULL) {
  if (!is.rxEt(events)) {
    return(.etFixCmtForSolve(events))
  }
  .env <- .rxEtEnv(events)
  if (isTRUE(.env$nobs == 0L)) {
    return(.etFixCmtForSolve(.etMaterialize(events)))
  }
  if (is.null(ctl$iCov) && length(.etGroups(.env)) > 0L) {
    return(.etGroupedSolveData(events))
  }
  .etFixCmtForSolve(.etMaterialize(events))
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
  .groups <- .etGetGroups(.env)

  if (length(.groups) > 0L) {
    .nonNull <- Filter(function(.g) !is.null(.g$data) && nrow(.g$data) > 0L, .groups)
    if (length(.nonNull) == 0L) return(.etEmptyDf())
    .dtList <- lapply(.nonNull, function(.g) {
      .df <- .g$data
      if (!is.data.frame(.df)) {
        .df <- .etExpandObsChunk(.df)
      }
      if ("id" %in% names(.df)) {
        return(data.table::as.data.table(.df))
      }
      .n <- nrow(.df)
      if (.n == 0L || length(.g$ids) == 0L) {
        return(data.table::as.data.table(.etEmptyDf()))
      }
      .idx <- rep.int(seq_len(.n), times = length(.g$ids))
      .id <- rep(as.integer(.g$ids), each = .n)
      .ret <- data.table::as.data.table(.df[.idx, , drop = FALSE])
      .ret[, id := .id]
      .ret
    })
    .dt <- data.table::rbindlist(.dtList, fill = TRUE, use.names = TRUE)
  } else {
    .chunks <- .env$chunks

    if (length(.chunks) == 0L) return(.etEmptyDf())

    # Each element is a data.frame for one ID; filter NULL entries
    .nonNull <- Filter(Negate(is.null), .chunks)
    if (length(.nonNull) == 0L) return(.etEmptyDf())

    # rbindlist: fast sparse bind across ID data.frames
    .dt <- data.table::rbindlist(.nonNull, fill = TRUE, use.names = TRUE)
  }

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

# rxEt chunk builders and shell rebuilders -------------------------------

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
