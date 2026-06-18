#' Create a file-backed event table reference
#'
#' @param path path to the file containing the event table
#' @param format file format: "auto" (detect from extension), "parquet", "csv", "fst", or "rds"
#' @param id name of the subject ID column
#' @return An \code{rxEtFile} object
#' @export
rxEventTableFile <- function(path, format = c("auto", "parquet", "csv", "fst", "rds"),
                              id = "id") {
  format <- match.arg(format)
  if (format == "auto") {
    .ext <- tolower(tools::file_ext(path))
    format <- switch(.ext, parquet = "parquet", csv = "csv", fst = "fst", rds = "rds", "rds")
  }
  structure(list(path = path, format = format, id_col = id), class = "rxEtFile")
}

.rxEtFileReadFull <- function(x) {
  switch(x$format,
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE))
        stop("package 'arrow' is required for parquet format")
      as.data.frame(arrow::read_parquet(x$path))
    },
    csv     = data.table::fread(x$path, data.table = FALSE),
    fst     = {
      if (!requireNamespace("fst", quietly = TRUE))
        stop("package 'fst' is required for fst format")
      fst::read_fst(x$path)
    },
    rds     = readRDS(x$path)
  )
}

.rxEtFileReadCols <- function(x, cols) {
  switch(x$format,
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE))
        stop("package 'arrow' is required for parquet format")
      as.data.frame(arrow::read_parquet(x$path, col_select = dplyr::all_of(cols)))
    },
    csv     = data.table::fread(x$path, select = cols, data.table = FALSE),
    fst     = {
      if (!requireNamespace("fst", quietly = TRUE))
        stop("package 'fst' is required for fst format")
      fst::read_fst(x$path, columns = cols)
    },
    rds     = {
      .d <- readRDS(x$path)
      .d[, intersect(cols, names(.d)), drop = FALSE]
    }
  )
}

.rxEtFileReadChunk <- function(x, ids) {
  .dat <- .rxEtFileReadFull(x)
  .dat[.dat[[x$id_col]] %in% ids, , drop = FALSE]
}

rxMemSummary.rxEtFile <- function(x, ...) {
  .cols <- c(x$id_col, "evid")
  .dat <- .rxEtFileReadCols(x, .cols)
  .rxMemSummarizeDat(.dat)
}

# ── Main OOM solve loop ───────────────────────────────────────────────────────

.rxSolveOom <- function(object, params, events, inits, .ctl, .envir = parent.frame()) {
  .prefix   <- .ctl$file
  .nDaemons <- if (is.null(.ctl$parallel)) 0L else as.integer(.ctl$parallel)
  .useMirai <- .nDaemons > 0L && requireNamespace("mirai", quietly = TRUE)

  # Normalize: if events is an rxEt but params is not, events is the params; swap
  if ((is.rxEt(params) || rxIs(params, "rx.event")) && !is.rxEt(events)) {
    .tmp  <- events
    events <- params
    params <- .tmp
  }

  # Build memory summary from events
  .summary <- if (inherits(events, "rxEtFile")) {
    rxMemSummary.rxEtFile(events)
  } else {
    .evDf <- if (is.rxEt(events)) as.data.frame(events) else as.data.frame(events)
    .rxMemSummarizeDat(.evDf)
  }

  # Chunk size
  .chunkSize <- if (!is.null(.ctl$chunkSize)) {
    as.integer(.ctl$chunkSize)
  } else {
    .rxOomChunkSize(object, .summary, .ctl)
  }

  # Capture or assign base seed
  .baseSeed <- rxGetSeed()
  if (.baseSeed == -1L) {
    .baseSeed <- sample.int(.Machine$integer.max, 1L)
    rxSetSeed(.baseSeed)
  }

  # Split IDs into chunks
  .allIds    <- .summary$id
  .nSub      <- length(.allIds)
  .nChunks   <- ceiling(.nSub / .chunkSize)
  .chunkList <- split(.allIds, ceiling(seq_len(.nSub) / .chunkSize))

  .manifest <- list(
    version = 1L, prefix = .prefix,
    chunks  = character(.nChunks), nrows = integer(.nChunks),
    seed    = .baseSeed
  )
  .outFiles <- character(.nChunks)
  .cumSub   <- 0L

  # Control args forwarded to each chunk rxSolve call (strip OOM-specific fields)
  .fwdCtlArgs <- as.list(.ctl)
  .fwdCtlArgs$file      <- NULL
  .fwdCtlArgs$chunkSize <- NULL
  .fwdCtlArgs$parallel  <- NULL
  .fwdCtlArgs$serializeFile <- NULL

  # Pre-draw ALL subjects' etas once using the base seed so that chunked solves
  # reproduce the same etas as a single full rxSolve(seed=baseSeed) call.
  #
  # rxSolve_ calls seedEng(op->cores) BEFORE rxSimThetaOmega, advancing rxSeed
  # by 2*ncores.  We replicate that here with rxSeedEng() so our standalone
  # rxSimThetaOmega sees the same effective seed as the internal call in rxSolve_.
  .preDrawnParams <- NULL
  if (!is.null(.ctl$omega)) {
    .ncores <- if (!is.null(.ctl$cores) && .ctl$cores > 0L) {
      as.integer(.ctl$cores)
    } else {
      getRxThreads()
    }
    rxSetSeed(.baseSeed)
    rxSeedEng(.ncores)
    .preDrawnParams <- rxSimThetaOmega(
      params          = params,
      omega           = .ctl$omega,
      omegaDf         = .ctl$omegaDf,
      omegaLower      = if (!is.null(.ctl$omegaLower))  .ctl$omegaLower  else -Inf,
      omegaUpper      = if (!is.null(.ctl$omegaUpper))  .ctl$omegaUpper  else  Inf,
      omegaIsChol     = if (!is.null(.ctl$omegaIsChol)) .ctl$omegaIsChol else FALSE,
      omegaSeparation = if (!is.null(.ctl$omegaSeparation)) .ctl$omegaSeparation else "auto",
      omegaXform      = if (!is.null(.ctl$omegaXform))  .ctl$omegaXform  else 1L,
      nSub            = .nSub,
      nCoresRV        = 1L,
      nStud           = 1L
    )
    # Strip omega from forwarded args — etas are now baked into per-chunk params
    .fwdCtlArgs$omega           <- NULL
    .fwdCtlArgs$omegaDf         <- NULL
    .fwdCtlArgs$omegaLower      <- NULL
    .fwdCtlArgs$omegaUpper      <- NULL
    .fwdCtlArgs$omegaIsChol     <- NULL
    .fwdCtlArgs$omegaSeparation <- NULL
    .fwdCtlArgs$omegaXform      <- NULL
  }

  # Normalize: ensure id column is always present so chunks can be rbind'd.
  # Single-subject solves drop the id column; stamp it back from .chunkIds.
  .normalizeResult <- function(.result, .chunkIds) {
    .df <- as.data.frame(.result)
    if (!("id" %in% names(.df))) {
      .nPerSub <- nrow(.df) %/% max(length(.chunkIds), 1L)
      .df <- cbind(id = rep(.chunkIds, each = .nPerSub), .df)
    }
    .df
  }

  .writeResult <- function(.result, .chunkIds) {
    .df <- .normalizeResult(.result, .chunkIds)
    if (requireNamespace("arrow", quietly = TRUE)) {
      .f <- tempfile(fileext = ".parquet")
      arrow::write_parquet(.df, .f)
    } else {
      .f <- tempfile(fileext = ".rds")
      saveRDS(.df, .f)
    }
    .f
  }

  .extractChunkEvents <- function(.chunkIds) {
    if (inherits(events, "rxEtFile")) {
      .rxEtFileReadChunk(events, .chunkIds)
    } else {
      .evDf  <- if (is.rxEt(events)) as.data.frame(events) else as.data.frame(events)
      .idCol <- grep("^id$", names(.evDf), ignore.case = TRUE, value = TRUE)[1]
      if (is.na(.idCol)) {
        .evDf
      } else {
        .evDf[.evDf[[.idCol]] %in% .chunkIds, , drop = FALSE]
      }
    }
  }

  if (.useMirai) {
    .modelObj <- if (inherits(object, c("rxode2", "rxDll"))) object else rxode2(object)
    mirai::daemons(.nDaemons)
    on.exit(mirai::daemons(0), add = TRUE)
    .chunkEvList   <- vector("list", .nChunks)
    .chunkParamsList <- vector("list", .nChunks)
    for (.i in seq_len(.nChunks)) {
      .chunkEvList[[.i]] <- .extractChunkEvents(.chunkList[[.i]])
      .nThis <- length(.chunkList[[.i]])
      .chunkParamsList[[.i]] <- if (!is.null(.preDrawnParams)) {
        .preDrawnParams[(.cumSub + 1L):(.cumSub + .nThis), , drop = FALSE]
      } else {
        params
      }
      .cumSub <- .cumSub + .nThis
    }
    .cumSub <- 0L
    .chunkIdsList <- .chunkList
    .tasks <- mirai::mirai_map(
      seq_len(.nChunks),
      function(.i) {
        library(rxode2)
        .result <- do.call(rxSolve,
                           c(list(object = .modelObj, params = .chunkParamsList[[.i]],
                                  events = .chunkEvList[[.i]], inits = .inits),
                             .fwdCtlArgs))
        .df <- as.data.frame(.result)
        if (!("id" %in% names(.df))) {
          .ids <- .chunkIdsList[[.i]]
          .nPerSub <- nrow(.df) %/% max(length(.ids), 1L)
          .df <- cbind(id = rep(.ids, each = .nPerSub), .df)
        }
        if (requireNamespace("arrow", quietly = TRUE)) {
          .f <- tempfile(fileext = ".parquet")
          arrow::write_parquet(.df, .f)
        } else {
          .f <- tempfile(fileext = ".rds")
          saveRDS(.df, .f)
        }
        list(file = .f, nrows = nrow(.df))
      },
      .args = list(.chunkEvList = .chunkEvList, .chunkIdsList = .chunkIdsList,
                   .chunkParamsList = .chunkParamsList,
                   .inits = inits, .fwdCtlArgs = .fwdCtlArgs)
    )
    for (.i in seq_len(.nChunks)) {
      .r <- .tasks[[.i]][]
      .outFiles[.i] <- .r$file
      .manifest$nrows[.i] <- .r$nrows
    }
  } else {
    for (.i in seq_len(.nChunks)) {
      .chunkIds <- .chunkList[[.i]]
      .nThis    <- length(.chunkIds)
      .chunkEvents <- .extractChunkEvents(.chunkIds)
      .chunkParams <- if (!is.null(.preDrawnParams)) {
        .preDrawnParams[(.cumSub + 1L):(.cumSub + .nThis), , drop = FALSE]
      } else {
        rxSetSeed(as.integer(
          (as.double(.baseSeed) + as.double(.cumSub)) %% .Machine$integer.max
        ))
        params
      }
      .result <- do.call(rxSolve,
                         c(list(object = object, params = .chunkParams,
                                events = .chunkEvents, inits = inits,
                                envir = .envir), .fwdCtlArgs))
      .outFiles[.i] <- .writeResult(.result, .chunkIds)
      .manifest$nrows[.i] <- nrow(.result)
      .cumSub <- .cumSub + .nThis
    }
  }

  for (.i in seq_len(.nChunks)) {
    .manifest$chunks[.i] <- .outFiles[.i]
  }

  saveRDS(.manifest, paste0(.prefix, "_manifest.rds"))
  .rxSolveOomFromManifest(.manifest)
}

# ── rxSolveOom return object ──────────────────────────────────────────────────

.rxSolveOomFromManifest <- function(manifest) {
  structure(list(), class = "rxSolveOom", manifest = manifest)
}

.rxOomHasParquet <- function(manifest) {
  any(grepl("\\.parquet$", manifest$chunks))
}

#' @export
print.rxSolveOom <- function(x, ...) {
  .m <- attr(x, "manifest")
  .arrow <- .rxOomHasParquet(.m) && requireNamespace("arrow", quietly = TRUE)
  cat(sprintf("<rxSolveOom: %d chunks, %d total rows, prefix='%s'%s>\n",
              length(.m$chunks), sum(.m$nrows), .m$prefix,
              if (.arrow) " [Arrow-backed]" else ""))
  invisible(x)
}

#' Convert an rxSolveOom result to an Arrow Table
#'
#' Reads all parquet chunk files and concatenates them into a single in-memory
#' Arrow Table using \code{arrow::concat_tables()}.  Requires the \code{arrow}
#' package.
#'
#' @param x An \code{rxSolveOom} object.
#' @param ... Ignored.
#' @return An \code{arrow::Table}.
#' @export
as_arrow_table.rxSolveOom <- function(x, ...) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("package 'arrow' is required for as_arrow_table()")
  .m <- attr(x, "manifest")
  .pq <- .m$chunks[grepl("\\.parquet$", .m$chunks)]
  if (length(.pq) == 0L)
    return(arrow::as_arrow_table(as.data.frame(x)))
  # concat_tables() takes tables as individual `...` arguments, not a list,
  # so splice the per-chunk tables in with do.call().
  do.call(arrow::concat_tables, lapply(.pq, arrow::read_parquet))
}

#' Convert an rxSolveOom result to a lazy Arrow Dataset
#'
#' Opens all parquet chunk files as a single lazy \code{arrow::Dataset} using
#' \code{arrow::open_dataset()}.  The dataset can be filtered and selected with
#' dplyr verbs before calling \code{dplyr::collect()} to materialise.  Requires
#' the \code{arrow} package.
#'
#' @param x An \code{rxSolveOom} object.
#' @param ... Ignored.
#' @return An \code{arrow::Dataset}.
#' @export
as_arrow_dataset.rxSolveOom <- function(x, ...) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("package 'arrow' is required for as_arrow_dataset()")
  .m <- attr(x, "manifest")
  .pq <- .m$chunks[grepl("\\.parquet$", .m$chunks)]
  if (length(.pq) == 0L)
    stop("No parquet chunk files found. Re-run rxSolve() with the arrow package installed.")
  arrow::open_dataset(.pq)
}

#' @export
as.data.frame.rxSolveOom <- function(x, ...) {
  .m <- attr(x, "manifest")
  .total <- sum(.m$nrows)
  if (.total > 1e6)
    message(sprintf("Materializing %.0f rows into memory", .total))
  if (requireNamespace("arrow", quietly = TRUE) && .rxOomHasParquet(.m))
    return(as.data.frame(as_arrow_table.rxSolveOom(x)))
  do.call(rbind, lapply(.m$chunks, readRDS))
}

#' @export
as_tibble.rxSolveOom <- function(x, ...) {
  if (requireNamespace("arrow", quietly = TRUE) && .rxOomHasParquet(attr(x, "manifest")))
    return(tibble::as_tibble(as_arrow_table.rxSolveOom(x)))
  tibble::as_tibble(as.data.frame(x))
}

#' @export
as.data.table.rxSolveOom <- function(x, keep.rownames = FALSE, ...) {
  if (requireNamespace("arrow", quietly = TRUE) && .rxOomHasParquet(attr(x, "manifest")))
    return(data.table::as.data.table(as.data.frame(as_arrow_table.rxSolveOom(x))))
  data.table::as.data.table(as.data.frame(x), keep.rownames = keep.rownames)
}

#' @export
`$.rxSolveOom` <- function(x, name) {
  .m <- attr(x, "manifest")
  .pq <- .m$chunks[grepl("\\.parquet$", .m$chunks)]
  if (length(.pq) > 0L && requireNamespace("arrow", quietly = TRUE)) {
    .cols <- lapply(.pq, function(.f)
      arrow::read_parquet(.f, col_select = name)[[1L]])
    return(unlist(.cols, use.names = FALSE))
  }
  unlist(lapply(.m$chunks, function(.f) readRDS(.f)[[name]]), use.names = FALSE)
}

# nrow() is not an S3 generic, so a `nrow.rxSolveOom` method is never
# dispatched (and exporting it as a plain function trips an "undocumented
# code object" check).  base::nrow(x) is dim(x)[1L], and dim() *is* generic,
# so dim.rxSolveOom() below already makes nrow() return the right value.
#' @export
dim.rxSolveOom <- function(x) c(sum(attr(x, "manifest")$nrows), NA_integer_)

# ── User-facing convenience wrapper ──────────────────────────────────────────

#' Solve an ODE model in memory-safe chunks
#'
#' Splits subjects into chunks sized to fit in available RAM, solves each chunk,
#' and writes output to parquet (or rds) files. Returns an \code{rxSolveOom}
#' object that lazily reads chunks on demand.
#'
#' @param object rxode2 model
#' @param params model parameters
#' @param events event table or \code{rxEtFile}
#' @param inits initial conditions
#' @param ... additional arguments passed to \code{rxControl()}
#' @param chunkSize number of subjects per chunk (auto-computed from free RAM if omitted)
#' @param seed random seed (sets before solving if not NULL)
#' @param parallel number of mirai daemons for parallel chunk solving (0 = serial)
#' @return An \code{rxSolveOom} object
#' @export
rxSolveChunked <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                            chunkSize, seed = NULL, parallel = 0L) {
  # Normalize params/events to match rxSolve convention
  if ((is.rxEt(params) || rxIs(params, "rx.event")) && !is.rxEt(events)) {
    .tmp   <- events
    events <- params
    params <- .tmp
  }
  if (!missing(chunkSize) && !is.null(chunkSize)) {
    .chunkSize <- as.integer(chunkSize)
  } else {
    .chunkSize <- NULL
  }
  if (!is.null(seed)) rxSetSeed(seed)
  .ctl <- rxControl(...,
    file      = tempfile("rxChunk"),
    chunkSize = .chunkSize,
    parallel  = as.integer(parallel))
  .rxSolveOom(object, params = params, events = events, inits = inits,
              .ctl = .ctl, .envir = parent.frame())
}
