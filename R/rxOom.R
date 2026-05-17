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
  .prefix   <- .ctl$oomFile
  .nDaemons <- if (is.null(.ctl$oomParallel)) 0L else as.integer(.ctl$oomParallel)
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
  .chunkSize <- if (!is.null(.ctl$oomChunkSize)) {
    as.integer(.ctl$oomChunkSize)
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
  .allIds   <- .summary$id
  .nSub     <- length(.allIds)
  .nChunks  <- ceiling(.nSub / .chunkSize)
  .chunkList <- split(.allIds, ceiling(seq_len(.nSub) / .chunkSize))

  .manifest <- list(
    version = 1L, prefix = .prefix,
    chunks  = character(.nChunks), nrows = integer(.nChunks),
    seed    = .baseSeed
  )
  .binFiles <- character(.nChunks)
  .cumSub   <- 0L

  # ── Sub-phase A: serialize each chunk ──────────────────────────────────────
  for (.i in seq_len(.nChunks)) {
    .chunkIds <- .chunkList[[.i]]
    .nThis    <- length(.chunkIds)

    # Advance seed to correct position for this chunk (see par_solve.cpp seed arithmetic)
    rxSetSeed(as.integer(
      (as.double(.baseSeed) + as.double(.cumSub)) %% .Machine$integer.max
    ))

    # Extract chunk events
    .chunkEvents <- if (inherits(events, "rxEtFile")) {
      .rxEtFileReadChunk(events, .chunkIds)
    } else {
      .evDf  <- if (is.rxEt(events)) as.data.frame(events) else as.data.frame(events)
      .idCol <- grep("^id$", names(.evDf), ignore.case = TRUE, value = TRUE)[1]
      .evDf[.evDf[[.idCol]] %in% .chunkIds, , drop = FALSE]
    }

    .binFile <- sprintf("%s_chunk_%05d.rxbin", .prefix, .i)

    # Unpack rxControl into individual named args for do.call; override oom and serialize fields
    .serCtlArgs <- as.list(.ctl)
    .serCtlArgs$oomFile       <- NULL
    .serCtlArgs$oomChunkSize  <- NULL
    .serCtlArgs$oomParallel   <- NULL
    .serCtlArgs$serializeFile <- .binFile

    do.call(rxSolve, c(list(object = object, params = params,
                             events = .chunkEvents, inits = inits,
                             envir = .envir), .serCtlArgs))

    .binFiles[.i] <- .binFile
    .cumSub <- .cumSub + .nThis
  }

  # ── Sub-phase B: solve each chunk and write output ─────────────────────────
  .solveChunk <- function(.binFile) {
    .result  <- rxSolve(object, .binFile, envir = .envir)
    .parquet <- sub("\\.rxbin$", ".parquet", .binFile)
    if (requireNamespace("arrow", quietly = TRUE)) {
      arrow::write_parquet(as.data.frame(.result), .parquet)
    } else {
      .parquet <- sub("\\.parquet$", ".rds", .parquet)
      saveRDS(as.data.frame(.result), .parquet)
    }
    .parquet
  }

  if (.useMirai) {
    .modelObj <- if (inherits(object, c("rxode2", "rxDll"))) object else rxode2(object)
    mirai::daemons(.nDaemons)
    on.exit(mirai::daemons(0), add = TRUE)
    .tasks <- mirai::mirai_map(.binFiles, function(.binFile) {
      library(rxode2)
      .result  <- rxSolve(.modelObj, .binFile)
      .parquet <- sub("\\.rxbin$", ".parquet", .binFile)
      if (requireNamespace("arrow", quietly = TRUE)) {
        arrow::write_parquet(as.data.frame(.result), .parquet)
      } else {
        .parquet <- sub("\\.parquet$", ".rds", .parquet)
        saveRDS(as.data.frame(.result), .parquet)
      }
      .parquet
    }, .args = list(.modelObj = .modelObj))
    .parquetFiles <- vapply(.tasks, function(m) m[], character(1))
  } else {
    .parquetFiles <- vapply(.binFiles, .solveChunk, character(1))
  }

  # Record row counts in manifest
  for (.i in seq_len(.nChunks)) {
    .manifest$chunks[.i] <- .parquetFiles[.i]
    if (requireNamespace("arrow", quietly = TRUE) &&
        grepl("\\.parquet$", .parquetFiles[.i])) {
      .pf <- arrow::read_parquet(.parquetFiles[.i], as_data_frame = FALSE)
      .manifest$nrows[.i] <- nrow(.pf)
    } else if (grepl("\\.rds$", .parquetFiles[.i])) {
      .manifest$nrows[.i] <- nrow(readRDS(.parquetFiles[.i]))
    }
  }

  saveRDS(.manifest, paste0(.prefix, "_manifest.rds"))
  .rxSolveOomFromManifest(.manifest)
}

# ── rxSolveOom return object ──────────────────────────────────────────────────

.rxSolveOomFromManifest <- function(manifest) {
  structure(list(), class = "rxSolveOom", manifest = manifest)
}

#' @export
print.rxSolveOom <- function(x, ...) {
  .m <- attr(x, "manifest")
  cat(sprintf("<rxSolveOom: %d chunks, %d total rows, prefix='%s'>\n",
              length(.m$chunks), sum(.m$nrows), .m$prefix))
  invisible(x)
}

#' @export
as.data.frame.rxSolveOom <- function(x, ...) {
  .m <- attr(x, "manifest")
  .total <- sum(.m$nrows)
  if (.total > 1e6)
    message(sprintf("Materializing %.0f rows into memory", .total))
  .chunks <- lapply(.m$chunks, function(.f) {
    if (grepl("\\.parquet$", .f)) as.data.frame(arrow::read_parquet(.f))
    else readRDS(.f)
  })
  do.call(rbind, .chunks)
}

#' @export
`$.rxSolveOom` <- function(x, name) {
  .m <- attr(x, "manifest")
  .cols <- lapply(.m$chunks, function(.f) {
    if (grepl("\\.parquet$", .f))
      as.data.frame(arrow::read_parquet(.f, col_select = name))[[1L]]
    else
      readRDS(.f)[[name]]
  })
  unlist(.cols, use.names = FALSE)
}

#' @export
nrow.rxSolveOom <- function(x) sum(attr(x, "manifest")$nrows)

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
    oomFile      = tempfile("rxChunk"),
    oomChunkSize = .chunkSize,
    oomParallel  = as.integer(parallel))
  .rxSolveOom(object, params = params, events = events, inits = inits,
              .ctl = .ctl, .envir = parent.frame())
}
