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
  .fwdCtlArgs$oomFile       <- NULL
  .fwdCtlArgs$oomChunkSize  <- NULL
  .fwdCtlArgs$oomParallel   <- NULL
  .fwdCtlArgs$serializeFile <- NULL

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
    .chunkSeeds <- integer(.nChunks)
    .chunkEvList <- vector("list", .nChunks)
    for (.i in seq_len(.nChunks)) {
      .chunkSeeds[.i] <- as.integer(
        (as.double(.baseSeed) + as.double(.cumSub)) %% .Machine$integer.max
      )
      .chunkEvList[[.i]] <- .extractChunkEvents(.chunkList[[.i]])
      .cumSub <- .cumSub + length(.chunkList[[.i]])
    }
    .chunkIdsList <- .chunkList
    .tasks <- mirai::mirai_map(
      seq_len(.nChunks),
      function(.i) {
        library(rxode2)
        rxSetSeed(.chunkSeeds[.i])
        .result <- do.call(rxSolve,
                           c(list(object = .modelObj, params = .params,
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
      .args = list(.chunkSeeds = .chunkSeeds, .chunkEvList = .chunkEvList,
                   .chunkIdsList = .chunkIdsList,
                   .params = params, .inits = inits, .fwdCtlArgs = .fwdCtlArgs)
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
      rxSetSeed(as.integer(
        (as.double(.baseSeed) + as.double(.cumSub)) %% .Machine$integer.max
      ))
      .chunkEvents <- .extractChunkEvents(.chunkIds)
      .result <- do.call(rxSolve,
                         c(list(object = object, params = params,
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
