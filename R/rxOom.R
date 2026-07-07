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

# First n rows without materializing the whole file (rds has no lazy read).
.rxEtFileReadHead <- function(x, n = 100L) {
  switch(x$format,
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE))
        stop("package 'arrow' is required for parquet format")
      as.data.frame(utils::head(arrow::open_dataset(x$path), n))
    },
    csv     = data.table::fread(x$path, nrows = n, data.table = FALSE),
    fst     = {
      if (!requireNamespace("fst", quietly = TRUE))
        stop("package 'fst' is required for fst format")
      .n <- min(as.integer(n), fst::metadata_fst(x$path)$nrOfRows)
      fst::read_fst(x$path, from = 1L, to = .n)
    },
    rds     = utils::head(readRDS(x$path), n)
  )
}

rxMemSummary.rxEtFile <- function(x, ...) {
  .cols <- c(x$id_col, "evid")
  .dat <- .rxEtFileReadCols(x, .cols)
  .rxMemSummarizeDat(.dat)
}

# -- Main OOM solve loop -------------------------------------------------------

.rxSolveOom <- function(object, params, events, inits, .ctl, .envir = parent.frame()) {
  .prefix   <- .ctl$file
  .nDaemons <- if (is.null(.ctl$parallel)) 0L else as.integer(.ctl$parallel)
  .useMirai <- .nDaemons > 0L && requireNamespace("mirai", quietly = TRUE)

  # Normalize: if events is an rxEt but params is not, events is the params; swap
  if ((is.rxEt(params) || rxIs(params, "rx.event") || inherits(params, "rxEtFile")) &&
        !is.rxEt(events)) {
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
    paramChunks = character(.nChunks), inits = NULL,
    seed    = .baseSeed
  )
  .outFiles    <- character(.nChunks)
  .paramFiles  <- character(.nChunks)
  .cumSub      <- 0L

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
    # Strip omega from forwarded args -- etas are now baked into per-chunk params
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
    if (.rxOomHasArrow()) {
      .f <- tempfile(fileext = ".parquet")
      arrow::write_parquet(.df, .f)
    } else {
      .f <- tempfile(fileext = ".rds")
      saveRDS(.df, .f)
    }
    .f
  }

  # Persist the per-subject parameter table (res$params) for one chunk. The id
  # column is stamped from .chunkIds so the chunk param tables concatenate
  # cleanly (single-subject solves drop the id column).
  .writeParams <- function(.result, .chunkIds) {
    .pars <- tryCatch(as.data.frame(.result$params), error = function(e) NULL)
    if (is.null(.pars) || nrow(.pars) == 0L) return(NA_character_)
    if (!("id" %in% names(.pars)) && nrow(.pars) == length(.chunkIds)) {
      .pars <- cbind(id = .chunkIds, .pars)
    }
    if (.rxOomHasArrow()) {
      .f <- tempfile(fileext = ".parquet")
      arrow::write_parquet(.pars, .f)
    } else {
      .f <- tempfile(fileext = ".rds")
      saveRDS(.pars, .f)
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
    # Resolve the backend in the parent (where the rxode2.oom.backend option
    # lives) and forward it to the daemons: the option for faithful propagation,
    # and the already-resolved write decision because the daemon closure cannot
    # reach the unexported .rxOomHasArrow() helper.
    .backendOpt    <- .rxOomBackendOpt()
    .useArrowWrite <- .rxOomHasArrow()
    .tasks <- mirai::mirai_map(
      seq_len(.nChunks),
      function(.i, .modelObj, .chunkEvList, .chunkIdsList, .chunkParamsList, .inits, .fwdCtlArgs, .mainTmp, .backendOpt, .useArrowWrite) {
        library(rxode2)
        options(rxode2.oom.backend = .backendOpt)
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
        # Write to the parent process's tempdir (shared filesystem). A daemon's
        # own session tempdir is removed when the daemon shuts down, which would
        # leave the manifest pointing at deleted chunk files.
        # Per-subject parameter table; stamp id so chunks concatenate cleanly.
        .pars <- tryCatch(as.data.frame(.result$params), error = function(e) NULL)
        .ids  <- .chunkIdsList[[.i]]
        if (!is.null(.pars) && nrow(.pars) > 0L &&
            !("id" %in% names(.pars)) && nrow(.pars) == length(.ids)) {
          .pars <- cbind(id = .ids, .pars)
        }
        if (.useArrowWrite) {
          .f <- tempfile(fileext = ".parquet", tmpdir = .mainTmp)
          arrow::write_parquet(.df, .f)
          .pf <- if (!is.null(.pars) && nrow(.pars) > 0L) {
            .p <- tempfile(fileext = ".parquet", tmpdir = .mainTmp)
            arrow::write_parquet(.pars, .p)
            .p
          } else NA_character_
        } else {
          .f <- tempfile(fileext = ".rds", tmpdir = .mainTmp)
          saveRDS(.df, .f)
          .pf <- if (!is.null(.pars) && nrow(.pars) > 0L) {
            .p <- tempfile(fileext = ".rds", tmpdir = .mainTmp)
            saveRDS(.pars, .p)
            .p
          } else NA_character_
        }
        list(file = .f, nrows = nrow(.df), paramFile = .pf,
             inits = tryCatch(.result$inits, error = function(e) NULL))
      },
      .args = list(.modelObj = .modelObj,
                   .chunkEvList = .chunkEvList, .chunkIdsList = .chunkIdsList,
                   .chunkParamsList = .chunkParamsList,
                   .inits = inits, .fwdCtlArgs = .fwdCtlArgs,
                   .mainTmp = tempdir(),
                   .backendOpt = .backendOpt, .useArrowWrite = .useArrowWrite)
    )
    for (.i in seq_len(.nChunks)) {
      .r <- .tasks[[.i]][]
      if (inherits(.r, "miraiError") || inherits(.r, "errorValue") || is.null(.r$file)) {
        stop(sprintf("parallel chunk %d failed in a mirai daemon: %s", .i,
                     tryCatch(conditionMessage(.r),
                              error = function(e) paste(utils::head(unclass(.r), 1L), collapse = ""))),
             call. = FALSE)
      }
      .outFiles[.i] <- .r$file
      .paramFiles[.i] <- if (is.null(.r$paramFile)) NA_character_ else .r$paramFile
      .manifest$nrows[.i] <- .r$nrows
      if (is.null(.manifest$inits) && !is.null(.r$inits)) {
        .manifest$inits <- .r$inits
      }
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
      .outFiles[.i]   <- .writeResult(.result, .chunkIds)
      .paramFiles[.i] <- .writeParams(.result, .chunkIds)
      .manifest$nrows[.i] <- nrow(.result)
      if (is.null(.manifest$inits)) {
        .manifest$inits <- tryCatch(.result$inits, error = function(e) NULL)
      }
      .cumSub <- .cumSub + .nThis
    }
  }

  for (.i in seq_len(.nChunks)) {
    .manifest$chunks[.i] <- .outFiles[.i]
  }
  # Drop param chunks that failed to write (NA); keep only valid files.
  .manifest$paramChunks <- .paramFiles[!is.na(.paramFiles)]

  saveRDS(.manifest, paste0(.prefix, "_manifest.rds"))
  .rxSolveOomFromManifest(.manifest)
}

# -- rxSolveOom return object --------------------------------------------------

.rxSolveOomFromManifest <- function(manifest) {
  structure(list(), class = "rxSolveOom", manifest = manifest)
}

.rxOomHasParquet <- function(manifest) {
  any(grepl("\\.parquet$", manifest$chunks))
}

# -- DuckDB lazy query layer over the parquet chunks --------------------------
#
# DuckDB is preferred for lazy access (head, single column, schema) because it
# pushes the LIMIT / column projection into the parquet reader instead of
# materializing whole files.  Everything is guarded so the arrow (and rds)
# fallbacks remain when duckdb/DBI are unavailable.
#
# The backend can be pinned with the `rxode2.oom.backend` option, which makes
# every code path deterministically exercisable (e.g. in tests):
#   "auto"   - duckdb if installed, else arrow, else rds (default)
#   "duckdb" - duckdb query layer over arrow-written parquet
#   "arrow"  - arrow reads/writes, no duckdb
#   "rds"    - plain rds files, no arrow or duckdb
# A requested engine that is not installed silently degrades (duckdb -> arrow
# -> rds), so the option is a preference cap, never a hard requirement.

.rxOomBackendOpt <- function() {
  match.arg(getOption("rxode2.oom.backend", "auto"),
            c("auto", "duckdb", "arrow", "rds"))
}

.rxOomHasDuckdb <- function() {
  if (!(.rxOomBackendOpt() %in% c("auto", "duckdb"))) return(FALSE)
  requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)
}

# Wrapper around the arrow availability check.  Pulling it out (instead of
# calling requireNamespace() inline) lets the `rxode2.oom.backend` option pin
# the rds / arrow / duckdb code paths.
.rxOomHasArrow <- function() {
  if (.rxOomBackendOpt() == "rds") return(FALSE)
  requireNamespace("arrow", quietly = TRUE)
}

# parquet files for a given set of paths
.rxOomParquetFiles <- function(files) {
  files[grepl("\\.parquet$", files)]
}

# Build a DuckDB SQL list literal of parquet paths: ['a.parquet','b.parquet']
.rxOomDuckFileList <- function(files) {
  paste0("[", paste0("'", gsub("'", "''", files), "'", collapse = ", "), "]")
}

# Run a SELECT against a set of parquet files via an in-memory DuckDB and return
# a data.frame.  `sql` must reference the placeholder {tbl}, which is replaced
# with read_parquet([...]).
.rxOomDuckQuery <- function(files, sql) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .tbl <- paste0("read_parquet(", .rxOomDuckFileList(files), ")")
  DBI::dbGetQuery(con, gsub("{tbl}", .tbl, sql, fixed = TRUE))
}

# Read the persisted per-subject parameter table (res$params).
.rxOomParams <- function(manifest) {
  .pc <- manifest$paramChunks
  .pq <- .rxOomParquetFiles(.pc)
  if (length(.pq) > 0L) {
    if (.rxOomHasDuckdb()) {
      return(.rxOomDuckQuery(.pq, "SELECT * FROM {tbl}"))
    }
    if (.rxOomHasArrow()) {
      return(do.call(rbind, lapply(.pq, function(.f)
        as.data.frame(arrow::read_parquet(.f)))))
    }
  }
  do.call(rbind, lapply(.pc, readRDS))
}

.rxOomInits <- function(manifest) manifest$inits

# Backend label for the print footer.
.rxOomBackend <- function(manifest) {
  if (.rxOomHasParquet(manifest)) {
    if (.rxOomHasDuckdb()) return(" [DuckDB/Arrow-backed]")
    if (.rxOomHasArrow()) return(" [Arrow-backed]")
  }
  ""
}

#' @export
print.rxSolveOom <- function(x, ...) {
  .m <- attr(x, "manifest")
  .args <- as.list(match.call(expand.dots = TRUE))
  .n <- if (any(names(.args) == "n")) .args$n else 6L
  .bound <- .getBound(x, parent.frame(2))

  cat(cli::cli_format_method({
    .h2(crayon::bold("Solved rxode2 object"))
  }), sep = "\n")

  # Parameters (res$params)
  cat(format.boundParams(.bound), sep = "\n")
  .pars <- .rxOomParams(.m)
  if (requireNamespace("tibble", quietly = TRUE)) {
    print(tibble::as_tibble(.pars))
  } else {
    print(utils::head(.pars))
  }

  # Initial Conditions (res$inits)
  cat(format.boundInits(.bound), sep = "\n")
  print(.rxOomInits(.m))

  # First part of data (object)
  cat(cli::cli_format_method({
    .h2(crayon::bold("First part of data (object):"))
  }), sep = "\n")
  .isDplyr <- requireNamespace("tibble", quietly = TRUE) &&
    getOption("rxode2.display.tbl", TRUE)
  .head <- utils::head(x, n = .n)
  if (.isDplyr) {
    print(tibble::as_tibble(.head), n = .n)
  } else {
    print(.head)
  }

  # Footer: chunk / backend note
  cat(sprintf("<rxSolveOom: %d chunks, %d total rows, prefix='%s'%s>\n",
              length(.m$chunks), sum(.m$nrows), .m$prefix,
              .rxOomBackend(.m)))
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
#' @keywords internal
#' @noRd
as_arrow_table.rxSolveOom <- function(x, ...) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("package 'arrow' is required for as_arrow_table()")
  .m <- attr(x, "manifest")
  .pq <- .m$chunks[grepl("\\.parquet$", .m$chunks)]
  if (length(.pq) == 0L)
    return(arrow::as_arrow_table(as.data.frame(x)))
  # read_parquet() returns a tibble by default; concat_tables() needs Arrow
  # Tables, so read with as_data_frame = FALSE. concat_tables() also takes the
  # tables as individual `...` arguments, not a list, hence do.call().
  do.call(arrow::concat_tables,
          lapply(.pq, function(.f) arrow::read_parquet(.f, as_data_frame = FALSE)))
}

#' Convert an rxSolveOom result to a lazy Arrow Dataset
#'
#' Opens all parquet chunk files as a single lazy \code{arrow::Dataset} using
#' \code{arrow::open_dataset()}.  The dataset can be filtered and selected with
#' dplyr verbs before calling \code{dplyr::collect()} to materialise.  Requires
#' the \code{arrow} package.
#'
#' This is an rxode2 generic: \pkg{arrow} provides \code{open_dataset()} but no
#' lazy-dataset coercion generic to dispatch on, so rxode2 defines its own.
#'
#' @param x An \code{rxSolveOom} object.
#' @param ... Ignored.
#' @return An \code{arrow::Dataset}.
#' @export
as.arrow <- function(x, ...) UseMethod("as.arrow")

#' @rdname as.arrow
#' @export
as.arrow.rxSolveOom <- function(x, ...) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("package 'arrow' is required for as.arrow()")
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
  if (.rxOomHasArrow() && .rxOomHasParquet(.m))
    return(as.data.frame(as_arrow_table.rxSolveOom(x)))
  do.call(rbind, lapply(.m$chunks, readRDS))
}

as_tibble.rxSolveOom <- function(x, ...) {
  if (.rxOomHasArrow() && .rxOomHasParquet(attr(x, "manifest")))
    return(tibble::as_tibble(as_arrow_table.rxSolveOom(x)))
  tibble::as_tibble(as.data.frame(x))
}

#' @export
as.data.table.rxSolveOom <- function(x, keep.rownames = FALSE, ...) {
  if (.rxOomHasArrow() && .rxOomHasParquet(attr(x, "manifest")))
    return(data.table::as.data.table(as.data.frame(as_arrow_table.rxSolveOom(x))))
  data.table::as.data.table(as.data.frame(x), keep.rownames = keep.rownames)
}

#' First rows of an out-of-memory solved object
#'
#' Reads only the first \code{n} rows from the parquet (or rds) chunks,
#' preferring DuckDB (which pushes the row limit into the parquet reader) and
#' falling back to arrow / rds.
#'
#' @param x An \code{rxSolveOom} object.
#' @param n Number of rows to return.
#' @param ... Ignored.
#' @return A \code{data.frame} with the first \code{n} rows.
#' @keywords internal
#' @export
head.rxSolveOom <- function(x, n = 6L, ...) {
  .m <- attr(x, "manifest")
  .pq <- .rxOomParquetFiles(.m$chunks)
  if (length(.pq) > 0L && .rxOomHasDuckdb()) {
    return(.rxOomDuckQuery(.pq, sprintf("SELECT * FROM {tbl} LIMIT %d", as.integer(n))))
  }
  if (length(.pq) > 0L && .rxOomHasArrow()) {
    # Walk chunks until we have n rows; only the first chunk(s) are read.
    .acc <- vector("list", 0L)
    .got <- 0L
    for (.f in .pq) {
      .d <- as.data.frame(arrow::read_parquet(.f))
      .acc[[length(.acc) + 1L]] <- utils::head(.d, n - .got)
      .got <- .got + nrow(.acc[[length(.acc)]])
      if (.got >= n) break
    }
    return(do.call(rbind, .acc))
  }
  .acc <- vector("list", 0L)
  .got <- 0L
  for (.f in .m$chunks) {
    .d <- readRDS(.f)
    .acc[[length(.acc) + 1L]] <- utils::head(.d, n - .got)
    .got <- .got + nrow(.acc[[length(.acc)]])
    if (.got >= n) break
  }
  do.call(rbind, .acc)
}

#' @export
`$.rxSolveOom` <- function(x, name) {
  .m <- attr(x, "manifest")
  if (name %in% c("params", "par", "pars", "param")) {
    return(.rxOomParams(.m))
  }
  if (name %in% c("inits", "init")) {
    return(.rxOomInits(.m))
  }
  .pq <- .rxOomParquetFiles(.m$chunks)
  if (length(.pq) > 0L && .rxOomHasDuckdb()) {
    .r <- .rxOomDuckQuery(.pq, sprintf('SELECT "%s" FROM {tbl}', gsub('"', '""', name)))
    return(.r[[1L]])
  }
  if (length(.pq) > 0L && .rxOomHasArrow()) {
    .cols <- lapply(.pq, function(.f)
      arrow::read_parquet(.f, col_select = name)[[1L]])
    return(unlist(.cols, use.names = FALSE))
  }
  unlist(lapply(.m$chunks, function(.f) readRDS(.f)[[name]]), use.names = FALSE)
}

# Column count from the first chunk's schema (cheap: header-only for parquet).
.rxOomNcol <- function(manifest) {
  .pq <- .rxOomParquetFiles(manifest$chunks)
  if (length(.pq) > 0L && .rxOomHasDuckdb()) {
    return(nrow(.rxOomDuckQuery(.pq[1L], "DESCRIBE SELECT * FROM {tbl}")))
  }
  if (length(.pq) > 0L && .rxOomHasArrow()) {
    return(length(arrow::open_dataset(.pq[1L])$schema$names))
  }
  if (length(manifest$chunks) > 0L) return(ncol(readRDS(manifest$chunks[1L])))
  NA_integer_
}

# nrow() is not an S3 generic, so a `nrow.rxSolveOom` method is never
# dispatched (and exporting it as a plain function trips an "undocumented
# code object" check).  base::nrow(x) is dim(x)[1L], and dim() *is* generic,
# so dim.rxSolveOom() below already makes nrow() return the right value.
#' @export
dim.rxSolveOom <- function(x) {
  .m <- attr(x, "manifest")
  c(sum(.m$nrows), .rxOomNcol(.m))
}

# -- User-facing convenience wrapper ------------------------------------------

#' Solve an ODE model in memory-safe chunks
#'
#' Splits subjects into chunks sized to fit in available RAM, solves each chunk,
#' and writes output to parquet (or rds) files. Returns an \code{rxSolveOom}
#' object that lazily reads chunks on demand.
#'
#' The storage/query engine used by the resulting \code{rxSolveOom} object is
#' controlled by the \code{rxode2.oom.backend} option:
#' \describe{
#'   \item{\code{"auto"}}{(default) DuckDB if installed, else arrow, else rds.}
#'   \item{\code{"duckdb"}}{lazy DuckDB SQL queries over arrow-written parquet.}
#'   \item{\code{"arrow"}}{arrow parquet reads/writes, no DuckDB.}
#'   \item{\code{"rds"}}{plain rds files, no arrow or DuckDB.}
#' }
#' A requested engine that is not installed silently degrades
#' (duckdb \eqn{\to} arrow \eqn{\to} rds).
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
  if ((is.rxEt(params) || rxIs(params, "rx.event") || inherits(params, "rxEtFile")) &&
        !is.rxEt(events)) {
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
