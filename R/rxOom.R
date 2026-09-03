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

# The per study omega/sigma draws are written by the C++ side into the shared
# `.rxModels` environment rather than returned, so a chunked solve has to read
# them in the parent -- the only process that ran the draw.  Missing is normal
# (no draw was made), so this returns NULL rather than erroring.
.rxOomDrawnList <- function(what) {
  .e <- .rxModels
  if (!is.environment(.e) || !exists(what, envir=.e, inherits=FALSE)) return(NULL)
  .l <- get(what, envir=.e, inherits=FALSE)
  if (is.null(.l) || length(.l) == 0L) return(NULL)
  .l
}

# Drop what an earlier solve in this session left in `.rxModels`, so what the
# pre-draw reads back is what the pre-draw itself made.
.rxOomClearDrawn <- function(what) {
  .e <- .rxModels
  if (!is.environment(.e)) return(invisible())
  for (.w in what) {
    if (exists(.w, envir=.e, inherits=FALSE)) rm(list=.w, envir=.e)
  }
  invisible()
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

  # `nSub` replicates a single subject event table into that many subjects.
  # The chunking cannot express it: chunks are cut by the ids the event table
  # actually has, and the pre-draw is sized the same way, so the solve came
  # back with one subject where `nSub` were asked for -- and with a `thetaMat`
  # or `omega`, that one subject's draw rather than `nSub` of them.
  if (!is.null(.ctl$nSub) && length(.ctl$nSub) == 1L && !is.na(.ctl$nSub) &&
        .ctl$nSub > 1L && .ctl$nSub != .nSub) {
    stop("a chunked solve ('file='/'chunkSize=') cannot simulate 'nSub' (",
         .ctl$nSub, ") subjects from an event table that has ", .nSub,
         "; chunks are cut by subject, so give the event table one record ",
         "set per subject ('et(id=1:", .ctl$nSub, ")'), or solve without ",
         "chunking.",
         call.=FALSE)
  }

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
  # The pre-draw covers every study as well as every subject, so `nStud > 1`
  # draws its omega uncertainty here once rather than in each chunk -- which
  # is what makes it right: a per-chunk draw would give each chunk its own
  # study omegas, so subjects in different chunks would not share a study.
  .nStud <- if (!is.null(.ctl$nStud) && length(.ctl$nStud) == 1L &&
                  !is.na(.ctl$nStud) && .ctl$nStud > 1L) {
    as.integer(.ctl$nStud)
  } else {
    1L
  }

  .preDrawnParams <- NULL
  .preDrawnOmegaL <- NULL
  .preDrawnSigmaL <- NULL
  .preDrawnTheta  <- NULL

  # The pre-draw is study major: all `nSub` subjects of study 1, then of study
  # 2, and so on -- the same layout `rxSimThetaOmega()` gives the unchunked
  # solve.  A chunk holds a contiguous run of SUBJECTS, so its rows are one
  # stride per study rather than one contiguous block.  Taking the contiguous
  # block instead would hand the later chunks another study's etas.
  .preDrawnSlice <- function(.first, .n) {
    as.integer(vapply(seq_len(.nStud) - 1L,
                      function(.s) .s * .nSub + seq.int(.first, length.out=.n),
                      double(.n)))
  }
  # `dfObs` is what turns the sigma uncertainty draw on.  The pre-draw does not
  # cover the residual draw -- it is per observation, not per subject, so it is
  # not something a chunk's slice of the parameter table can carry -- and
  # `sigma` is therefore still forwarded, so each chunk would draw its OWN per
  # study sigma and subjects in different chunks would end up with different
  # residual covariance inside the same study.  Refuse it rather than answer
  # wrongly, as a chunked solve already does for the draws it cannot share.
  if (!is.null(.ctl$sigma) && !is.null(.ctl$dfObs) &&
        length(.ctl$dfObs) == 1L && !is.na(.ctl$dfObs) && .ctl$dfObs > 0) {
    stop("a chunked solve ('file='/'chunkSize=') cannot simulate sigma ",
         "uncertainty ('dfObs' > 0): each chunk would draw its own per study ",
         "sigma, so subjects in different chunks would not share a study.  ",
         "Solve with 'dfObs=0', or without chunking.",
         call.=FALSE)
  }

  # A joint (TNPRI) draw carries the omega/sigma entries in the `thetaMat` and
  # draws them with the thetas.  The pre-draw below goes through the exported
  # `rxSimThetaOmega()`, which has no argument for any of that, so the chunks
  # would come back drawn from the point estimate omega with the joint draw
  # gone and nothing to signal it.  Refuse it rather than answer wrongly.
  # `omegaSeparation`/`sigmaSeparation` are asked directly as well as through
  # the `priorOmegaEl`/`priorSigmaEl` they resolve to: `rxSolveChunked()` builds
  # its control itself and never runs `.rxTnpriApplyControl()`, so the resolved
  # form is not there to test.
  if (!is.null(.ctl$priorOmega) || !is.null(.ctl$priorOmegaEl) ||
        !is.null(.ctl$priorSigmaEl) ||
        identical(.ctl$omegaSeparation, "tnpri") ||
        identical(.ctl$sigmaSeparation, "tnpri")) {
    stop("a chunked solve ('file='/'chunkSize=') cannot draw the omega/sigma ",
         "entries a 'thetaMat' carries ('omegaSeparation=\"tnpri\"', ",
         "'sigmaSeparation=\"tnpri\"', or a prior on an omega block): the ",
         "one draw every chunk shares cannot express them, so they would be ",
         "dropped without warning.  Solve without chunking.",
         call.=FALSE)
  }

  # `thetaMat` is drawn here for the same reason omega is, and more sharply:
  # the pre-draw hands each chunk a parameter data frame, and `rxSolve()`
  # refuses a `thetaMat` alongside one, so a forwarded `thetaMat` did not
  # merely draw the wrong thing -- it killed the solve outright.  Drawing per
  # chunk would be wrong even where it ran, since each chunk would get its own
  # thetas and subjects in different chunks would no longer share a study.
  # That is nlmixr2/rxode2#1263.
  if (!is.null(.ctl$omega) || !is.null(.ctl$thetaMat)) {
    # The draw is made from a named parameter vector -- that is all
    # `rxSimThetaOmega()` takes, and it is what the chunks are sliced out of.
    # A per-subject parameter data frame reached it as an opaque coercion
    # error ("Not compatible with requested type"); say what happened instead.
    # `rxSolve()` refuses the `thetaMat` half of this unchunked as well.
    if (is.data.frame(params) || is.matrix(params)) {
      stop("a chunked solve ('file='/'chunkSize=') cannot draw an 'omega'/",
           "'thetaMat' when the parameters are a 'data.frame'/'matrix'; the ",
           "one draw every chunk shares is made from a named parameter ",
           "vector.  Solve without chunking.",
           call.=FALSE)
    }
    .ncores <- if (!is.null(.ctl$cores) && .ctl$cores > 0L) {
      as.integer(.ctl$cores)
    } else {
      getRxThreads()
    }
    .rxOomClearDrawn(c(".omegaL", ".sigmaL", ".theta"))
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
      # the theta draw is one row per study, added into the parameter columns
      # it names, so it has to happen in the same call as the omega draw: it
      # is the same `params` table the chunks are sliced out of, and running
      # it separately would also take the RNG out of the order the unchunked
      # solve draws in
      thetaMat        = .ctl$thetaMat,
      thetaLower      = if (!is.null(.ctl$thetaLower))  .ctl$thetaLower  else -Inf,
      thetaUpper      = if (!is.null(.ctl$thetaUpper))  .ctl$thetaUpper  else  Inf,
      thetaDf         = .ctl$thetaDf,
      thetaIsChol     = if (!is.null(.ctl$thetaIsChol)) .ctl$thetaIsChol else FALSE,
      nCoresRV        = 1L,
      nStud           = .nStud,
      # `dfSub` is what turns the omega uncertainty draw on, so the pre-draw
      # has to carry it or `nStud > 1` would still come back with every study
      # sharing the point estimate omega
      dfSub           = if (!is.null(.ctl$dfSub)) .ctl$dfSub else 0,
      simVariability  = if (!is.null(.ctl$simVariability)) .ctl$simVariability else NA
    )
    # The drawn per study omegas live in the shared `.rxModels` environment that
    # the C++ side writes.  They are read here, in the parent, because that is
    # the only process that ran the draw -- a chunk never sees them, so without
    # this `$omegaList`/`$sigmaList`/`$thetaMat` would come back empty on a
    # chunked solve while a plain one reports them.
    .preDrawnOmegaL <- .rxOomDrawnList(".omegaL")
    .preDrawnSigmaL <- .rxOomDrawnList(".sigmaL")
    .preDrawnTheta  <- .rxOomDrawnList(".theta")
    if (!is.null(.ctl$omega)) {
      # Strip omega from forwarded args -- etas are now baked into per-chunk params
      .fwdCtlArgs$omega           <- NULL
      .fwdCtlArgs$omegaDf         <- NULL
      .fwdCtlArgs$omegaLower      <- NULL
      .fwdCtlArgs$omegaUpper      <- NULL
      .fwdCtlArgs$omegaIsChol     <- NULL
      .fwdCtlArgs$omegaSeparation <- NULL
      .fwdCtlArgs$omegaXform      <- NULL
    }
    if (!is.null(.ctl$thetaMat)) {
      # Likewise for thetaMat -- the drawn thetas are baked into the per-chunk
      # parameter table, and forwarding it would have each chunk draw its own
      # on top of them (where it did not simply error out)
      .fwdCtlArgs$thetaMat    <- NULL
      .fwdCtlArgs$thetaDf     <- NULL
      .fwdCtlArgs$thetaLower  <- NULL
      .fwdCtlArgs$thetaUpper  <- NULL
      .fwdCtlArgs$thetaIsChol <- NULL
    }
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
        .preDrawnParams[.preDrawnSlice(.cumSub + 1L, .nThis), , drop = FALSE]
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
    .droppedCtl <- character(0)
    .daemonVer  <- character(0)
    .tasks <- mirai::mirai_map(
      seq_len(.nChunks),
      function(.i, .modelObj, .chunkEvList, .chunkIdsList, .chunkParamsList, .inits, .fwdCtlArgs, .mainTmp, .backendOpt, .useArrowWrite) {
        library(rxode2)
        options(rxode2.oom.backend = .backendOpt)
        # A daemon is a separate R process that loads its OWN rxode2, which need
        # not be the build the parent is running: a source checkout under
        # pkgload::load_all(), a library updated underneath a long-lived pool, or
        # simply a parent newer than the library the daemons find.  rxSolve()
        # rejects a control argument it has no formal for ("unused argument"),
        # which loses the whole chunk over a setting that version had no notion
        # of.  Drop those and report them back rather than fail.
        #
        # What the daemon accepts is asked OF THE DAEMON -- its own formals plus
        # whatever its own rxControl() produces, which is by construction a set
        # of names it round-trips.  Reimplementing rxSolve's acceptance rule here
        # would drift, and the failure mode of drift is the bad one: dropping an
        # argument a matching version would have honoured, and silently solving
        # a chunk under different settings than were asked for.
        .accept <- union(names(formals(rxSolve)), names(rxControl()))
        .dropped <- setdiff(names(.fwdCtlArgs), .accept)
        if (length(.dropped) > 0L) {
          .fwdCtlArgs <- .fwdCtlArgs[!(names(.fwdCtlArgs) %in% .dropped)]
        }
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
             dropped = .dropped,
             rxVersion = as.character(utils::packageVersion("rxode2")),
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
      .droppedCtl <- unique(c(.droppedCtl, .r$dropped))
      if (!is.null(.r$rxVersion)) .daemonVer <- unique(c(.daemonVer, .r$rxVersion))
      if (is.null(.manifest$inits) && !is.null(.r$inits)) {
        .manifest$inits <- .r$inits
      }
    }
    # Never silent: the chunks were solved under different settings than were
    # asked for, and which ones is not something a user could work out from the
    # result.
    if (length(.droppedCtl) > 0L) {
      warning(sprintf(
        "parallel chunks ignored %s: the rxode2 the mirai daemons loaded (%s) does not have %s",
        paste0("'", .droppedCtl, "'", collapse = ", "),
        paste(.daemonVer, collapse = ", "),
        if (length(.droppedCtl) == 1L) "it" else "them"),
        call. = FALSE)
    }
  } else {
    for (.i in seq_len(.nChunks)) {
      .chunkIds <- .chunkList[[.i]]
      .nThis    <- length(.chunkIds)
      .chunkEvents <- .extractChunkEvents(.chunkIds)
      .chunkParams <- if (!is.null(.preDrawnParams)) {
        .preDrawnParams[.preDrawnSlice(.cumSub + 1L, .nThis), , drop = FALSE]
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

  # `$omegaList`/`$sigmaList` are what tell a user the between study
  # variability was actually simulated, so a chunked solve has to report them
  # like a plain one does
  .manifest$omegaList <- .preDrawnOmegaL
  .manifest$sigmaList <- .preDrawnSigmaL
  # `$thetaMat` is the drawn thetas, one row per study -- the same thing a
  # plain solve reports
  .manifest$thetaMat  <- .preDrawnTheta

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
  .ret <- if (length(.pq) > 0L && .rxOomHasDuckdb()) {
    .rxOomDuckQuery(.pq, "SELECT * FROM {tbl}")
  } else if (length(.pq) > 0L && .rxOomHasArrow()) {
    do.call(rbind, lapply(.pq, function(.f) as.data.frame(arrow::read_parquet(.f))))
  } else {
    do.call(rbind, lapply(.pc, readRDS))
  }
  .rxOomOrderParams(.ret)
}

# Chunks are concatenated in chunk order, which is subject major: chunk 1 holds
# its subjects across every study, then chunk 2 does.  An unchunked solve
# reports the parameter table study major instead, so ordering here is what
# keeps `$params` the same table either way -- the rows are already identical.
.rxOomOrderParams <- function(pars) {
  if (is.null(pars) || !is.data.frame(pars) || nrow(pars) == 0L) return(pars)
  if (!all(c("sim.id", "id") %in% names(pars))) return(pars)
  .o <- order(pars[["sim.id"]], pars[["id"]])
  if (identical(.o, seq_len(nrow(pars)))) return(pars)
  .ret <- pars[.o, , drop = FALSE]
  rownames(.ret) <- NULL
  .ret
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
  ## the per study draws come off the manifest rather than the chunk files:
  ## they are one matrix per study, not a column to read back out of the rows
  if (name == "omegaList") {
    return(.m$omegaList)
  }
  if (name == "sigmaList") {
    return(.m$sigmaList)
  }
  if (name == "thetaMat" || name == "theta.mat") {
    return(.m$thetaMat)
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
