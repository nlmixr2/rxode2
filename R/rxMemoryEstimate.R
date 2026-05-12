#' Is the dataset a memory summary?'
#'
#' @param dat data set
#' @return TRUE if the dataset is an \code{rxMemSummary}, FALSE otherwise
#' @noRd
#' @author Matthew L. Fidler
.isRxMemSummary <- function(dat) {
  inherits(dat, "rxMemSummary") ||
    (is.data.frame(dat) &&
       all(c("nobs", "ndoses") %in% names(dat)) &&
       !("evid" %in% names(dat)))
}

#' Create a per-ID event summary for memory estimation
#'
#' @param id   Integer or character vector of subject IDs (optional).
#' @param nobs Integer vector of observation counts per ID.
#' @param ndoses Integer vector of dose event counts per ID.
#' @return A data.frame with class \code{"rxMemSummary"}.
#' @examples
#'
#' # Three subjects with known observation and dose counts
#' rxMemSummary(nobs = c(48L, 96L, 48L), ndoses = c(7L, 14L, 7L))
#'
#' # Explicit subject IDs
#' rxMemSummary(nobs = c(10L, 20L), ndoses = c(5L, 5L), id = c(101L, 102L))
#' @export
rxMemSummary <- function(nobs, ndoses, id = seq_along(nobs)) {
  .ret <- data.frame(id = id, nobs = as.integer(nobs), ndoses = as.integer(ndoses))
  class(.ret) <- c("rxMemSummary", "data.frame")
  .ret
}
#' Summarize data for memory estimation from an event-table dataset
#'
#' It will be sent rxMemSummarizeDat()
#'
#' @param dat data to summarize
#' @return An \code{rxMemSummary} data.frame with columns \code{id}, \code{nobs}, and
#'  \code{ndoses}.
#' @noRd
#' @author Matthew L. Fidler
.rxMemSummarizeDat <- function(dat) {
  .dt    <- data.table::as.data.table(dat)
  .idCol <- grep("^id$", names(.dt), ignore.case = TRUE, value = TRUE)[1]

  if (is.na(.idCol)) {
    .ret <- rxMemSummary(
      nobs   = sum(.dt[["evid"]] == 0L, na.rm = TRUE),
      ndoses = sum(.dt[["evid"]] != 0L, na.rm = TRUE)
    )
  } else {
    .agg <- .dt[, .(nobs = sum(evid == 0L, na.rm = TRUE),
                    ndoses = sum(evid != 0L, na.rm = TRUE)),
                by = .idCol]
    .ret <- rxMemSummary(id = .agg[[.idCol]], nobs = .agg$nobs, ndoses = .agg$ndoses)
  }
  .ret
}
#' Extract model dimensions from model variables
#'
#'
#' @param model model variables to extract dimensions from
#'
#' @return A list with elements \code{neq}, \code{stateSize},
#'   \code{nlhs}, \code{npars}, \code{extraCmt}, \code{linB},
#'   \code{nMtime}, \code{nLlik}, and \code{nIndSim}. All are used to
#'   calculate memory useage.
#' @noRd
#' @author Matthew L. Fidler
.rxMemExtractModel <- function(model) {
  .mv    <- rxModelVars(model)
  .flags <- .mv[["flags"]]

  list(
    neq       = length(.mv[["state"]]),
    stateSize = length(.mv[["state"]]),
    nlhs      = length(.mv[["lhs"]]),
    npars     = length(.mv[["params"]]),
    extraCmt  = as.integer(.mv[["extraCmt"]]),
    linB      = as.integer(.flags["linB"]),
    nMtime    = as.integer(.mv[["nMtime"]]),
    nLlik     = as.integer(.flags["nLlik"]),
    nIndSim   = as.integer(.flags["nIndSim"])
  )
}
#' Extract memory-relevant options from an rxControl object
#'
#' @param ctrl An \code{rxControl} object.
#' @return A list with \code{cores}, \code{nsim}, \code{neta}, \code{neps},
#'   \code{nLlikAlloc}, and \code{nSubEff} (effective subject count;
#'   0 means use the data-derived count).
#' @noRd
#' @author Matthew L. Fidler
.rxMemExtractControl <- function(ctrl) {
  .cores <- max(1L, as.integer(ctrl$cores))
  .nsim  <- if (is.null(ctrl$nsim)) 1L else as.integer(ctrl$nsim)
  .neta  <- if (is.matrix(ctrl$omega)) nrow(ctrl$omega) else 0L
  .neps  <- if (is.matrix(ctrl$sigma)) nrow(ctrl$sigma) else 0L
  .nSub  <- if (is.null(ctrl$nSub))  1L else as.integer(ctrl$nSub)
  .nStud <- if (is.null(ctrl$nStud)) 1L else as.integer(ctrl$nStud)
  list(
    cores      = .cores,
    nsim       = .nsim,
    neta       = as.integer(.neta),
    neps       = as.integer(.neps),
    nLlikAlloc = ctrl$nLlikAlloc,
    nSub       = .nSub,
    nStud      = .nStud
  )
}
#' Detect total physical RAM in bytes
#'
#' @return Numeric bytes of total RAM, or \code{NA_real_} if unavailable.
#' @noRd
#' @author Matthew L. Fidler
.getRamBytes <- function() {
  if (requireNamespace("memuse", quietly = TRUE)) {
    .info <- tryCatch(memuse::Sys.meminfo(), error = function(e) NULL)
    if (!is.null(.info)) {
      return(as.numeric(memuse::mu(.info$totalram, unit = "B")))
    }
  }
  tryCatch({
    if (file.exists("/proc/meminfo")) {
      .m <- grep("^MemTotal:", readLines("/proc/meminfo", n = 10L), value = TRUE)
      if (length(.m)) return(as.numeric(gsub("\\D", "", .m[1L])) * 1024)
    }
    if (.Platform$OS.type == "windows") {
      return(utils::memory.limit() * 1024^2)
    }
    .out <- system("sysctl -n hw.memsize", intern = TRUE, ignore.stderr = TRUE)
    if (length(.out) && nzchar(.out[1L])) return(as.numeric(.out[1L]))
    NA_real_
  }, error = function(e) NA_real_)
}

#' Estimate memory required by rxSolve() for a given dataset and model
#'
#' Accepts either a pre-summarised per-ID table (an \code{\link{rxMemSummary}}
#' or any data.frame with \code{nobs} and \code{ndoses} columns) or a full
#' event-table data.frame with an \code{evid} column.  Model dimensions can
#' be supplied via a compiled rxode2 model object or overridden individually.
#'
#' The byte counts are computed by \code{rxMemoryComponents_()} which calls
#' the same \code{rxFillMemLayout()} used by the real allocator, so any
#' change to the allocation formulas propagates here automatically.
#'
#' @param dat A \code{\link{rxMemSummary}}, a data.frame with
#'   \code{nobs}/\code{ndoses} columns, or a full event-table
#'   data.frame with an \code{evid} column.
#' @param model Optional rxode2 model object.  When supplied,
#'   \code{neq}, \code{nlhs}, \code{npars}, \code{extraCmt},
#'   \code{linB}, \code{nMtime}, \code{nLlik}, and \code{nIndSim} are
#'   extracted automatically.
#' @param control Optional \code{\link{rxControl}} object.  When
#'   supplied, \code{cores}, \code{nsim}, \code{neta} (from
#'   \code{omega}), \code{neps} (from \code{sigma}), and \code{nLlik}
#'   (adjusted by \code{nLlikAlloc}) are overridden automatically.
#' @param neq Number of ODE states.
#' @param stateSize Effective \code{state.size()} seen by the solver.
#'   Equals \code{neq} for pure ODE models; may differ for linCmt-only
#'   models.  Defaults to \code{neq}.
#' @param nlhs Number of LHS (calculated) output variables.
#' @param npars Number of model parameters (drives \code{gpars} size).
#' @param neta Number of random effects (etas).
#' @param neps Number of residual-error levels (epsilons).
#' @param ncov Number of time-varying covariates.
#' @param nsim Number of simulations.
#' @param cores Number of parallel OMP threads.
#' @param nMtime Number of model measurement times.
#' @param extraCmt Extra compartments (0, 1 = depot, 2 =
#'   depot+central).
#' @param linB \code{TRUE}/\code{1} if using a linear-compartment
#'   model.
#' @param nLlik Number of log-likelihood terms (FOCEi use).
#' @param nIndSim Per-individual simulation count.  Defaults to
#'   \code{neta + neps} when not supplied explicitly.
#' @param numLinSens Number of linear sensitivity parameters (FOCEi +
#'   linCmt).
#' @param numLin Number of linear compartment terms (FOCEi + linCmt).
#' @return A named list of class \code{"rxMemoryEstimate"} whose
#'   elements are raw byte counts plus \code{ramBytes}, \code{total},
#'   \code{sizeofInd}, and \code{rxLlikSaveSize}.
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' mod <- rxode2::rxode2({
#'   d/dt(depot)  <- -ka * depot
#'   d/dt(center) <- ka * depot - cl / v * center
#'   cp           <- center / v
#' })
#'
#' ev <- rxode2::et(amt = 100, ii = 24, until = 168) |>
#'   rxode2::et(seq(0, 168, by = 1))
#'
#' # Basic estimate from event table and model
#' rxMemoryEstimate(as.data.frame(ev), model = mod)
#'
#' # With rxControl: population simulation with omega and 4 cores
#' ctrl <- rxode2::rxControl(
#'   cores = 4L,
#'   omega = lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.04)
#' )
#' rxMemoryEstimate(as.data.frame(ev), model = mod, control = ctrl)
#'
#' }
rxMemoryEstimate <- function(
  dat,
  model     = NULL,
  control   = NULL,
  neq       = 1L,
  stateSize = neq,
  nlhs      = 0L,
  npars     = neq,
  neta      = 0L,
  neps      = 0L,
  ncov      = 0L,
  nsim      = 1L,
  cores     = 0L,
  nMtime    = 0L,
  extraCmt  = 0L,
  linB      = FALSE,
  nLlik     = 0L,
  nIndSim   = NULL,
  numLinSens = 0L,
  numLin    = 0L) {
  if (.isRxMemSummary(dat)) {
    .summary <- dat
    if (!inherits(.summary, "rxMemSummary")) {
      class(.summary) <- c("rxMemSummary", "data.frame")
    }
  } else if (is.data.frame(dat) && "evid" %in% names(dat)) {
    .summary <- .rxMemSummarizeDat(dat)
  } else {
    stop("'dat' must be an rxMemSummary, a data.frame with 'nobs'/'ndoses' ",
         "columns, or a data.frame with an 'evid' column")
  }

  if (!is.null(model)) {
    .mi       <- .rxMemExtractModel(model)
    neq       <- .mi$neq
    stateSize <- .mi$stateSize
    nlhs      <- .mi$nlhs
    npars     <- .mi$npars
    extraCmt  <- .mi$extraCmt
    linB      <- .mi$linB
    nMtime    <- .mi$nMtime
    nLlik     <- .mi$nLlik
    if (is.null(nIndSim)) nIndSim <- .mi$nIndSim
  }

  .ci <- NULL
  if (!is.null(control)) {
    .ci   <- .rxMemExtractControl(control)
    cores <- .ci$cores
    nsim  <- .ci$nsim
    if (.ci$neta > 0L) neta <- .ci$neta
    if (.ci$neps > 0L) neps <- .ci$neps
    if (!is.null(.ci$nLlikAlloc)) nLlik <- max(nLlik, as.integer(.ci$nLlikAlloc))
  }
  if (cores == 0) {
    cores <- getRxThreads()
  }
  if (is.null(nIndSim)) nIndSim <- neta + neps

  .nallVec     <- .summary$nobs + .summary$ndoses
  .nsub        <- nrow(.summary)
  .nallTotal   <- sum(.nallVec)
  .maxAllTimes <- max(.nallVec)

  if (!is.null(.ci) && (.ci$nSub > 1L || .ci$nStud > 1L)) {
    .subPerStudy <- if (.ci$nSub > 1L) .ci$nSub else .nsub
    .meanAllTimes <- .nallTotal / .nsub
    .nsub      <- .subPerStudy * .ci$nStud
    .nallTotal <- .meanAllTimes * .nsub
  }

  .raw <- rxMemoryComponents_(
    neq        = as.integer(neq),
    stateSize  = as.integer(stateSize),
    nlhs       = as.integer(nlhs),
    npars      = as.integer(npars),
    neta       = as.integer(neta),
    neps       = as.integer(neps),
    ncov       = as.integer(ncov),
    nsim       = as.integer(nsim),
    cores      = as.integer(cores),
    nMtime     = as.integer(nMtime),
    extraCmt   = as.integer(extraCmt),
    linB       = as.integer(linB),
    nLlik      = as.integer(nLlik),
    nIndSim    = as.integer(nIndSim),
    numLinSens = as.integer(numLinSens),
    numLin     = as.integer(numLin),
    nsub       = as.integer(.nsub),
    nallTotal  = as.double(.nallTotal),
    maxAllTimes = as.double(.maxAllTimes)
  )

  .meta    <- c("sizeofInd", "rxLlikSaveSize")
  .sizes   <- .raw[!names(.raw) %in% .meta]
  .wrapped <- lapply(.sizes, function(bytes) {
    structure(bytes, class = "rxRawBytes")
  })
  .total   <- Reduce(`+`, .wrapped)

  .ret <- c(list(total = .total), .wrapped,
            list(sizeofInd      = .raw[["sizeofInd"]],
                 rxLlikSaveSize = .raw[["rxLlikSaveSize"]],
                 ramBytes       = .getRamBytes(),
                 effectiveSubs  = .nsub))
  class(.ret) <- "rxMemoryEstimate"
  attr(.ret, "summary") <- .summary
  .ret
}

#' @export
print.rxMemoryEstimate <- function(x, ...) {
  .meta  <- c("total", "sizeofInd", "rxLlikSaveSize", "ramBytes", "effectiveSubs")
  .comps <- x[!names(x) %in% .meta]

  .hasMem <- requireNamespace("memuse", quietly = TRUE)

  .fmtSize <- function(v) {
    if (.hasMem && inherits(v, "memuse")) {
      format(v, ...)
    } else {
      .b <- if (is.numeric(v)) v else unclass(v)
      if (.b >= 1e9)       sprintf("%.2f GB", .b / 1e9)
      else if (.b >= 1e6)  sprintf("%.2f MB", .b / 1e6)
      else if (.b >= 1e3)  sprintf("%.2f KB", .b / 1e3)
      else                 sprintf("%.0f B",  .b)
    }
  }

  cat("rxSolve() memory estimate\n")
  cat(sprintf("  Total: %s\n\n", .fmtSize(x$total)))

  .bytes <- vapply(.comps, function(v) {
    if (.hasMem && inherits(v, "memuse")) {
      .obj <- memuse::mu(v, unit = "B", unit.names = "short")
      as.numeric(format(.obj, unit = "B"))
    } else {
      as.numeric(v)
    }
  }, numeric(1))

  .ord <- order(.bytes, decreasing = TRUE)
  .nm  <- names(.comps)[.ord]
  .sz  <- .bytes[.ord]
  .tot <- sum(.sz)

  .labels <- c(
    gsolve        = "gsolve (double buffer total)",
    gsolve_n0     = "  └─ n0: ODE state output matrix",
    gon           = "gon (int buffer)",
    gall_times    = "gall_times (event time/dv/amt/ii/limit)",
    gevid         = "gevid (event IDs)",
    gcov          = "gcov (covariates)",
    gpars         = "gpars (parameters)",
    gomega        = "gomega (omega matrix)",
    gsigma        = "gsigma (sigma matrix)",
    gall_timesS   = "gall_timesS (extra sim times)",
    ordId         = "ordId (subject ordering)",
    gInfusionRate = "gInfusionRate (per-thread infusion)",
    inds_global   = "inds_global (per-subject structs)"
  )

  for (.i in seq_along(.nm)) {
    .n   <- .nm[.i]
    .lab <- if (!is.na(.labels[.n])) .labels[.n] else .n
    .pct <- if (.tot > 0) sprintf(" (%4.1f%%)", 100 * .sz[.i] / .tot) else ""
    cat(sprintf("  %-42s %s%s\n", .lab, .fmtSize(.comps[[.n]]), .pct))
  }

  .nsub <- if (!is.null(x$effectiveSubs)) as.integer(x$effectiveSubs) else nrow(attr(x, "summary"))
  cat(sprintf("\n  Subjects: %d  |  sizeof(rx_solving_options_ind): %d B",
              .nsub, as.integer(x$sizeofInd)))

  .ramBytes <- x$ramBytes
  if (!is.null(.ramBytes) && !is.na(.ramBytes) && .ramBytes > 0) {
    .totalBytes <- as.numeric(x$total)
    cat(sprintf("  |  %.1f%% of RAM (%s)\n",
                100 * .totalBytes / .ramBytes, .fmtSize(.ramBytes)))
  } else {
    cat("\n")
  }
  invisible(x)
}
