## Memory estimation for rxSolve()
##
## Public surface:
##   rxMemSummary()       — constructor for per-ID summary data
##   rxMemoryEstimate()   — main estimation function
##   print.rxMemoryEstimate()

# -------------------------------------------------------------------------
# Detection helpers
# -------------------------------------------------------------------------

.isRxMemSummary <- function(.dat) {
  inherits(.dat, "rxMemSummary") ||
    (is.data.frame(.dat) &&
       all(c("nobs", "ndoses") %in% names(.dat)) &&
       !("evid" %in% names(.dat)))
}

# -------------------------------------------------------------------------
# rxMemSummary constructor
# -------------------------------------------------------------------------

#' Create a per-ID event summary for memory estimation
#'
#' @param id   Integer or character vector of subject IDs (optional).
#' @param nobs Integer vector of observation counts per ID.
#' @param ndoses Integer vector of dose event counts per ID.
#' @return A data.frame with class \code{"rxMemSummary"}.
#' @export
rxMemSummary <- function(nobs, ndoses, id = seq_along(nobs)) {
  .ret <- data.frame(id = id, nobs = as.integer(nobs), ndoses = as.integer(ndoses))
  class(.ret) <- c("rxMemSummary", "data.frame")
  .ret
}

# -------------------------------------------------------------------------
# Internal: summarise a raw event table → rxMemSummary
# -------------------------------------------------------------------------

.rxMemSummarizeDat <- function(.dat) {
  .evidCol <- "evid"
  .idCol   <- grep("^id$", names(.dat), ignore.case = TRUE, value = TRUE)[1]

  if (is.na(.idCol)) {
    .ret <- rxMemSummary(
      nobs   = sum(.dat[[.evidCol]] == 0L, na.rm = TRUE),
      ndoses = sum(.dat[[.evidCol]] != 0L, na.rm = TRUE)
    )
  } else {
    .ids <- unique(.dat[[.idCol]])
    .ret <- do.call(rbind, lapply(.ids, function(.id) {
      .sub <- .dat[.dat[[.idCol]] == .id, ]
      rxMemSummary(
        id     = .id,
        nobs   = sum(.sub[[.evidCol]] == 0L, na.rm = TRUE),
        ndoses = sum(.sub[[.evidCol]] != 0L, na.rm = TRUE)
      )
    }))
    class(.ret) <- c("rxMemSummary", "data.frame")
  }
  .ret
}

# -------------------------------------------------------------------------
# Internal: extract model dimensions from an rxode2 model object
# -------------------------------------------------------------------------

.rxMemExtractModel <- function(.model) {
  .mv    <- rxModelVars(.model)
  .flags <- .mv[["flags"]]  # integer vector; positions are RxMvFlag_* (0-indexed)

  list(
    neq        = length(.mv[["state"]]),
    state_size = length(.mv[["state"]]),   # equals neq for pure ODE models
    nlhs       = length(.mv[["lhs"]]),
    npars      = length(.mv[["params"]]),
    extraCmt   = as.integer(.mv[["extraCmt"]]),
    linB       = as.integer(.flags[3L]),   # RxMvFlag_linB = 2
    nMtime     = as.integer(.mv[["nMtime"]]),
    nLlik      = as.integer(.flags[12L]),  # RxMvFlag_nLlik = 11
    nIndSim    = as.integer(.flags[9L])    # RxMvFlag_nIndSim = 8
  )
}

# -------------------------------------------------------------------------
# Internal: wrap raw bytes in memuse objects when the package is available
# -------------------------------------------------------------------------

.toMemuse <- function(.bytes) {
  if (requireNamespace("memuse", quietly = TRUE)) {
    memuse::mu(.bytes, unit = "B", unit.names = "short")
  } else {
    structure(.bytes, class = "rxRawBytes")
  }
}

# -------------------------------------------------------------------------
# rxMemoryEstimate
# -------------------------------------------------------------------------

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
#' @param dat        A \code{\link{rxMemSummary}}, a data.frame with
#'   \code{nobs}/\code{ndoses} columns, or a full event-table data.frame
#'   with an \code{evid} column.
#' @param model      Optional rxode2 model object.  When supplied, \code{neq},
#'   \code{nlhs}, \code{npars}, \code{extraCmt}, \code{linB}, \code{nMtime},
#'   \code{nLlik}, and \code{nIndSim} are extracted automatically.
#' @param neq        Number of ODE states.
#' @param state_size Effective \code{state.size()} seen by the solver.  Equals
#'   \code{neq} for pure ODE models; may differ for linCmt-only models.
#'   Defaults to \code{neq}.
#' @param nlhs       Number of LHS (calculated) output variables.
#' @param npars      Number of model parameters (drives \code{gpars} size).
#' @param neta       Number of random effects (etas).
#' @param neps       Number of residual-error levels (epsilons).
#' @param ncov       Number of time-varying covariates.
#' @param nsim       Number of simulations.
#' @param cores      Number of parallel OMP threads.
#' @param nMtime     Number of model measurement times.
#' @param extraCmt   Extra compartments (0, 1 = depot, 2 = depot+central).
#' @param linB       \code{TRUE}/\code{1} if using a linear-compartment model.
#' @param nLlik      Number of log-likelihood terms (FOCEi use).
#' @param nIndSim    Per-individual simulation count.  Defaults to
#'   \code{neta + neps} when not supplied explicitly.
#' @param numLinSens Number of linear sensitivity parameters (FOCEi + linCmt).
#' @param numLin     Number of linear compartment terms (FOCEi + linCmt).
#' @return A named list of class \code{"rxMemoryEstimate"} whose elements are
#'   \code{memuse} objects (or raw byte counts if \pkg{memuse} is not
#'   installed) plus \code{total}, \code{sizeof_ind}, and
#'   \code{rxLlikSaveSize}.
#' @export
rxMemoryEstimate <- function(
  dat,
  model      = NULL,
  neq        = 1L,
  state_size = neq,
  nlhs       = 0L,
  npars      = neq,
  neta       = 0L,
  neps       = 0L,
  ncov       = 0L,
  nsim       = 1L,
  cores      = 1L,
  nMtime     = 0L,
  extraCmt   = 0L,
  linB       = FALSE,
  nLlik      = 0L,
  nIndSim    = NULL,
  numLinSens = 0L,
  numLin     = 0L
) {

  # ------------------------------------------------------------------
  # 1. Resolve dat → rxMemSummary
  # ------------------------------------------------------------------
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

  # ------------------------------------------------------------------
  # 2. Extract model dimensions (overrides manual args)
  # ------------------------------------------------------------------
  if (!is.null(model)) {
    .mi        <- .rxMemExtractModel(model)
    neq        <- .mi$neq
    state_size <- .mi$state_size
    nlhs       <- .mi$nlhs
    npars      <- .mi$npars
    extraCmt   <- .mi$extraCmt
    linB       <- .mi$linB
    nMtime     <- .mi$nMtime
    nLlik      <- .mi$nLlik
    if (is.null(nIndSim)) nIndSim <- .mi$nIndSim
  }

  # ------------------------------------------------------------------
  # 3. Auto-default nIndSim
  # ------------------------------------------------------------------
  if (is.null(nIndSim)) nIndSim <- neta + neps

  # ------------------------------------------------------------------
  # 4. Summary statistics
  # ------------------------------------------------------------------
  .nsub        <- nrow(.summary)
  .nall_vec    <- .summary$nobs + .summary$ndoses
  .nall_total  <- sum(.nall_vec)
  .maxAllTimes <- max(.nall_vec)

  # ------------------------------------------------------------------
  # 5. Byte counts via C (same formulas as the allocator)
  # ------------------------------------------------------------------
  .raw <- rxMemoryComponents_(
    neq        = as.integer(neq),
    state_size = as.integer(state_size),
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
    nall_total = as.double(.nall_total),
    maxAllTimes = as.double(.maxAllTimes)
  )

  # ------------------------------------------------------------------
  # 6. Wrap in memuse objects
  # ------------------------------------------------------------------
  .meta <- c("sizeof_ind", "rxLlikSaveSize")
  .sizes <- .raw[!names(.raw) %in% .meta]

  .wrapped <- lapply(.sizes, .toMemuse)
  .total   <- Reduce(`+`, .wrapped)

  .ret <- c(list(total = .total), .wrapped,
            list(sizeof_ind      = .raw[["sizeof_ind"]],
                 rxLlikSaveSize  = .raw[["rxLlikSaveSize"]]))
  class(.ret) <- "rxMemoryEstimate"
  attr(.ret, "summary") <- .summary
  .ret
}

# -------------------------------------------------------------------------
# print method
# -------------------------------------------------------------------------

#' @export
print.rxMemoryEstimate <- function(x, ...) {
  .meta  <- c("total", "sizeof_ind", "rxLlikSaveSize")
  .comps <- x[!names(x) %in% .meta]

  .hasMem <- requireNamespace("memuse", quietly = TRUE)

  .fmtSize <- function(.v) {
    if (.hasMem && inherits(.v, "memuse")) {
      format(.v, ...)
    } else {
      .b <- if (is.numeric(.v)) .v else unclass(.v)
      if (.b >= 1e9)       sprintf("%.2f GB", .b / 1e9)
      else if (.b >= 1e6)  sprintf("%.2f MB", .b / 1e6)
      else if (.b >= 1e3)  sprintf("%.2f KB", .b / 1e3)
      else                 sprintf("%.0f B",  .b)
    }
  }

  cat("rxSolve() memory estimate\n")
  cat(sprintf("  Total: %s\n\n", .fmtSize(x$total)))

  .bytes <- vapply(.comps, function(.v) {
    if (.hasMem && inherits(.v, "memuse")) {
      .obj <- memuse::mu(.v, unit = "B", unit.names = "short")
      as.numeric(format(.obj, unit = "B"))
    } else {
      as.numeric(.v)
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

  .nsub <- nrow(attr(x, "summary"))
  cat(sprintf("\n  Subjects: %d  |  sizeof(rx_solving_options_ind): %d B",
              .nsub, as.integer(x$sizeof_ind)))

  if (.hasMem) {
    .avail <- tryCatch(memuse::Sys.meminfo()$totalram, error = function(e) NULL)
    if (!is.null(.avail)) {
      .pct <- 100 * as.numeric(memuse::mu(x$total, unit = "B")) /
        as.numeric(memuse::mu(.avail, unit = "B"))
      cat(sprintf("  |  %.1f%% of RAM (%s)\n", .pct, format(.avail)))
    } else {
      cat("\n")
    }
  } else {
    cat("\n")
  }
  invisible(x)
}
