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
#' Convert raw byte counts to memuse objects if memuse is available
#'
#'
#' @param bytes bytes to convert
#' @return A memuse object if memuse is available, otherwise the
#'   original byte count with class "rxRawBytes".
#' @noRd
#' @author Matthew L. Fidler
.toMemuse <- function(bytes) {
  if (requireNamespace("memuse", quietly = TRUE)) {
    memuse::mu(bytes, unit = "B", unit.names = "short")
  } else {
    structure(bytes, class = "rxRawBytes")
  }
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
#' @param dat        A \code{\link{rxMemSummary}}, a data.frame with
#'   \code{nobs}/\code{ndoses} columns, or a full event-table data.frame
#'   with an \code{evid} column.
#' @param model      Optional rxode2 model object.  When supplied, \code{neq},
#'   \code{nlhs}, \code{npars}, \code{extraCmt}, \code{linB}, \code{nMtime},
#'   \code{nLlik}, and \code{nIndSim} are extracted automatically.
#' @param neq        Number of ODE states.
#' @param stateSize  Effective \code{state.size()} seen by the solver.  Equals
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
#'   installed) plus \code{total}, \code{sizeofInd}, and
#'   \code{rxLlikSaveSize}.
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
#' rxMemoryEstimate(as.data.frame(ev), model = mod)
#'
#' }
rxMemoryEstimate <- function(
  dat,
  model     = NULL,
  neq       = 1L,
  stateSize = neq,
  nlhs      = 0L,
  npars     = neq,
  neta      = 0L,
  neps      = 0L,
  ncov      = 0L,
  nsim      = 1L,
  cores     = 1L,
  nMtime    = 0L,
  extraCmt  = 0L,
  linB      = FALSE,
  nLlik     = 0L,
  nIndSim   = NULL,
  numLinSens = 0L,
  numLin    = 0L
) {
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

  if (is.null(nIndSim)) nIndSim <- neta + neps

  .nsub        <- nrow(.summary)
  .nallVec     <- .summary$nobs + .summary$ndoses
  .nallTotal   <- sum(.nallVec)
  .maxAllTimes <- max(.nallVec)

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
  .wrapped <- lapply(.sizes, .toMemuse)
  .total   <- Reduce(`+`, .wrapped)

  .ret <- c(list(total = .total), .wrapped,
            list(sizeofInd     = .raw[["sizeofInd"]],
                 rxLlikSaveSize = .raw[["rxLlikSaveSize"]]))
  class(.ret) <- "rxMemoryEstimate"
  attr(.ret, "summary") <- .summary
  .ret
}

#' @export
print.rxMemoryEstimate <- function(x, ...) {
  .meta  <- c("total", "sizeofInd", "rxLlikSaveSize")
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

  .nsub <- nrow(attr(x, "summary"))
  cat(sprintf("\n  Subjects: %d  |  sizeof(rx_solving_options_ind): %d B",
              .nsub, as.integer(x$sizeofInd)))

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
