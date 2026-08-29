#' Simulated percentiles, with confidence bands, from a solved rxode2 object
#'
#' Summarizes a solved object into the percentiles of the simulated values at
#' each time and, when the simulation can support it, a confidence band around
#' each of those percentiles.
#'
#' The percentiles are always taken over the simulated individuals; how (and
#' whether) the band around them is obtained depends on the simulation:
#'
#' * `ci = FALSE` -- no band; the pooled percentiles are returned.
#'
#' * `nStud > 1` -- the percentiles are computed within each study and the band
#'   is the quantile of those study-level percentiles.  This is the meaningful
#'   case, since the studies differ by the `thetaMat`/`omega` uncertainty draw.
#'
#' * a single study of at least 2500 individuals -- the individuals are split
#'   into `round(sqrt(n))` sub-samples, and the band is the quantile of the
#'   sub-sample percentiles, that is, the sampling variability of the percentile
#'   itself.  It does not include parameter uncertainty.
#'
#' * anything smaller -- no band, with a message saying so.
#'
#' @param object solved rxode2 object
#' @param parm compartments or calculated (`lhs`) variables to summarize; when
#'   `NULL` everything `rxStack()` returns is summarized
#' @param level width of the interval taken over the simulated individuals,
#'   that is, which percentiles are reported at each time
#' @param ... other options:
#'
#'   * `ci` -- width of the confidence band placed around each percentile,
#'     defaulting to `level`.  `ci = FALSE` (or `0`) returns the percentiles
#'     with no band.
#'
#'   * `mean` -- when `TRUE` report the mean and its interval with
#'     [meanProbs()] instead of the empirical quantiles; `mean = "binom"` uses
#'     [binomProbs()] for a 0/1 variable.
#'
#'   * `by` -- character vector of extra columns of `object` to stratify by.
#'
#'   * `useT`, `pred` -- passed to [meanProbs()]; `n`, `m`, `M`, `tol`, `pred`,
#'     `ciMethod` -- passed to [binomProbs()].
#'
#'   * `doSim` -- passed to [rxStack()].
#'
#' @return A `data.frame` (a `tibble` when \pkg{tibble} is present) with one row
#'   per time, endpoint and requested percentile.  Without a band it is class
#'   `rxSolveConfint1` with the percentile in `p1` and its value in `eff`; with
#'   a band it is class `rxSolveConfint2` with the percentile in `p1` and the
#'   band in the `p<lower>`, `p50` and `p<upper>` columns.  Both carry a
#'   `Percentile` label used by [plot()].
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' \donttest{
#'
#' mod <- rxode2({
#'   ka <- 1
#'   cl <- 1 * exp(eta.cl)
#'   v <- 20
#'   d/dt(depot) <- -ka * depot
#'   d/dt(center) <- ka * depot - cl / v * center
#'   cp <- center / v
#' })
#'
#' ev <- et(amt=100) |> et(seq(0, 24, length.out=25))
#'
#' s <- rxSolve(mod, ev, omega=lotri(eta.cl ~ 0.1), nSub=100)
#'
#' # 100 individuals in one study: percentiles only
#' confint(s, "cp", level=0.95, ci=FALSE)
#'
#' # with 20 studies the percentiles get a confidence band
#' s2 <- rxSolve(mod, ev, omega=lotri(eta.cl ~ 0.1), nSub=100, nStud=20,
#'               thetaMat=lotri(ka ~ 0.01))
#'
#' confint(s2, "cp", level=0.95)
#'
#' }
#'
#' @export
confint.rxSolve <- function(object, parm = NULL, level = 0.95, ...) {
  sim.id <- id <- NULL # rcheck nonsense
  .SD <- . <- NULL # nolint
  `:=` <- NULL # nolint
  rxode2::rxReq("data.table")
  checkmate::assertNumeric(level, lower=0, upper=1, finite=TRUE, any.missing=FALSE)
  .opt <- .confintOptions(list(...), object, level)
  .by <- .opt$by
  .ci <- .opt$ci
  .stk <- rxStack(object, parm, doSim=.opt$doSim) # nolint
  .nStud <- object$env$.args$nStud
  if (!checkmate::testIntegerish(.nStud, len=1L, any.missing=FALSE)) .nStud <- 1L
  .nSub <- object$env$.args$nSub
  if (!checkmate::testIntegerish(.nSub, len=1L, any.missing=FALSE)) .nSub <- 1L
  if (!any(names(.stk) == "id") &&
        any(names(.stk) == "sim.id")) {
    if (.nStud > 1L && .nSub > 1L) {
      # With a single-subject event table rxode2 solves nStud*nSub simulations
      # numbered study-major in `sim.id` and emits no `id` column; split it back
      # into study (`sim.id`) and individual (`id`) so the study dimension can
      # still be used for the confidence bands
      .stk$id <- as.integer(.stk$sim.id)
      .stk$sim.id <- (.stk$id - 1L) %/% as.integer(.nSub) + 1L
    } else {
      names(.stk) <- gsub("sim.id", "id", names(.stk))
    }
  }
  for (.v in .by) {
    .stk[[.v]] <- object[[.v]]
  }
  setDT(.stk) # nolint
  .a <- (1 - level) / 2
  .p <- c(.a, 0.5, 1 - .a)
  .c <- (1-.ci) / 2
  .p2 <- c(.c, 0.5, 1 - .c)
  .lst <- list(
    lvl = paste0("p", .p * 100),
    ci = paste0("p", .p2 * 100),
    parm = levels(.stk$trt),
    by = .by,
    mean = .opt$mean,
    binom = .opt$binom
  )
  class(.lst) <- "rxHidden"
  # `.n` is the number of replicate summaries the confidence band is taken
  # over; it stays NA when the simulation cannot supply any
  .n <- NA_integer_
  if (.ci ==0 || !any(names(.stk) == "sim.id") ||
        !isTRUE(.nStud > 1L)) {
    if (any(names(.stk) == "sim.id")) {
      .stk$id <- factor(paste(.stk$sim.id, .stk$id))
      .ntot <- length(levels(.stk$id))
      .stk$id <- as.integer(.stk$id)
    } else if (any(names(.stk) == "id")) {
      .ntot <- length(unique(.stk$id))
    } else {
      .ntot <- .nSub
      if (.ntot == 1L && .nStud > 1L) {
        .ntot <- .nStud
      }
    }
    if (.ci != 0 && .ntot >= 2500) {
      # one study, but enough individuals to sub-sample it
      .n <- round(sqrt(.ntot))
      if (!any(names(.stk) == "sim.id")) {
        # `id` can be a factor (character subject identifiers); densify to
        # 1:.ntot so the modulus below splits it into `.n` sub-samples
        .stk$sim.id <- as.integer(factor(.stk$id))
      }
    } else if (.ci != 0.0) {
      .mwarn("in order to put confidence bands around the intervals, you need at least 2500 simulations") # nolint
    }
  } else {
    # each study is its own uncertainty draw
    .n <- .nStud
  }
  message("summarizing data...", appendLF = FALSE)
  if (is.na(.n)) {
    .stk <- .stk[, list(
      p1 = .p, eff = .confintProbs(.SD$value, .p, .opt),
      Percentile = sprintf("%s%%", .p * 100)
    ),
    by = c("time", "trt", .by)
    ]
    if (requireNamespace("tibble", quietly = TRUE)) {
      .stk <- tibble::as_tibble(.stk)
    }
    .cls <- c("rxSolveConfint1", class(.stk))
    attr(.cls, ".rx") <- .lst
    class(.stk) <- .cls
    message("done")
    return(.stk)
  }
  .ret <- .stk[, id := sim.id %% .n]
  .ret <- .ret[, list(p1 = .p, eff = .confintProbs(.SD$value, .p, .opt)),
               by = c("id", "time", "trt", .by)]
  .ret <- .ret[, setNames(as.list(stats::quantile(.SD$eff, probs = .p2, na.rm = TRUE)),
                          sprintf("p%s", .p2 * 100)),
               by = c("p1", "time", "trt", .by)]
  .ret$Percentile <- factor(sprintf("%s%%", .ret$p1 * 100))
  if (requireNamespace("tibble", quietly = TRUE)) {
    .ret <- tibble::as_tibble(.ret)
  }
  message("done")
  .cls <- c("rxSolveConfint2", class(.ret))
  attr(.cls, ".rx") <- .lst
  class(.ret) <- .cls
  .ret
}
