#' @export
confint.rxSolve <- function(object, parm = NULL, level = 0.95, ...) {
  sim.id <- id <- NULL # rcheck nonsense
  rxode2::rxReq("data.table")
  ## p1 <-eff <-Percentile <-sim.id <-id <-p2 <-p50 <-p05 <- p95 <- . <- time <- trt <- NULL
  ## rxode2::rxReq("dplyr")
  ## rxode2::rxReq("tidyr")
  if (level <= 0 || level >= 1) {
    stop("simulation summaries must be between 0 and 1", call. = FALSE)
  }
  .args <- list(...)
  if (any(names(.args) == "doSim")) {
    .doSim <- .args$doSim
  } else {
    .doSim=TRUE
  }
  .by <- NULL
  if (any(names(.args) == "by")) {
    .by <- .args$by
  }
  .stk <- rxStack(object, parm, doSim=.doSim)
  for(.v in .by) {
    .stk[[.v]] <- object[[.v]]
  }
  setDT(.stk)
  .a <- (1 - level) / 2
  .p <- c(.a, 0.5, 1 - .a)
  .lst <- list(
    lvl = paste0("p", .p * 100),
    parm = levels(.stk$trt),
    by = .by
  )
  class(.lst) <- "rxHidden"
  if (object$env$.args$nStud <= 1) {
    if (object$env$.args$nSub < 2500) {
      .mwarn("in order to put confidence bands around the intervals, you need at least 2500 simulations")
      message("summarizing data...", appendLF = FALSE)
      .stk <- .stk[, list(
        p1 = .p, eff = stats::quantile(.SD$value, probs = .p, na.rm = TRUE),
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
    } else {
      .n <- round(sqrt(object$env$.args$nSub))
      if (!any(names(.stk) == "sim.id")) {
        .stk$sim.id <- .stk$id
      }
    }
  } else {
    .n <- object$env$.args$nStud
  }
  message("summarizing data...", appendLF = FALSE)
  .ret <- .stk[, id := sim.id %% .n][, list(p1 = .p, eff = stats::quantile(.SD$value, probs = .p, na.rm = TRUE)), by = c("id", "time", "trt", .by)][, setNames(
    as.list(stats::quantile(.SD$eff, probs = .p, na.rm = TRUE)),
    sprintf("p%s", .p * 100)
  ),
  by = c("p1", "time", "trt", .by)
  ]
  .ret$Percentile <- factor(sprintf("%s%%", .ret$p1 * 100))
  if (requireNamespace("tibble", quietly = TRUE)) {
    .ret <- tibble::as_tibble(.ret)
  }
  message("done")
  .cls <- c("rxSolveConfint2", class(.ret))
  attr(.cls, ".rx") <- .lst
  class(.ret) <- .cls
  return(.ret)
}
