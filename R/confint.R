#' @export
confint.rxSolve <- function(object, parm = NULL, level = 0.95, ...) {
  sim.id <- id <- NULL # rcheck nonsense
  rxode2::rxReq("data.table")
  ## p1 <-eff <-Percentile <-sim.id <-id <-p2 <-p50 <-p05 <- p95 <- . <- time <- trt <- NULL
  ## rxode2::rxReq("dplyr")
  ## rxode2::rxReq("tidyr")
  checkmate::assertNumeric(level, lower=0, upper=1, finite=TRUE, any.missing=FALSE)
  .args <- list(...)
  if (any(names(.args) == "doSim")) {
    checkmate::assertLogical(.args$doSim, len=1,
                                       any.missing=FALSE, .var.name="doSim")
    .doSim <- .args$doSim
  } else {
    .doSim<-TRUE
  }
  .by <- NULL
  if (any(names(.args) == "by")) {
    .by <- .args$by
    checkmate::assertSubset(.by, names(object), .var.name="by")
  }
  .ci <- level
  if (any(names(.args) == "ci")) {
    .ci <- .args$ci
    if (inherits(.ci, "logical")) {
      checkmate::assertLogical(.ci, len=1, any.missing=FALSE, .var.name="ci")
      if (!.ci) {
        .ci <- 0.0
      }
    } else {
      checkmate::assertNumeric(.ci, lower=0, upper=1, finite=TRUE, any.missing=FALSE, .var.name="ci")
    }
  }
  .mean <- FALSE
  .binom <- FALSE
  .m <- 0L
  .nC <- 0L
  .pred <- FALSE
  .useT <- TRUE
  if (any(names(.args) == "useT")) {
    .useT <- .args$useT
    checkmate::assertLogical(.useT, len=1, any.missing=FALSE, .var.name="useT")
  }
  if (any(names(.args) == "mean")) {
    .mean <- .args$mean
    if (inherits(.mean, "character") &&
          length(.mean) == 1L &&
          .mean == "binom") {
      .binom <- TRUE
      .mean <- FALSE
    } else {
      checkmate::assertLogical(.mean, len=1, any.missing=FALSE, .var.name="mean")
    }
  }
  if (any(names(.args) == "pred")) {
    .pred <- .args$pred
    checkmate::assertLogical(.pred, len=1, any.missing=FALSE, .var.name="pred")
  }

  if (any(names(.args) == "n")) {
    .nC <- unique(.args$n)
  }
  if (any(names(.args) == "m")) {
    .m <- unique(.args$m)
  }
  .stk <- rxStack(object, parm, doSim=.doSim)
  if (!any(names(.stk) == "id") &&
        any(names(.stk) == "sim.id")) {
    names(.stk) <- gsub("sim.id", "id", names(.stk))
  }
  for(.v in .by) {
    .stk[[.v]] <- object[[.v]]
  }
  setDT(.stk)
  .a <- (1 - level) / 2
  .p <- c(.a, 0.5, 1 - .a)
  .c <- (1-.ci) / 2
  .p2 <- c(.c, 0.5, 1 - .c)
  .lst <- list(
    lvl = paste0("p", .p * 100),
    ci = paste0("p", .p2 * 100),
    parm = levels(.stk$trt),
    by = .by,
    mean = .mean,
    binom=.binom
  )
  class(.lst) <- "rxHidden"
  if (.ci ==0 || !any(names(.stk) == "sim.id")) {
    if (any(names(.stk) == "sim.id")) {
      .stk$id <- factor(paste(.stk$sim.id, .stk$id))
      .ntot <- length(levels(.stk$id))
      .stk$id <- as.integer(.stk$id)
    } else {
      .ntot <- object$env$.args$nSub
      if (.ntot == 1L && object$env$.args$nStud > 1L) {
        .ntot <- object$env$.args$nStud
      }
    }
    if (.ci == 0 || .ntot < 2500) {
      if (.ci != 0.0) {
        .mwarn("in order to put confidence bands around the intervals, you need at least 2500 simulations")
      }
      message("summarizing data...", appendLF = FALSE)
      if (.mean) {
        .stk <- .stk[, list(
          p1 = .p, eff = rxode2::meanProbs(.SD$value, probs = .p, na.rm = TRUE, useT=.useT,
                                           n=.nC, pred=.pred),
          Percentile = sprintf("%s%%", .p * 100)
        ),
        by = c("time", "trt", .by)
        ]
      } else if (.binom) {
        .stk <- .stk[, list(
          p1 = .p, eff = rxode2::binomProbs(.SD$value, probs = .p, na.rm = TRUE,
                                            n=.nC, m=.m, pred=.pred),
          Percentile = sprintf("%s%%", .p * 100)
        ),
        by = c("time", "trt", .by)
        ]
      } else {
        .stk <- .stk[, list(
          p1 = .p, eff = stats::quantile(.SD$value, probs = .p, na.rm = TRUE),
          Percentile = sprintf("%s%%", .p * 100)
        ),
        by = c("time", "trt", .by)
        ]
      }
      if (requireNamespace("tibble", quietly = TRUE)) {
        .stk <- tibble::as_tibble(.stk)
      }
      .cls <- c("rxSolveConfint1", class(.stk))
      attr(.cls, ".rx") <- .lst
      class(.stk) <- .cls
      message("done")
      return(.stk)
    } else {
      .n <- round(sqrt(.ntot))
      if (!any(names(.stk) == "sim.id")) {
        .stk$sim.id <- .stk$id
      }
    }
  } else {
    .n <- object$env$.args$nStud
  }
  message("summarizing data...", appendLF = FALSE)
  .ret <- .stk[, id := sim.id %% .n]
  if (.mean) {
    .ret <- .ret[, list(p1 = .p,
                        eff = rxode2::meanProbs(.SD$value, probs = .p, na.rm = TRUE, n=.nC,
                                                useT=.useT,
                                                pred=.pred)),
                 by = c("id", "time", "trt", .by)]
  } else if (.binom) {
    print(.nC)
    .ret <- .ret[, list(p1 = .p,
                        eff = rxode2::binomProbs(.SD$value, probs = .p, na.rm = TRUE,
                                                 n=.nC, m=.m, pred=.pred)),
                 by = c("id", "time", "trt", .by)]
  } else {
    .ret <- .ret[, list(p1 = .p,
                        eff = stats::quantile(.SD$value, probs = .p, na.rm = TRUE)), by = c("id", "time", "trt", .by)]

  }
  .ret <- .ret[, setNames(as.list(stats::quantile(.SD$eff, probs = .p2, na.rm = TRUE)),
                          sprintf("p%s", .p2 * 100)),
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
