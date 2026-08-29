# P5 A/B: direct state-read emission (HEAD) vs call-read emission
# (baseline d21ddc900~1, built in ~/src/rxode2-p5-base).
#
# Each tree generates ITS OWN FOCEi inner model text (call reads on the
# baseline, state reads on HEAD) and solves it through its own build, so
# the measurement captures exactly what a fit pays on that tree.
# Protocol: optimized builds, single thread, taskset-pinned by the
# caller, warm-up solve excluded, medians of REPS, per-cell load
# recorded.  MODE=base|head selects the tree; MODE=fit runs the FOCEi
# 40x100 fit cell (both arms + the ODE arm) on the tree given by TREE.
suppressMessages({
  mode <- Sys.getenv("MODE", "head")
  tree <- switch(Sys.getenv("TREE", if (mode == "base") "base" else "head"),
                 base = "~/src/rxode2-p5-base",
                 head = "~/src/rxode2-lincmt-analytic")
  devtools::load_all(tree, compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est", helpers = FALSE,
                     quiet = TRUE)
})
rxode2::setRxThreads(1L)
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
REPS <- 3L

mkUi <- function(ncmt) {
  if (ncmt == 2L) {
    function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 0.8; tv2 <- 4
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v); q <- exp(tq); v2 <- exp(tv2)
        linCmt() ~ add(add.sd)
      })
    }
  } else {
    function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 0.8; tv2 <- 4
        tq2 <- 0.2; tv3 <- 4.5
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v); q <- exp(tq); v2 <- exp(tv2)
        q2 <- exp(tq2); v3 <- exp(tv3)
        linCmt() ~ add(add.sd)
      })
    }
  }
}

innerOf <- function(ncmt) {
  ui <- nlmixr2est::nlmixr(mkUi(ncmt))
  fm <- ui$foceiModel
  list(inner = fm$inner, theta = ui$theta)
}

evOf <- function(nSub, nObs, uniform = TRUE) {
  tmax <- 48
  tt <- if (uniform) seq(tmax / nObs, tmax, length.out = nObs) else
    sort(unique(round(cumsum(stats::qexp(seq(0.02, 0.98, length.out = nObs))) /
                        sum(stats::qexp(seq(0.02, 0.98, length.out = nObs))) * tmax, 4)))
  rxode2::et(amt = 100, ii = 12, addl = 3) |> rxode2::et(tt) |>
    rxode2::et(id = seq_len(nSub))
}

parsOf <- function(inner, nSub) {
  pn <- rxode2::rxModelVars(inner)$params
  p <- setNames(rep(0.1, length(pn)), pn)
  p[grepl("^THETA", names(p))] <- 0.9
  p[grepl("^ETA", names(p))] <- 0.05
  d <- data.frame(id = seq_len(nSub), as.list(p), check.names = FALSE)
  names(d) <- c("id", names(p)) # keep THETA[1]-style names unmangled
  d
}

if (mode %in% c("base", "head")) {
  out <- NULL
  for (ncmt in c(2L, 3L)) {
    im <- innerOf(ncmt)$inner
    stateReads <- grepl("rx__sens_central_BY_", rxode2::rxNorm(im))
    for (cell in list(c(400L, 1000L, 1L), c(40L, 200L, 1L), c(40L, 200L, 0L))) {
      nSub <- cell[1]; nObs <- cell[2]; unif <- cell[3] == 1L
      ev <- evOf(nSub, nObs, unif)
      p <- parsOf(im, nSub)
      invisible(rxode2::rxSolve(im, p, ev, cores = 1L)) # warm
      tv <- numeric(REPS)
      for (r in seq_len(REPS)) {
        t0 <- proc.time()[["elapsed"]]
        invisible(rxode2::rxSolve(im, p, ev, cores = 1L))
        tv[r] <- proc.time()[["elapsed"]] - t0
      }
      usObs <- stats::median(tv) / (nSub * nObs) * 1e6
      row <- data.frame(mode = mode, ncmt = ncmt, nSub = nSub, nObs = nObs,
                        uniform = unif, stateReads = stateReads,
                        usObs = usObs, load = loadAvg())
      print(row)
      out <- rbind(out, row)
    }
  }
  saveRDS(out, sprintf("bench/results/lincmt_p5_ab_%s.rds", mode))
} else if (mode == "fit") {
  # FOCEi 40x100 uniform fit on this TREE (linCmt arm), plus the ODE arm
  # when TREE=head (the ODE arm is emission-independent).
  d <- local({
    ui <- nlmixr2est::nlmixr(mkUi(2L))
    ev <- evOf(40L, 100L, TRUE)
    s <- rxode2::rxSolve(ui, ev, returnType = "data.frame", seed = 42)
    vcol <- intersect(c("sim", "cp", "value"), names(s))[1]
    set.seed(42)
    obs <- data.frame(id = s$id, time = s$time, amt = NA_real_,
                      evid = 0L, ii = 0, dv = s[[vcol]] + stats::rnorm(nrow(s), 0, 0.7))
    dose <- data.frame(id = seq_len(40L), time = 0, amt = 100,
                       evid = 1L, ii = 12, dv = NA_real_)
    dose$addl <- 3L; obs$addl <- 0L
    d <- rbind(dose, obs)
    d[order(d$id, d$time, -d$evid), ]
  })
  ctl <- nlmixr2est::foceiControl(print = 0L, calcTables = FALSE,
                                  covMethod = "")
  tv <- numeric(REPS)
  for (r in seq_len(REPS)) {
    t0 <- proc.time()[["elapsed"]]
    f <- suppressWarnings(suppressMessages(
      nlmixr2est::nlmixr(mkUi(2L), d, est = "focei", control = ctl)))
    tv[r] <- proc.time()[["elapsed"]] - t0
  }
  cat(sprintf("fit tree=%s median %.2f s objf %.6f load %.2f\n",
              Sys.getenv("TREE"), stats::median(tv), f$objective, loadAvg()))
}
