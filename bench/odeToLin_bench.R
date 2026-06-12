#!/usr/bin/env Rscript
## Benchmark: odeToLin() conversion speedup
##
##   NOT_CRAN=true Rscript bench/odeToLin_bench.R [--quick]
##
## Compares wall-clock time for three solve paths across 1-cmt, 2-cmt, and
## 3-cmt oral PK models:
##   ODE     : original ODE model (LSODA)
##   linCmt  : model written directly with linCmt() (analytical, no conversion)
##   odeToLin: ODE model auto-converted by odeToLin() before solving; pre-compiled
##   useLinCmt: ODE model solved with rxSolve(..., useLinCmt=TRUE) (incl. conversion
##              overhead on every call)

.benchRoot <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(".")
}
.quick <- "--quick" %in% commandArgs(trailingOnly = TRUE)

suppressMessages(devtools::load_all(.benchRoot, quiet = TRUE))

.reps  <- if (.quick) 3L else 10L
.npop  <- if (.quick) 500L else 2000L

cat(sprintf("rxode2 version: %s\n", as.character(packageVersion("rxode2"))))
cat(sprintf("reps=%d  npop=%d\n\n", .reps, .npop))

## Median wall-clock time (seconds) of expr over .reps reps after one warmup.
.timeIt <- function(expr, reps = .reps) {
  eval(expr, envir = parent.frame())  # warmup
  ts <- vapply(seq_len(reps), function(i) {
    t0 <- Sys.time(); eval(expr, envir = parent.frame())
    as.numeric(Sys.time() - t0, units = "secs")
  }, numeric(1))
  median(ts)
}

## -------------------------------------------------------------------------
## Model definitions
## -------------------------------------------------------------------------

.def1cmt <- function() {
  ini({ tka <- 0.45; tcl <- 1; tv <- 3.45
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    d/dt(depot)  <- -ka * depot
    d/dt(center) <- ka * depot - cl/v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
}

.def1cmtLin <- function() {
  ini({ tka <- 0.45; tcl <- 1; tv <- 3.45
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    cp <- linCmt()
    cp ~ add(add.sd)
  })
}

.def2cmt <- function() {
  ini({ tka <- 0.5; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
  model({
    ka  <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v  <- exp(tv  + eta.v)
    q   <- exp(tq);           vp <- exp(tvp)
    d/dt(depot)      <- -ka * depot
    d/dt(center)     <- ka * depot - (cl+q)/v * center + q/vp * peripheral
    d/dt(peripheral) <- q/v * center - q/vp * peripheral
    cp <- center / v
    cp ~ add(add.sd)
  })
}

.def2cmtLin <- function() {
  ini({ tka <- 0.5; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
  model({
    ka  <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v  <- exp(tv  + eta.v)
    q   <- exp(tq);           vp <- exp(tvp)
    cp <- linCmt()
    cp ~ add(add.sd)
  })
}

.def3cmt <- function() {
  ini({ tka <- 0.5; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6; tq2 <- 0.3; tvp2 <- 10
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
  model({
    ka  <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v   <- exp(tv   + eta.v)
    q   <- exp(tq);           vp <- exp(tvp);          q2  <- exp(tq2); vp2 <- exp(tvp2)
    d/dt(depot)      <- -ka * depot
    d/dt(center)     <- ka * depot - (cl+q+q2)/v * center + q/vp * periph1 + q2/vp2 * periph2
    d/dt(periph1)    <- q/v * center - q/vp * periph1
    d/dt(periph2)    <- q2/v * center - q2/vp2 * periph2
    cp <- center / v
    cp ~ add(add.sd)
  })
}

.def3cmtLin <- function() {
  ini({ tka <- 0.5; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6; tq2 <- 0.3; tvp2 <- 10
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
  model({
    ka  <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v   <- exp(tv   + eta.v)
    q   <- exp(tq);           vp <- exp(tvp);          q2  <- exp(tq2); vp2 <- exp(tvp2)
    cp <- linCmt()
    cp ~ add(add.sd)
  })
}

## -------------------------------------------------------------------------
## Compile / convert
## -------------------------------------------------------------------------

cat("Compiling models ...\n")
m1ode <- suppressMessages(rxode2(.def1cmt))
m1lin <- suppressMessages(rxode2(.def1cmtLin))
m1ctl <- suppressMessages(odeToLin(m1ode))    # pre-converted, compiled once

m2ode <- suppressMessages(rxode2(.def2cmt))
m2lin <- suppressMessages(rxode2(.def2cmtLin))
m2ctl <- suppressMessages(odeToLin(m2ode))

m3ode <- suppressMessages(rxode2(.def3cmt))
m3lin <- suppressMessages(rxode2(.def3cmtLin))
m3ctl <- suppressMessages(odeToLin(m3ode))

ev <- et(amt = 300, ii = 12, addl = 13) |> et(seq(0, 168, by = 0.5))

## Verify correctness (odeToLin matches linCmt)
.chk <- function(label, r1, r2) {
  d <- max(abs(r1$cp - r2$cp), na.rm = TRUE)
  cat(sprintf("  %-40s max|diff| = %.2e  [%s]\n", label, d,
              if (d < 1e-6) "PASS" else "FAIL"))
}
cat("\nCorrectness (ODE tight-tol vs odeToLin, same seed):\n")
.chk("1cmt odeToLin vs ODE (tight)",
     suppressMessages(rxSolve(m1ode, ev, nSub = 10, seed = 42, atol = 1e-12, rtol = 1e-10)),
     suppressMessages(rxSolve(m1ctl, ev, nSub = 10, seed = 42)))
.chk("2cmt odeToLin vs ODE (tight)",
     suppressMessages(rxSolve(m2ode, ev, nSub = 10, seed = 42, atol = 1e-12, rtol = 1e-10)),
     suppressMessages(rxSolve(m2ctl, ev, nSub = 10, seed = 42)))
.chk("3cmt odeToLin vs ODE (tight)",
     suppressMessages(rxSolve(m3ode, ev, nSub = 10, seed = 42, atol = 1e-12, rtol = 1e-10)),
     suppressMessages(rxSolve(m3ctl, ev, nSub = 10, seed = 42)))

## -------------------------------------------------------------------------
## Timing
## -------------------------------------------------------------------------

.header <- function(s) cat(sprintf("\n=== %s ===\n", s))
.row    <- function(label, t_ms) cat(sprintf("  %-28s %7.2f ms\n", label, t_ms))
.ratio  <- function(t_num, t_den) sprintf("%.2fx", t_den / t_num)

.time_ms <- function(expr) .timeIt(expr) * 1000

.header(sprintf("single-subject solve (1 rep = 1 subject, %d reps)", .reps))
t1s_ode <- .time_ms(quote(suppressMessages(rxSolve(m1ode, ev))))
t1s_lin <- .time_ms(quote(suppressMessages(rxSolve(m1lin, ev))))
t1s_ctl <- .time_ms(quote(suppressMessages(rxSolve(m1ctl, ev))))
t1s_use <- .time_ms(quote(suppressMessages(rxSolve(m1ode, ev, useLinCmt = TRUE))))
.row("1cmt ODE",                 t1s_ode)
.row("1cmt linCmt (native)",     t1s_lin)
.row("1cmt odeToLin (compiled)", t1s_ctl)
.row("1cmt useLinCmt=TRUE",      t1s_use)
cat(sprintf("  speedup odeToLin vs ODE: %s   useLinCmt vs ODE: %s\n",
            .ratio(t1s_ctl, t1s_ode), .ratio(t1s_use, t1s_ode)))

t2s_ode <- .time_ms(quote(suppressMessages(rxSolve(m2ode, ev))))
t2s_lin <- .time_ms(quote(suppressMessages(rxSolve(m2lin, ev))))
t2s_ctl <- .time_ms(quote(suppressMessages(rxSolve(m2ctl, ev))))
t2s_use <- .time_ms(quote(suppressMessages(rxSolve(m2ode, ev, useLinCmt = TRUE))))
.row("2cmt ODE",                 t2s_ode)
.row("2cmt linCmt (native)",     t2s_lin)
.row("2cmt odeToLin (compiled)", t2s_ctl)
.row("2cmt useLinCmt=TRUE",      t2s_use)
cat(sprintf("  speedup odeToLin vs ODE: %s   useLinCmt vs ODE: %s\n",
            .ratio(t2s_ctl, t2s_ode), .ratio(t2s_use, t2s_ode)))

t3s_ode <- .time_ms(quote(suppressMessages(rxSolve(m3ode, ev))))
t3s_lin <- .time_ms(quote(suppressMessages(rxSolve(m3lin, ev))))
t3s_ctl <- .time_ms(quote(suppressMessages(rxSolve(m3ctl, ev))))
t3s_use <- .time_ms(quote(suppressMessages(rxSolve(m3ode, ev, useLinCmt = TRUE))))
.row("3cmt ODE",                 t3s_ode)
.row("3cmt linCmt (native)",     t3s_lin)
.row("3cmt odeToLin (compiled)", t3s_ctl)
.row("3cmt useLinCmt=TRUE",      t3s_use)
cat(sprintf("  speedup odeToLin vs ODE: %s   useLinCmt vs ODE: %s\n",
            .ratio(t3s_ctl, t3s_ode), .ratio(t3s_use, t3s_ode)))

.header(sprintf("population solve (%d subjects)", .npop))
t1p_ode <- .time_ms(quote(suppressMessages(rxSolve(m1ode, ev, nSub = .npop))))
t1p_lin <- .time_ms(quote(suppressMessages(rxSolve(m1lin, ev, nSub = .npop))))
t1p_ctl <- .time_ms(quote(suppressMessages(rxSolve(m1ctl, ev, nSub = .npop))))
t1p_use <- .time_ms(quote(suppressMessages(rxSolve(m1ode, ev, nSub = .npop, useLinCmt = TRUE))))
.row("1cmt ODE",                 t1p_ode)
.row("1cmt linCmt (native)",     t1p_lin)
.row("1cmt odeToLin (compiled)", t1p_ctl)
.row("1cmt useLinCmt=TRUE",      t1p_use)
cat(sprintf("  speedup odeToLin vs ODE: %s   useLinCmt vs ODE: %s\n",
            .ratio(t1p_ctl, t1p_ode), .ratio(t1p_use, t1p_ode)))

t2p_ode <- .time_ms(quote(suppressMessages(rxSolve(m2ode, ev, nSub = .npop))))
t2p_lin <- .time_ms(quote(suppressMessages(rxSolve(m2lin, ev, nSub = .npop))))
t2p_ctl <- .time_ms(quote(suppressMessages(rxSolve(m2ctl, ev, nSub = .npop))))
t2p_use <- .time_ms(quote(suppressMessages(rxSolve(m2ode, ev, nSub = .npop, useLinCmt = TRUE))))
.row("2cmt ODE",                 t2p_ode)
.row("2cmt linCmt (native)",     t2p_lin)
.row("2cmt odeToLin (compiled)", t2p_ctl)
.row("2cmt useLinCmt=TRUE",      t2p_use)
cat(sprintf("  speedup odeToLin vs ODE: %s   useLinCmt vs ODE: %s\n",
            .ratio(t2p_ctl, t2p_ode), .ratio(t2p_use, t2p_ode)))

t3p_ode <- .time_ms(quote(suppressMessages(rxSolve(m3ode, ev, nSub = .npop))))
t3p_lin <- .time_ms(quote(suppressMessages(rxSolve(m3lin, ev, nSub = .npop))))
t3p_ctl <- .time_ms(quote(suppressMessages(rxSolve(m3ctl, ev, nSub = .npop))))
t3p_use <- .time_ms(quote(suppressMessages(rxSolve(m3ode, ev, nSub = .npop, useLinCmt = TRUE))))
.row("3cmt ODE",                 t3p_ode)
.row("3cmt linCmt (native)",     t3p_lin)
.row("3cmt odeToLin (compiled)", t3p_ctl)
.row("3cmt useLinCmt=TRUE",      t3p_use)
cat(sprintf("  speedup odeToLin vs ODE: %s   useLinCmt vs ODE: %s\n",
            .ratio(t3p_ctl, t3p_ode), .ratio(t3p_use, t3p_ode)))

.header("odeToLin conversion overhead (one-time cost)")
t_conv1 <- .time_ms(quote(suppressMessages(odeToLin(m1ode))))
t_conv2 <- .time_ms(quote(suppressMessages(odeToLin(m2ode))))
t_conv3 <- .time_ms(quote(suppressMessages(odeToLin(m3ode))))
.row("odeToLin(1cmt ODE)",  t_conv1)
.row("odeToLin(2cmt ODE)",  t_conv2)
.row("odeToLin(3cmt ODE)",  t_conv3)
cat(sprintf("  (incl. model compilation; amortized over all subsequent solves)\n"))

cat("\n")
