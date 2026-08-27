# A/B for the RX_LINCMT_PHI transition-matrix prototype: the same
# optimized binary with the switch off (shipped window+tail path) and on
# (Phi assembled once per theta-window x row gap, then plain double
# multiply-adds per row).  The switch is read once per process, so each
# arm is its own Rscript.  Pinned single core; idle machine; load per cell.
#
# Usage: RX_LINCMT_PHI=<0|1> taskset -c <core> Rscript bench/lincmt_transition_matrix_ab.R
MODE <- as.integer(Sys.getenv("RX_LINCMT_PHI", "0"))
WHAT <- Sys.getenv("WHAT", "solve")
message("== transition_matrix_ab phi=", MODE, " ", WHAT, " ==")
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
REPS <- as.integer(Sys.getenv("REPS", "5"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

if (WHAT == "solve") {
  gradModel <- function(ncmt, oral0, dirs) {
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)", k,
                                                sprintf(args, -2L, k)), ""))
    suppressWarnings(rxode2::rxode2(paste(lines, collapse = "\n")))
  }
  parsFor <- function(ncmt, oral0) {
    p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
    if (ncmt < 2) p[c("q", "vp")] <- 0
    if (ncmt < 3) p[c("q2", "vp2")] <- 0
    if (oral0 == 0) p["ka"] <- 0
    p
  }
  cells <- list(
    list(nm = "2cmt uniform 400x1000", ncmt = 2, dirs = 0:4, nSub = 400L,
         ev = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                    seq(0.1, 100, by = 0.1))),
    list(nm = "3cmt uniform 400x1000", ncmt = 3, dirs = 0:6, nSub = 400L,
         ev = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                    seq(0.1, 100, by = 0.1))),
    list(nm = "2cmt multidose 400x1000", ncmt = 2, dirs = 0:4, nSub = 400L,
         ev = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1,
                                               ii = 24, addl = 3),
                                    seq(0.1, 100, by = 0.1))),
    list(nm = "3cmt multidose 400x1000", ncmt = 3, dirs = 0:6, nSub = 400L,
         ev = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1,
                                               ii = 24, addl = 3),
                                    seq(0.1, 100, by = 0.1))),
    list(nm = "2cmt nonuniform 400x1000", ncmt = 2, dirs = 0:4, nSub = 400L,
         ev = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                    cumsum(seq(0.02, 0.18, length.out = 1000)))),
    list(nm = "3cmt nonuniform 400x1000", ncmt = 3, dirs = 0:6, nSub = 400L,
         ev = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                    cumsum(seq(0.02, 0.18, length.out = 1000))))
  )
  res <- do.call(rbind, lapply(cells, function(cl) {
    mod <- gradModel(cl$ncmt, 1L, cl$dirs)
    pars <- parsFor(cl$ncmt, 1L)
    ev <- rxode2::et(cl$ev(), id = seq_len(cl$nSub))
    nObs <- cl$nSub*1000L
    invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE,
                              linCmtSensType = "AD"))
    rxode2:::linCmtSeqStats(TRUE)
    ts <- vapply(seq_len(REPS), function(r) {
      t0 <- proc.time()[["elapsed"]]
      invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE,
                                linCmtSensType = "AD"))
      proc.time()[["elapsed"]] - t0
    }, 0.0)
    st <- rxode2:::linCmtSeqStats(TRUE)
    data.frame(cell = cl$nm, phi = MODE, sec = median(ts),
               usPerObs = 1e6*median(ts)/nObs,
               phiRows = unname(st[["phiRows"]]),
               tailRows = unname(st[["seqTailRows"]]),
               load = loadAvg(), stringsAsFactors = FALSE)
  }))
  print(res, row.names = FALSE)
  saveRDS(res, sprintf("bench/results/transition_ab_solve_phi%d_abl%s.rds", MODE, Sys.getenv("RX_LINCMT_ABLATE", "0")))
} else {
  suppressMessages(devtools::load_all("~/src/nlmixr2est",
                                      quiet = TRUE, helpers = FALSE))
  set.seed(1234)
  mod <- function() {
    ini({ tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 1.2; tv2 <- 3.9
      eta.ka ~ 0.4; eta.cl ~ 0.3; eta.v ~ 0.2
      prop.sd <- 0.15 })
    model({ ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
      q <- exp(tq); v2 <- exp(tv2)
      linCmt() ~ prop(prop.sd) })
  }
  evF <- et(amt = 100, ii = 24, addl = 3) |> et(seq(0.25, 96, length.out = 100L)) |>
    et(id = 1:40)
  simP <- c(tka = 0.45, tcl = 1, tv = 3.45, tq = 1.2, tv2 = 3.9)
  sim <- suppressWarnings(rxSolve(mod, evF, params = c(simP, prop.sd = 0.15),
    omega = lotri::lotri(eta.ka ~ 0.4, eta.cl ~ 0.3, eta.v ~ 0.2),
    addDosing = TRUE, seed = 42))
  dat <- as.data.frame(sim)[, c("id", "time", "sim")]
  names(dat)[3] <- "dv"
  d0 <- as.data.frame(evF$get.EventTable())
  d0 <- d0[d0$evid != 0, c("id", "time", "amt", "evid", "ii", "addl")]
  dat$amt <- NA; dat$evid <- 0; dat$ii <- 0; dat$addl <- 0
  dat <- rbind(transform(d0[, c("id","time","amt","evid","ii","addl")], dv = NA),
               dat[, c("id","time","amt","evid","ii","addl","dv")])
  dat <- dat[order(dat$id, dat$time, -dat$evid), ]
  # OUTER=0 gives the posthoc/inner cell (fast, and the only part Phi can
  # touch); OUTER>0 the full fit.  A full 40x100 fit did not fit this
  # exploration's foreground budget, so the reported cell is the posthoc.
  OUTER <- as.integer(Sys.getenv("OUTER", "0"))
  tf <- vapply(1:2, function(r) {
    t0 <- proc.time()[["elapsed"]]
    f <- suppressWarnings(suppressMessages(nlmixr2(mod, dat, est = "focei",
      control = foceiControl(calcTables = FALSE, print = 0L,
                             maxOuterIterations = OUTER))))
    el <- proc.time()[["elapsed"]] - t0
    attr(el, "objf") <- f$objective
    el
  }, 0)
  res <- data.frame(phi = MODE, outer = OUTER, secWarm = tf[2],
                    secAll = paste(round(tf, 1), collapse = "/"),
                    objf = attr(tf[2], "objf"), load = loadAvg())
  print(res, row.names = FALSE)
  saveRDS(res, sprintf("bench/results/transition_ab_fit_phi%d_outer%d.rds", MODE, OUTER))
}
