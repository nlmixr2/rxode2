# SUPERSEDED: this measurement was taken through devtools::load_all(), which
# compiles at -O0, and its conclusion (a count-based auto rule) does not hold
# on an optimized build -- see bench/lincmt_auto_optimized.R and
# bench/results/lincmt_auto_optimized.rds (every kernel, trans, mask and
# shape, pinned single thread): forward mode is at least as fast as reverse
# in every realistic cell.  Kept as the record of what the earlier rule rested on.
# Boundary measurement for the count-based linCmtSensType="auto" rule
# (PR #1280): forward-mode fvar costs one pass per requested direction k,
# reverse mode one adjoint sweep per compartment m = ncmt + oral0.  This
# times forced "AD" vs forced "ADr" through the real rxSolve() path for k
# at, just below and just above m on every config, two event shapes, one
# and all threads, so the comparison at k == m is pinned by measurement.
#
# Run from the package root:  Rscript bench/lincmt_auto_boundary.R
suppressMessages(devtools::load_all(".", quiet = TRUE))

.cfgs <- list(
  list(name = "1cmt-iv",   ncmt = 1L, oral0 = 0L),
  list(name = "1cmt-oral", ncmt = 1L, oral0 = 1L),
  list(name = "2cmt-iv",   ncmt = 2L, oral0 = 0L),
  list(name = "2cmt-oral", ncmt = 2L, oral0 = 1L),
  list(name = "3cmt-oral", ncmt = 3L, oral0 = 1L))

.pars <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 110, ka = 1.3)

# gradient model reading only the first k theta directions (which2 index ==
# theta index: p1, v1, p2, p3, p4, p5, ka = 2*ncmt)
.gradModel <- function(cfg, k) {
  npars <- 2L * cfg$ncmt + cfg$oral0
  dirs <- seq_len(k) - 1L
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                  cfg$ncmt, cfg$oral0)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(d) sprintf("d%d=linCmtB(%s)", d, sprintf(args, -2L, d)), ""))
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}

.ev <- function(nDoses, nObs, nSub, dtDose = 0.5, dtObs = 0.2) {
  doseT <- (seq_len(nDoses) - 1) * dtDose
  obsT <- max(doseT) + dtDose + dtObs * seq_len(nObs)
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    rbind(data.frame(id = i, time = doseT, amt = 100 * (1 + 0.01 * i), evid = 1, cmt = 1),
          data.frame(id = i, time = obsT + 0.01 * i, amt = 0, evid = 0, cmt = 1))
  }))
}

.tm <- function(fn, reps) {
  median(vapply(seq_len(reps), function(i) system.time(fn())[["elapsed"]], 0))
}

benchLinCmtAutoBoundary <- function(nSub = 40L, reps = 3L,
                                    shapes = list(c(11L, 200L), c(100L, 50L)),
                                    nThr = getRxThreads()) {
  out <- list()
  for (cfg in .cfgs) {
    m <- cfg$ncmt + cfg$oral0
    npars <- 2L * cfg$ncmt + cfg$oral0
    ks <- unique(pmax(1L, pmin(npars, c(m - 1L, m, m + 1L))))
    for (k in ks) {
      mod <- .gradModel(cfg, k)
      for (sh in shapes) {
        evt <- .ev(sh[1], sh[2], nSub)
        solve1 <- function(st, cores) {
          rxSolve(mod, .pars, evt, linCmtSensType = st, cores = cores,
                  returnType = "data.frame")
        }
        solve1("AD", 1L)  # warm
        row <- data.frame(config = cfg$name, m = m, k = k,
                          doses = sh[1], obs = sh[2],
                          fwd1 = .tm(function() solve1("AD", 1L), reps),
                          rev1 = .tm(function() solve1("ADr", 1L), reps),
                          fwdN = .tm(function() solve1("AD", nThr), reps),
                          revN = .tm(function() solve1("ADr", nThr), reps))
        row$revOverFwd1 <- row$fwd1 / row$rev1
        row$revOverFwdN <- row$fwdN / row$revN
        cat(sprintf("%-9s m=%d k=%d %3d/%3d  fwd1 %.3f rev1 %.3f (%.2fx)  fwdN %.3f revN %.3f (%.2fx)\n",
                    cfg$name, m, k, sh[1], sh[2], row$fwd1, row$rev1, row$revOverFwd1,
                    row$fwdN, row$revN, row$revOverFwdN))
        out[[length(out) + 1L]] <- row
      }
    }
  }
  res <- do.call(rbind, out)
  dir.create("bench/results", showWarnings = FALSE)
  saveRDS(res, "bench/results/lincmt_auto_boundary.rds")
  invisible(res)
}

if (sys.nframe() == 0L) benchLinCmtAutoBoundary()
