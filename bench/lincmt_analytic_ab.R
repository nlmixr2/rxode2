# A/B over the three ways a linCmt() sensitivity row can be evaluated, and
# the two forward-mode scalars that can drive them.
#
#   phi = 0  row tail, one closed-form evaluation per requested direction
#   phi = 1  probe-built transition matrix, reused where an interval recurs
#   phi = 2  closed-form transition matrix, assembled on every ordinary row
#   sens AD  one forward pass per direction
#   sens ADm all directions in one forward pass
#
# All are the same exact closed form; phi = 1 and phi = 2 sum it in a
# different order (matrix first, then applied).  The headroom being aimed at
# was measured with RX_LINCMT_ABLATE=2 in
# bench/results/analytic_phase0_ceiling.rds.
#
# Optimized build only -- devtools::load_all() WITH compilation builds at -O0
# and has inverted this comparison before (see NEWS.md 5.1.7).  One model
# compile per cell, every arm timed against it, so the arms see identical
# work.
#
# Usage: REPS=5 taskset -c <idle core> Rscript bench/lincmt_analytic_ab.R
REPS <- as.integer(Sys.getenv("REPS", "5"))
NSUB <- as.integer(Sys.getenv("NSUB", "100"))
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
stopifnot(Sys.getenv("RX_LINCMT_PHI") == "")  # the env force would pin every arm
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

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
designs <- list(
  uniform    = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                     seq(0.1, 100, by = 0.1)),
  multidose  = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1,
                                                ii = 24, addl = 3),
                                     seq(0.1, 100, by = 0.1)),
  nonuniform = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                     cumsum(seq(0.02, 0.18, length.out = 1000))),
  infusion   = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1,
                                                dur = 4, ii = 24, addl = 3),
                                     seq(0.1, 100, by = 0.1)))

# A FOCEi inner model asks for one direction per eta, so 2-5 is the range
# that matters for the NONMEM per-direction comparison; the widest a 3-cmt
# oral model can request is 7.
arms <- list(list(sens = "AD",  phi = 0L), list(sens = "AD",  phi = 1L),
             list(sens = "AD",  phi = 2L), list(sens = "ADm", phi = 0L),
             list(sens = "ADm", phi = 2L))

res <- list()
for (dn in names(designs)) {
  for (nc in 2:3) {
    for (k in c(2L, 5L, 2L * nc + 1L)) {
      if (k > 2L * nc + 1L) next
      dirs <- seq_len(k) - 1L
      mod <- gradModel(nc, 1L, dirs)
      pars <- parsFor(nc, 1L)
      ev <- rxode2::et(designs[[dn]](), id = seq_len(NSUB))
      nObs <- NSUB * 1000L
      for (arm in arms) {
        run <- function() invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L,
                                                    addDosing = FALSE,
                                                    linCmtSensType = arm$sens,
                                                    linCmtSensPhi = arm$phi))
        run()
        rxode2:::linCmtSeqStats(TRUE)
        ts <- vapply(seq_len(REPS), function(r) {
          t0 <- proc.time()[["elapsed"]]
          run()
          proc.time()[["elapsed"]] - t0
        }, 0.0)
        st <- rxode2:::linCmtSeqStats(TRUE)
        res[[length(res) + 1L]] <-
          data.frame(design = dn, ncmt = nc, nDir = k,
                     arm = sprintf("%s/phi%d", arm$sens, arm$phi),
                     usPerObs = 1e6 * median(ts) / nObs,
                     tailRows = unname(st[["seqTailRows"]]),
                     phiRows = unname(st[["phiRows"]]),
                     phiARows = unname(st[["phiAnalyticRows"]]),
                     dualRows = unname(st[["dualRows"]]),
                     load = loadAvg(), stringsAsFactors = FALSE)
      }
    }
  }
}
res <- do.call(rbind, res)
saveRDS(res, "bench/results/analytic_ab.rds")
w <- reshape(res[, c("design", "ncmt", "nDir", "arm", "usPerObs")],
             idvar = c("design", "ncmt", "nDir"), timevar = "arm", direction = "wide")
names(w) <- sub("^usPerObs\\.", "", names(w))
w$gain <- round(w$`AD/phi1` / w$`ADm/phi2`, 3)
print(w, row.names = FALSE, digits = 3)
