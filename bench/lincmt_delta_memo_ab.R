# A/B for the delta-keyed exponential memo (linCmtWinDeltaSlot): the same
# optimized binary solved with the memo ON (default) vs OFF
# (RX_LINCMT_DELTA_MEMO=off), so the comparison isolates the memo exactly.
# Cells: uniform dense (the win case: one exponential build per window),
# non-uniform dense (the honest no-reuse case) and the 40x100 FOCEi-style
# inner-model solve shape.  Pinned single core; run on an idle machine and
# record the load per cell.
#
# Usage: taskset -c <idle core> Rscript bench/lincmt_delta_memo_ab.R
message("== lincmt_delta_memo_ab ==")
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
REPS <- as.integer(Sys.getenv("REPS", "3"))

loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

gradModel <- function(ncmt, oral0, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                  ncmt, oral0)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(k) {
               sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
             }, ""))
  suppressWarnings(rxode2::rxode2(paste(lines, collapse = "\n")))
}
parsFor <- function(ncmt, oral0) {
  p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
  if (ncmt < 2) p[c("q", "vp")] <- 0
  if (ncmt < 3) p[c("q2", "vp2")] <- 0
  if (oral0 == 0) p["ka"] <- 0
  p
}
mkEv <- function(nSub, obsT) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    rbind(data.frame(id = i, time = 0, amt = 100, evid = 1, cmt = 1,
                     rate = 0, ii = 0, ss = 0),
          data.frame(id = i, time = obsT, amt = 0, evid = 0, cmt = 1,
                     rate = 0, ii = 0, ss = 0))
  }))
}

timeCell <- function(mod, pars, ev, memo) {
  env <- if (memo) c(RX_LINCMT_DELTA_MEMO = NA) else c(RX_LINCMT_DELTA_MEMO = "off")
  withr::with_envvar(env, {
    ts <- vapply(seq_len(REPS), function(r) {
      t0 <- proc.time()[["elapsed"]]
      invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE))
      proc.time()[["elapsed"]] - t0
    }, 0.0)
    median(ts)
  })
}

cells <- list(
  list(name = "2cmt uniform 400x1000", ncmt = 2, oral0 = 1, dirs = 0:4,
       ev = quote(mkEv(400L, seq(0.1, 100, by = 0.1)))),
  list(name = "3cmt uniform 400x1000", ncmt = 3, oral0 = 1, dirs = 0:6,
       ev = quote(mkEv(400L, seq(0.1, 100, by = 0.1)))),
  list(name = "2cmt nonuniform 400x1000", ncmt = 2, oral0 = 1, dirs = 0:4,
       ev = quote(mkEv(400L, cumsum(seq(0.02, 0.18, length.out = 1000))))),
  list(name = "3cmt nonuniform 400x1000", ncmt = 3, oral0 = 1, dirs = 0:6,
       ev = quote(mkEv(400L, cumsum(seq(0.02, 0.18, length.out = 1000))))),
  list(name = "2cmt uniform 40x100", ncmt = 2, oral0 = 1, dirs = 0:4,
       ev = quote(mkEv(40L, seq(0.5, 50, by = 0.5))))
)

res <- do.call(rbind, lapply(cells, function(cl) {
  mod <- gradModel(cl$ncmt, cl$oral0, cl$dirs)
  pars <- parsFor(cl$ncmt, cl$oral0)
  ev <- eval(cl$ev)
  nObs <- sum(ev$evid == 0)
  # warm-up (compile/caches) then timed
  invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE))
  tOff <- timeCell(mod, pars, ev, memo = FALSE)
  tOn <- timeCell(mod, pars, ev, memo = TRUE)
  rxode2:::linCmtSeqStats(TRUE)
  withr::with_envvar(c(RX_LINCMT_DELTA_MEMO = NA),
                     invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L,
                                               addDosing = FALSE)))
  st <- rxode2:::linCmtSeqStats(TRUE)
  data.frame(cell = cl$name, nObs = nObs,
             usObsOff = 1e6*tOff/nObs, usObsOn = 1e6*tOn/nObs,
             gain = tOff/tOn, expBuild = st[["expBuild"]],
             expHit = st[["expHit"]], load = loadAvg())
}))
print(res, digits = 4)
attr(res, "provenance") <- list(
  when = format(Sys.time(), tz = "UTC"), reps = REPS,
  commit = system("git rev-parse --short HEAD", intern = TRUE),
  note = "memo ON vs OFF on the same optimized binary; pinned single core")
saveRDS(res, "bench/results/lincmt_delta_memo_ab.rds")
message("saved bench/results/lincmt_delta_memo_ab.rds")
