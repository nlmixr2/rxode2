# Phase-0 gate for a transition-matrix propagation of linCmt()
# sensitivities.  Three predictions in this project have been contradicted
# by measurement, each because the per-row arithmetic was NOT the dominant
# cost, so nothing is built here until an ablation says how much of a
# sensitivity row the per-direction fvar work actually is.
#
# RX_LINCMT_ABLATE (exploration-only switch, read once per process):
#   0  full path (default, correct)
#   1  the *Tail kernel replaced by a bounded stand-in; the per-direction
#      constant fill (solComp structs, yp, preE) still runs
#   2  the whole per-direction body replaced by 2*m*m double multiply-adds
#      -- the cost a transition-matrix step would actually pay
# Modes 1 and 2 produce wrong derivatives on purpose; they are timing
# counterfactuals.  Ceiling of the design = t(0) / t(2).
#
# Usage: taskset -c <idle core> Rscript bench/lincmt_transition_matrix_gate.R
message("== lincmt_transition_matrix_gate (mode ",
        Sys.getenv("RX_LINCMT_ABLATE", "0"), ") ==")
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
REPS <- as.integer(Sys.getenv("REPS", "5"))
MODE <- as.integer(Sys.getenv("RX_LINCMT_ABLATE", "0"))

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
  ev <- rxode2::et(amt = 100, time = 0, cmt = 1)
  ev <- rxode2::et(ev, obsT)
  rxode2::et(ev, id = seq_len(nSub))
}

cells <- list(
  list(name = "2cmt uniform 400x1000", ncmt = 2, oral0 = 1, dirs = 0:4,
       nSub = 400L, obsT = seq(0.1, 100, by = 0.1)),
  list(name = "3cmt uniform 400x1000", ncmt = 3, oral0 = 1, dirs = 0:6,
       nSub = 400L, obsT = seq(0.1, 100, by = 0.1)),
  list(name = "2cmt nonuniform 400x1000", ncmt = 2, oral0 = 1, dirs = 0:4,
       nSub = 400L, obsT = cumsum(seq(0.02, 0.18, length.out = 1000))),
  list(name = "3cmt nonuniform 400x1000", ncmt = 3, oral0 = 1, dirs = 0:6,
       nSub = 400L, obsT = cumsum(seq(0.02, 0.18, length.out = 1000)))
)

res <- do.call(rbind, lapply(cells, function(cl) {
  mod <- gradModel(cl$ncmt, cl$oral0, cl$dirs)
  pars <- parsFor(cl$ncmt, cl$oral0)
  ev <- mkEv(cl$nSub, cl$obsT)
  nObs <- cl$nSub*length(cl$obsT)
  invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE,
                            linCmtSensType = "AD"))       # warm
  ts <- vapply(seq_len(REPS), function(r) {
    t0 <- proc.time()[["elapsed"]]
    invisible(rxode2::rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE,
                              linCmtSensType = "AD"))
    proc.time()[["elapsed"]] - t0
  }, 0.0)
  data.frame(cell = cl$name, mode = MODE, ncmt = cl$ncmt,
             nDir = length(cl$dirs), sec = median(ts),
             usPerObs = 1e6*median(ts)/nObs, load = loadAvg(),
             stringsAsFactors = FALSE)
}))
print(res)
dir.create("bench/results", showWarnings = FALSE, recursive = TRUE)
saveRDS(res, sprintf("bench/results/transition_gate_mode%d.rds", MODE))
message("saved bench/results/transition_gate_mode", MODE, ".rds")
