# Correctness of the RX_LINCMT_PHI transition-matrix prototype against the
# shipped tail path: same optimized binary, same model, same events, the
# switch flipped between two processes (it is read once per process).  The
# propagation reassociates the kernel's own arithmetic, so the gate here is
# round-off equivalence (<= 1e-13 relative), NOT bitwise -- a departure
# from every shipped change in this project, and the reason a production
# version would need that accepted explicitly.
#
# Usage: RX_LINCMT_PHI=<0|1> Rscript bench/lincmt_transition_matrix_valid.R
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
MODE <- as.integer(Sys.getenv("RX_LINCMT_PHI", "0"))

gradModel <- function(ncmt, oral0, trans, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, %d, p1, v1, p2, p3, p4, p5, ka",
                  ncmt, oral0, trans)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)", k,
                                              sprintf(args, -2L, k)), ""))
  suppressWarnings(rxode2::rxode2(paste(lines, collapse = "\n")))
}

# trans 1 (cl/v/q/vp), trans 2 (k-style) and trans 11 exercise different
# getJacCp/micro paths; the window/tail is shared by all of them.
cases <- list(
  list(nm = "1cmt iv t1",    ncmt = 1, oral0 = 0, trans = 1,
       p = c(p1=2.1, v1=21, p2=0, p3=0, p4=0, p5=0, ka=0), dirs = 0:1),
  list(nm = "1cmt oral t1",  ncmt = 1, oral0 = 1, trans = 1,
       p = c(p1=2.1, v1=21, p2=0, p3=0, p4=0, p5=0, ka=1.3), dirs = 0:2),
  list(nm = "2cmt iv t1",    ncmt = 2, oral0 = 0, trans = 1,
       p = c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0, p5=0, ka=0), dirs = 0:3),
  list(nm = "2cmt oral t1",  ncmt = 2, oral0 = 1, trans = 1,
       p = c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0, p5=0, ka=1.3), dirs = 0:4),
  list(nm = "2cmt oral t2",  ncmt = 2, oral0 = 1, trans = 2,
       p = c(p1=0.1, v1=21, p2=0.15, p3=0.08, p4=0, p5=0, ka=1.3), dirs = 0:4),
  list(nm = "3cmt iv t1",    ncmt = 3, oral0 = 0, trans = 1,
       p = c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0.9, p5=61, ka=0), dirs = 0:5),
  list(nm = "3cmt oral t1",  ncmt = 3, oral0 = 1, trans = 1,
       p = c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0.9, p5=61, ka=1.3), dirs = 0:6)
)

# uniform, non-uniform, multi-dose (memo reuse across dosing intervals) and
# an infusion regimen (affine rows -- the prototype must decline these and
# fall back, so they double as a fallback check).
regimens <- list(
  uniform = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                  seq(0.25, 24, by = 0.25)),
  nonunif = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1),
                                  cumsum(seq(0.05, 0.55, length.out = 96))),
  multi   = function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1,
                                             ii = 12, addl = 3),
                                  seq(0.5, 48, by = 0.5)),
  infusion= function() rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1,
                                             rate = 20),
                                  seq(0.25, 24, by = 0.25))
)

# One regimen per process: the delta memo's give-up guard is permanent for
# a window, so a non-uniform regimen run earlier in the same process
# disarms every later regimen sharing that theta.
REG <- Sys.getenv("REGIMEN", "uniform")
regimens <- regimens[REG]

out <- list()
for (cs in cases) {
  mod <- gradModel(cs$ncmt, cs$oral0, cs$trans, cs$dirs)
  for (rn in names(regimens)) {
    ev <- regimens[[rn]]()
    rxode2:::linCmtSeqStats(TRUE)
    s <- rxode2::rxSolve(mod, cs$p, ev, cores = 1L, addDosing = FALSE,
                         linCmtSensType = "AD")
    st <- rxode2:::linCmtSeqStats(TRUE)
    cols <- c("cp", paste0("d", cs$dirs))
    out[[paste(cs$nm, rn)]] <- list(
      case = cs$nm, regimen = rn,
      val = as.matrix(as.data.frame(s)[, cols, drop = FALSE]),
      phiRows = unname(st[["phiRows"]]), phiBuild = unname(st[["phiBuild"]]),
      tailRows = unname(st[["seqTailRows"]]))
  }
}
saveRDS(out, sprintf("bench/results/transition_valid_mode%d_%s.rds", MODE, REG))
message("saved mode ", MODE, " regimen ", REG)
