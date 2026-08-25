# Is the transition-matrix operation order closer to or farther from the
# truth than the tail's order?  Both evaluate the same exact closed form,
# so "which is more correct" is a question about floating point, not about
# the mathematics, and it needs a reference outside both.
#
# Reference: the same model integrated as differential equations at very
# tight tolerances (useLinCmt=FALSE, so it is genuinely integrated).  That
# gives an independent value for the concentration; the two closed-form
# orders are then scored against it.  Also reports absolute (not just
# relative) disagreement, since a relative figure taken near a sign change
# of a gradient column exaggerates a difference that is numerically
# irrelevant.
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)

# 3-cmt oral, the least well-conditioned kernel (cubic-root eigenvalues),
# in the two parameterizations that showed the largest disagreement.
P3 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0.9, p5=61, ka=1.3)
mkGrad <- function(trans, dirs=0:6) {
  args <- sprintf("rx__PTR__, t, 1, 3, 1, %%d, %%d, %d, p1, v1, p2, p3, p4, p5, ka", trans)
  suppressWarnings(rxode2::rxode2(paste(c(
    sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
    vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k)), "")),
    collapse="\n")))
}
ev <- rxode2::et(rxode2::et(amt=100, time=0, cmt=1), seq(0.25, 24, by=0.25))
evNU <- rxode2::et(rxode2::et(amt=100, time=0, cmt=1),
                   cumsum(seq(0.05, 0.55, length.out=96)))

# Independent reference for the concentration: the equivalent ODE system,
# integrated (not routed back through linCmt) at 1e-14 tolerances.
odeMod <- suppressWarnings(rxode2::rxode2("
  ke  = p1/v1
  k12 = p2/v1
  k21 = p2/p3
  k13 = p4/v1
  k31 = p4/p5
  d/dt(depot)   = -ka*depot
  d/dt(central) =  ka*depot - (ke+k12+k13)*central + k21*per1 + k31*per2
  d/dt(per1)    =  k12*central - k21*per1
  d/dt(per2)    =  k13*central - k31*per2
  cp = central/v1
"))

out <- list()
# The reference ODE encodes trans-1 parameter meanings, so it is a valid
# reference for trans 1 only; trans 10 is reported pairwise (absolute
# spread against the scale of the values) without it.
for (tr in c(1L, 10L)) {
  mod <- mkGrad(tr)
  for (rn in c("uniform", "nonunif")) {
    e <- if (rn == "uniform") ev else evNU
    off <- as.data.frame(rxode2::rxSolve(mod, P3, e, cores=1L, addDosing=FALSE,
                                         linCmtSensType="AD", linCmtSensPhi=FALSE))
    on  <- as.data.frame(rxode2::rxSolve(mod, P3, e, cores=1L, addDosing=FALSE,
                                         linCmtSensType="AD", linCmtSensPhi=TRUE))
    ref <- as.data.frame(rxode2::rxSolve(odeMod, P3, e, cores=1L, addDosing=FALSE,
                                         useLinCmt=FALSE, atol=1e-12, rtol=1e-12,
                                         maxsteps=200000L))
    n <- min(nrow(off), nrow(ref))
    dOff <- abs(off$cp[seq_len(n)] - ref$cp[seq_len(n)])
    dOn  <- abs(on$cp[seq_len(n)]  - ref$cp[seq_len(n)])
    sc <- pmax(abs(ref$cp[seq_len(n)]), 1e-300)
    # gradient columns: absolute and relative spread between the two orders
    gc <- paste0("d", 0:6)
    a <- as.matrix(off[seq_len(n), gc]); b <- as.matrix(on[seq_len(n), gc])
    out[[length(out)+1L]] <- data.frame(
      trans=tr, regimen=rn,
      cpRelTail=if (tr == 1L) max(dOff/sc) else NA_real_,
      cpRelPhi=if (tr == 1L) max(dOn/sc) else NA_real_,
      cpTailCloser=if (tr == 1L) sum(dOff < dOn) else NA_integer_,
      cpPhiCloser=if (tr == 1L) sum(dOn < dOff) else NA_integer_,
      cpTied=if (tr == 1L) sum(dOn == dOff) else NA_integer_,
      gradMaxAbs=max(abs(b-a)), gradMaxRel=max(abs(b-a)/pmax(abs(a),1e-300)),
      gradScale=max(abs(a)),
      stringsAsFactors=FALSE)
  }
}
out <- do.call(rbind, out)
saveRDS(out, "bench/results/phi_accuracy_ref.rds")
print(out, row.names=FALSE)
cat("\ncpRelTail / cpRelPhi: distance of each closed-form order from the",
    "\nintegrated reference (smaller = closer to truth).\n")
cat("gradMaxAbs vs gradScale: absolute spread between the two orders",
    "against the largest gradient value present -- the relative figure is
taken elementwise and is inflated where a gradient column crosses zero.\n")

# Engage rule in a FRESH window: a design whose intervals never repeat must
# build no transition matrix at all.  (Running two arms in one process
# leaves the previous arm's intervals in the memo, so this check needs its
# own window -- a distinct theta gives one.)
P3b <- P3; P3b["p1"] <- 2.1000001
rxode2:::linCmtSeqStats(TRUE)
invisible(rxode2::rxSolve(mkGrad(1L), P3b, evNU, cores=1L, addDosing=FALSE,
                          linCmtSensType="AD", linCmtSensPhi=TRUE))
stNU <- rxode2:::linCmtSeqStats(TRUE)
rxode2:::linCmtSeqStats(TRUE)
invisible(rxode2::rxSolve(mkGrad(1L), P3b, ev, cores=1L, addDosing=FALSE,
                          linCmtSensType="AD", linCmtSensPhi=TRUE))
stU <- rxode2:::linCmtSeqStats(TRUE)
cat(sprintf("\nfresh window, never-repeating intervals: phiBuild=%d phiRows=%d tailRows=%d\n",
            stNU[["phiBuild"]], stNU[["phiRows"]], stNU[["seqTailRows"]]))
cat(sprintf("fresh window, regular sampling:          phiBuild=%d phiRows=%d tailRows=%d\n",
            stU[["phiBuild"]], stU[["phiRows"]], stU[["seqTailRows"]]))
