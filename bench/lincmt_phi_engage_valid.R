# Transition-matrix propagation (rxSolve(linCmtSensPhi=)) against the tail's
# own operation order, over the full config x trans x regimen x direction
# mask matrix.
#
# Both arms evaluate the SAME exact closed-form solution.  The tail
# accumulates the kernel's products in its own order; the transition form
# sums the interval's matrix first and then applies it.  Floating point is
# not associative, so the two can differ in the last few digits -- neither
# is an approximation of the other and neither is the more correct.  What
# this script measures is that the disagreement stays at round-off, and
# what the engage rule builds (Phi is assembled only where a row's interval
# repeats, so a design whose intervals never repeat must build none).
#
# Both arms run in one process so they share window/memo history exactly.
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)

gradModel <- function(ncmt, oral0, trans, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, %d, p1, v1, p2, p3, p4, p5, ka",
                  ncmt, oral0, trans)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)", k,
                                              sprintf(args, -2L, k)), ""))
  suppressWarnings(rxode2::rxode2(paste(lines, collapse = "\n")))
}

P1 <- c(p1=2.1, v1=21, p2=0, p3=0, p4=0, p5=0, ka=1.3)
P2 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0, p5=0, ka=1.3)
P3 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0.9, p5=61, ka=1.3)
Pk <- c(p1=0.1, v1=21, p2=0.15, p3=0.08, p4=0, p5=0, ka=1.3)

cases <- list(
  list(nm="1cmt iv t1",   ncmt=1, oral0=0, trans=1,  p=P1, dirs=0:1),
  list(nm="1cmt iv t2",   ncmt=1, oral0=0, trans=2,  p=Pk, dirs=0:1),
  list(nm="1cmt oral t1", ncmt=1, oral0=1, trans=1,  p=P1, dirs=0:2),
  list(nm="1cmt oral m",  ncmt=1, oral0=1, trans=1,  p=P1, dirs=c(0L,2L)),
  list(nm="2cmt iv t1",   ncmt=2, oral0=0, trans=1,  p=P2, dirs=0:3),
  list(nm="2cmt iv t4",   ncmt=2, oral0=0, trans=4,  p=P2, dirs=0:3),
  list(nm="2cmt oral t1", ncmt=2, oral0=1, trans=1,  p=P2, dirs=0:4),
  list(nm="2cmt oral t2", ncmt=2, oral0=1, trans=2,  p=Pk, dirs=0:4),
  list(nm="2cmt oral t11",ncmt=2, oral0=1, trans=11, p=P2, dirs=0:4),
  list(nm="2cmt oral m",  ncmt=2, oral0=1, trans=1,  p=P2, dirs=c(0L,1L,4L)),
  list(nm="3cmt iv t1",   ncmt=3, oral0=0, trans=1,  p=P3, dirs=0:5),
  list(nm="3cmt oral t1", ncmt=3, oral0=1, trans=1,  p=P3, dirs=0:6),
  list(nm="3cmt oral t10",ncmt=3, oral0=1, trans=10, p=P3, dirs=0:6),
  list(nm="3cmt oral m",  ncmt=3, oral0=1, trans=1,  p=P3, dirs=c(0L,1L,6L))
)

bolus <- function() rxode2::et(amt=100, time=0, cmt=1)
regimens <- list(
  uniform  = function() rxode2::et(bolus(), seq(0.25, 24, by=0.25)),
  nonunif  = function() rxode2::et(bolus(), cumsum(seq(0.05, 0.55, length.out=96))),
  multi    = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1, ii=12, addl=3),
                                   seq(0.5, 48, by=0.5)),
  infusion = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1, rate=20),
                                   seq(0.25, 24, by=0.25)),
  ss       = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1, ii=12, ss=1),
                                   seq(0.25, 24, by=0.25))
)

runArm <- function(mod, p, ev, phi) {
  rxode2:::linCmtSeqStats(TRUE)
  s <- rxode2::rxSolve(mod, p, ev, cores=1L, addDosing=FALSE,
                       linCmtSensType="AD", linCmtSensPhi=phi)
  list(s=s, st=rxode2:::linCmtSeqStats(TRUE))
}

res <- list()
for (cs in cases) {
  mod <- gradModel(cs$ncmt, cs$oral0, cs$trans, cs$dirs)
  cols <- c("cp", paste0("d", cs$dirs))
  for (rn in names(regimens)) {
    ev <- regimens[[rn]]()
    off <- runArm(mod, cs$p, ev, FALSE)
    on  <- runArm(mod, cs$p, ev, TRUE)
    a <- as.matrix(as.data.frame(off$s)[, cols, drop=FALSE])
    b <- as.matrix(as.data.frame(on$s)[, cols, drop=FALSE])
    den <- pmax(abs(a), 1e-300)
    rel <- max(abs(b - a)/den, na.rm=TRUE)
    res[[length(res)+1L]] <- data.frame(
      case=cs$nm, regimen=rn, ncmt=cs$ncmt, oral0=cs$oral0, trans=cs$trans,
      nDir=length(cs$dirs), maxRel=rel, bitwise=identical(a, b),
      phiBuildOff=unname(off$st[["phiBuild"]]),
      phiBuildOn=unname(on$st[["phiBuild"]]),
      phiRowsOn=unname(on$st[["phiRows"]]),
      tailRowsOn=unname(on$st[["seqTailRows"]]),
      stringsAsFactors=FALSE)
  }
}
res <- do.call(rbind, res)
saveRDS(res, "bench/results/phi_engage_valid.rds")
print(res[order(-res$maxRel), ], row.names=FALSE)
cat("\nworst relative difference:", format(max(res$maxRel), digits=3), "\n")
cat("phiBuild with the control off (must be 0):", sum(res$phiBuildOff), "\n")
cat("nonuniform phiBuild with the control on:",
    sum(res$phiBuildOn[res$regimen == "nonunif"]), "\n")
# An infusion regimen has both kinds of row: while the rate is on the row
# is affine in the prior state and declines, and once it stops the rows are
# ordinary and engage like any other.  So a non-zero count here is expected
# -- correctness for the regimen is covered by maxRel above.
cat("infusion phiRows (rows after the rate stops; non-zero is expected):",
    sum(res$phiRowsOn[res$regimen == "infusion"]), "\n")
