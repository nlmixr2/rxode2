# What the engage rule buys, measured by alternating the control on ONE
# binary (so no build difference can enter) with rounds interleaved so a
# drifting machine load cancels between the arms rather than landing on
# one of them.
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
ROUNDS <- as.integer(Sys.getenv("ROUNDS", "5"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg")[1], " ")[[1]][1])

gradModel <- function(ncmt, oral0, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, p1, v1, p2, p3, p4, p5, ka",
                  ncmt, oral0)
  suppressWarnings(rxode2::rxode2(paste(c(
    sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
    vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k)), "")),
    collapse = "\n")))
}
P2 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0,   p5=0,  ka=1.3)
P3 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0.9, p5=61, ka=1.3)
NOBS <- 1000L; NSUB <- 20L
rep40 <- function(ev) do.call(rbind, lapply(seq_len(NSUB), function(i) {
  d <- as.data.frame(ev); d$id <- i; d }))

cells <- list(
  list(nm="2cmt uniform",  m=gradModel(2L,1L,0:4), p=P2,
       ev=rep40(rxode2::et(rxode2::et(amt=100,time=0,cmt=1), seq(0.05,0.05*NOBS,by=0.05)))),
  list(nm="3cmt uniform",  m=gradModel(3L,1L,0:6), p=P3,
       ev=rep40(rxode2::et(rxode2::et(amt=100,time=0,cmt=1), seq(0.05,0.05*NOBS,by=0.05)))),
  list(nm="2cmt multi",    m=gradModel(2L,1L,0:4), p=P2,
       ev=rep40(rxode2::et(rxode2::et(amt=100,time=0,cmt=1,ii=12,addl=9), seq(0.12,0.12*NOBS,by=0.12)))),
  list(nm="3cmt multi",    m=gradModel(3L,1L,0:6), p=P3,
       ev=rep40(rxode2::et(rxode2::et(amt=100,time=0,cmt=1,ii=12,addl=9), seq(0.12,0.12*NOBS,by=0.12)))),
  list(nm="3cmt infusion", m=gradModel(3L,1L,0:6), p=P3,
       ev=rep40(rxode2::et(rxode2::et(amt=100,time=0,cmt=1,rate=2), seq(0.05,0.05*NOBS,by=0.05))))
)

one <- function(cl, phi) {
  t0 <- proc.time()[["elapsed"]]
  invisible(rxode2::rxSolve(cl$m, cl$p, cl$ev, cores=1L, addDosing=FALSE,
                            linCmtSensType="AD", linCmtSensPhi=phi))
  proc.time()[["elapsed"]] - t0
}
res <- list()
for (cl in cells) {
  invisible(one(cl, FALSE)); invisible(one(cl, TRUE))   # warm both
  rxode2:::linCmtSeqStats(TRUE); invisible(one(cl, TRUE))
  st <- rxode2:::linCmtSeqStats(TRUE)
  offT <- numeric(ROUNDS); onT <- numeric(ROUNDS); ld <- numeric(ROUNDS)
  for (r in seq_len(ROUNDS)) {            # alternate within each round
    ld[r] <- loadAvg()
    offT[r] <- one(cl, FALSE)
    onT[r]  <- one(cl, TRUE)
  }
  n <- NSUB*NOBS
  res[[length(res)+1L]] <- data.frame(
    cell=cl$nm, offUs=1e6*median(offT)/n, onUs=1e6*median(onT)/n,
    gain=round(median(offT)/median(onT), 3),
    phiRows=unname(st[["phiRows"]]), phiBuild=unname(st[["phiBuild"]]),
    tailRows=unname(st[["seqTailRows"]]), load=median(ld), stringsAsFactors=FALSE)
}
res <- do.call(rbind, res)
saveRDS(res, "bench/results/phi_engage_interleave.rds")
print(res, row.names=FALSE)
