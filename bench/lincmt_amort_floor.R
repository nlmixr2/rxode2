# Value-only (linCmtA) floor per tree: RXTREE env picks the build.
# Part of the quiet-machine A/B for the sensitivity-amortization project.
suppressMessages(devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                                    compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
cfg <- Sys.getenv("CONFIG", "2cmt")
nRep <- as.integer(Sys.getenv("REPS", "3"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
thn <- list(`1cmt` = c("ka","cl","v"), `2cmt` = c("ka","cl","v","q","vp"),
            `3cmt` = c("ka","cl","v","q","vp","q2","vp2"))[[cfg]]
tt <- list(`1cmt` = c(1.2,4,30), `2cmt` = c(1.2,4,30,8,60),
           `3cmt` = c(1.2,4,30,8,60,3,200))[[cfg]]
m <- rxode2::rxode2(paste0("param(", paste(thn, collapse=", "), ")\ncp = linCmt()"))
obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = 1000L)), 4)))
ev <- rxode2::et(amt = 100, time = 0, cmt = 1) |> rxode2::et(obsT)
pars <- setNames(as.data.frame(t(replicate(400L, tt))), thn)
tset <- numeric(nRep)
for (r in seq_len(nRep)) {
  t0 <- proc.time()[["elapsed"]]
  s <- rxode2::rxSolve(m, pars, ev, cores = 1L, addDosing = FALSE)
  tset[r] <- proc.time()[["elapsed"]] - t0
  stopifnot(nrow(s) > 0)
}
cat(sprintf("FLOOR %s 400x1000: %.4f s = %.3f us/obs (load %.2f)\n",
            cfg, median(tset), median(tset)/(400*1000)*1e6, loadAvg()))
