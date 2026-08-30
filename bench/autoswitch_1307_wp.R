## Work-precision sweep for nlmixr2/rxode2#1307: achieved accuracy of each
## method at each tolerance, so a method is not credited for speed it bought by
## solving loosely.  Companion to bench/autoswitch_1307.R, which times them.
## Read the -O0 note at the top of that script before comparing builds.
if (nzchar(Sys.getenv("BENCH_LOADALL"))) {
  options(pkg.build_extra_flags = FALSE)
  suppressMessages(pkgload::load_all(Sys.getenv("BENCH_LOADALL"), quiet = TRUE))
} else {
  suppressMessages(library(rxode2))
}

source("bench/autoswitch_1307_models.R")

rxode2::setRxThreads(as.integer(Sys.getenv("BENCH_THREADS", "1")))

TOLS <- list(default = list(),
             `1e-6`  = list(atol = 1e-6,  rtol = 1e-6),
             `1e-8`  = list(atol = 1e-8,  rtol = 1e-8),
             `1e-10` = list(atol = 1e-10, rtol = 1e-10))
METHODS <- c("liblsoda", "dop853", "ros4", "dop853+ros4")

res <- NULL
for (nm in names(mods)) {
  mod <- mods[[nm]]
  ref <- .solve(mod, list(method = "liblsoda", atol = 1e-13, rtol = 1e-13), 1L)[[mod$out]]
  sc <- pmax(abs(ref), 1e-8 * max(abs(ref)))
  for (tn in names(TOLS)) for (mn in METHODS) {
    a <- .solve(mod, c(list(method = mn), TOLS[[tn]]), 1L)[[mod$out]]
    res <- rbind(res, data.frame(model = nm, tol = tn, method = mn,
                                 relerr = max(abs(a - ref) / sc)))
  }
}
print(res, row.names = FALSE)
saveRDS(res, Sys.getenv("BENCH_OUT", "bench/autoswitch_1307_wp.rds"))
