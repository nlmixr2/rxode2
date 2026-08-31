# A/B for rxode2#1298: the same optimized binary and the SAME generated
# sensitivity model, once as `rxSensMatExp()` emits it now and once with the
# two algebraically-zero `indLin()` lines -- and the un-cancelled
# `k_central_output` -- put back the way it emitted them before.  The fix is
# entirely in the generated model text, so one build measures both arms and the
# comparison isolates the spurious forcing exactly.
#
# The zero forcing is what classified the model as state dependent, which put
# the solve on the fixed-point iteration (`doIndLin` 4) instead of one cached
# exponential per interval (`doIndLin` 1).  `.rxMemDoIndLin()` is recorded per
# arm, and `cp` must agree between them to rounding: the dropped terms are
# zero, so a real difference means the arms are not the same model.
#
# Must be run against an INSTALLED (-O3) build in its own library -- load_all
# compiles at -O0 and its solver timings are meaningless.  Run on an idle
# machine.
#
# Usage:
#   R CMD INSTALL --library=/tmp/lib .
#   BENCH_LIB=/tmp/lib Rscript bench/indlin_zero_forcing_ab.R
message("== indlin_zero_forcing_ab ==")
.lib <- Sys.getenv("BENCH_LIB")
stopifnot(nzchar(.lib))
.libPaths(c(.lib, .libPaths()))
suppressMessages(library(rxode2))
# Guard against measuring a DIFFERENT rxode2 than the one just built.
stopifnot(identical(normalizePath(dirname(system.file(package = "rxode2"))),
                    normalizePath(.lib)))

.reps <- as.integer(Sys.getenv("BENCH_REPS", "7"))
cat("reps:", .reps, " load:",
    strsplit(readLines("/proc/loadavg"), " ")[[1]][1], "\n")

.mexp <- function(n) {
  .ln <- c("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v")
  if (n >= 2) .ln <- c(.ln, "k_central_periph <- q/v", "k_periph_central <- q/vp")
  if (n >= 3) .ln <- c(.ln, "k_central_periph2 <- q2/v", "k_periph2_central <- q2/vp2")
  paste(c(.ln, "cp <- central/v"), collapse = "\n")
}
.th <- c(ka = 1.1, cl = 4, v = 30, q = 8, vp = 40, q2 = 2, vp2 = 100)
# Log spaced on purpose: a uniform grid repeats one `dt`, and the
# content-addressed exponential cache then answers almost every interval.
.obs <- exp(seq(log(0.05), log(24), length.out = 200))
.ev <- as.data.frame(et(amt = 100, cmt = "depot", ii = 8, addl = 2) |>
                       et(.obs) |> et(id = 1:40))

.res <- NULL
for (.n in 2:3) {
  .new <- rxSensMatExp(model = .mexp(.n), calcSens = c("ka", "cl", "v"))
  .old <- sub("k_central_output = cl/v", "k_central_output = -q/v-(-q/v-cl/v)",
              .new, fixed = TRUE)
  .old <- paste0(
    .old,
    "\nindLin(central) <- -(-q/v-cl/v)*central-q*central/v-cl*central/v",
    "\nindLin(rx__sens_central_BY_v__) <- ",
    "-(q/Rx_pow_di(v,2)+cl/Rx_pow_di(v,2))*central",
    "+q*central/Rx_pow_di(v,2)+cl*central/Rx_pow_di(v,2)")
  .mn <- suppressMessages(rxode2(.new))
  .mo <- suppressMessages(rxode2(.old))
  .run <- function(m) {
    suppressMessages(rxSolve(m, .th, .ev, method = "indLin",
                             atol = 1e-10, rtol = 1e-10, cores = 1L))
  }
  .a <- as.data.frame(.run(.mn))
  .b <- as.data.frame(.run(.mo))
  .tn <- .to <- numeric(0)
  for (.i in seq_len(.reps)) {           # interleaved, so drift hits both arms
    .tn <- c(.tn, system.time(.run(.mn))[["elapsed"]])
    .to <- c(.to, system.time(.run(.mo))[["elapsed"]])
  }
  .res <- rbind(.res, data.frame(
    cmt = .n,
    fixed = min(.tn), spurious = min(.to), ratio = min(.to) / min(.tn),
    doIndLinFixed = rxode2:::.rxMemDoIndLin(rxModelVars(.mn)),
    doIndLinSpurious = rxode2:::.rxMemDoIndLin(rxModelVars(.mo)),
    maxCpDiff = max(abs(.a$cp - .b$cp))))
}
print(.res, digits = 4)
cat("load:", strsplit(readLines("/proc/loadavg"), " ")[[1]][1], "\n")
