## Benchmark for nlmixr2/rxode2#1762 -- rxControl(ssMethod = "anderson")
## against the default plain fixed-point iteration ("iterate").
##
## For each steady-state scenario (NSUB subjects, 5% CV on each parameter but
## the Michaelis-Menten vm, so the dose stays at the stated fraction of the
## elimination capacity) it reports the time per rxSolve() on one thread, the
## largest relative error of the profile against a reference (dop853 at 1e-10,
## ssAtol = ssRtol = 1e-12, maxSS = 1e7) and the number of NA values (failed
## solves).  For dop853 at 1e-10 the error is the steady-state error alone; for
## liblsoda at the default tolerances it also holds the integration error.  A
## scenario with no steady state (dosing above the elimination capacity)
## instead reports whether both methods give the identical result.
##
## Run from the package root against an optimized build:
##   R_LIBS=<lib with this branch installed> Rscript bench/ss_anderson_1762.R
## or BENCH_LOADALL=. Rscript bench/ss_anderson_1762.R (see
## bench/autoswitch_1307.R for why load_all needs pkg.build_extra_flags).
## Infusion scenarios are accelerated only from part 2 of #1762 on.
if (nzchar(Sys.getenv("BENCH_LOADALL"))) {
  options(pkg.build_extra_flags = FALSE)
  suppressMessages(pkgload::load_all(Sys.getenv("BENCH_LOADALL"), quiet = TRUE))
} else {
  suppressMessages(library(rxode2))
}

NSUB <- as.integer(Sys.getenv("BENCH_NSUB", "50"))
REFCORES <- as.integer(Sys.getenv("BENCH_REFCORES", "8"))

.time <- function(f, target = 0.5, maxReps = 50L) {
  f()
  k <- 1L
  repeat {
    t <- system.time(
      for (i in seq_len(k)) {
        f()
      }
    )[["elapsed"]]
    if (t >= target || k >= maxReps) {
      return(t / k)
    }
    k <- min(maxReps, max(k * 2L, as.integer(ceiling(k * target / max(t, 1e-4)))))
  }
}

.oral1 <- function() {
  ini({
    ka <- 1
    k <- 0.1
    v <- 20
  })
  model({
    d/dt(depot) <- -ka * depot
    d/dt(centr) <- ka * depot - k * centr
    cp <- centr / v
  })
}

.inf1 <- function() {
  ini({
    k <- 0.1
    v <- 20
  })
  model({
    d/dt(centr) <- -k * centr
    cp <- centr / v
  })
}

.oral2 <- function() {
  ini({
    ka <- 1
    cl <- 5
    v <- 20
    q <- 0.5
    vp <- 400
  })
  model({
    d/dt(depot) <- -ka * depot
    d/dt(centr) <- ka * depot - cl / v * centr - q / v * centr + q / vp * periph
    d/dt(periph) <- q / v * centr - q / vp * periph
    cp <- centr / v
  })
}

.idr <- function() {
  ini({
    ka <- 1
    k <- 0.5
    v <- 20
    kin <- 1
    kout <- 0.0116
    ic50 <- 2
  })
  model({
    resp(0) <- kin / kout
    d/dt(depot) <- -ka * depot
    d/dt(centr) <- ka * depot - k * centr
    cp <- centr / v
    d/dt(resp) <- kin * (1 - cp / (ic50 + cp)) - kout * resp
  })
}

.mm <- function() {
  ini({
    ka <- 1
    vm <- 10
    km <- 5
    v <- 50
  })
  model({
    d/dt(depot) <- -ka * depot
    d/dt(centr) <- ka * depot - vm * (centr / v) / (km + centr / v)
    cp <- centr / v
  })
}

.mmInf <- function() {
  ini({
    vm <- 10
    km <- 5
    v <- 50
  })
  model({
    d/dt(centr) <- -vm * (centr / v) / (km + centr / v)
    cp <- centr / v
  })
}

.ii <- 24
.th <- function(n) log(2) / (n * .ii) # elimination rate for t1/2 = n * ii
.scen <- list(
  list(name = "1-cmt oral, t1/2 = ii/4", model = .oral1, par = c(k = .th(0.25)), amt = 100),
  list(name = "1-cmt oral, t1/2 = 2 ii", model = .oral1, par = c(k = .th(2)), amt = 100),
  list(name = "1-cmt oral, t1/2 = 10 ii", model = .oral1, par = c(k = .th(10)), amt = 100),
  list(name = "2-cmt oral, slow peripheral", model = .oral2, par = c(q = 0.5, vp = 400), amt = 100),
  list(name = "indirect response, kout t1/2 = 2.5 ii", model = .idr, par = c(kout = .th(2.5)), amt = 100),
  list(name = "1-cmt infusion 2 h, t1/2 = 10 ii", model = .inf1, par = c(k = .th(10)), amt = 100, dur = 2),
  list(name = "MM oral, 50% capacity", model = .mm, par = c(vm = 10), amt = 120),
  list(name = "MM oral, 85% capacity", model = .mm, par = c(vm = 10), amt = 204),
  list(name = "MM oral, 95% capacity", model = .mm, par = c(vm = 10), amt = 228),
  list(name = "MM oral, 99% capacity", model = .mm, par = c(vm = 10), amt = 237.6),
  list(name = "MM infusion 2 h, 85% capacity", model = .mmInf, par = c(vm = 10), amt = 204, dur = 2),
  list(name = "MM infusion 30 h (> ii), 85% capacity", model = .mmInf, par = c(vm = 10), amt = 204, dur = 30),
  list(name = "MM oral, 125% capacity (no steady state)", model = .mm, par = c(vm = 10), amt = 300, noSS = TRUE)
)

.methods <- list(
  liblsoda = list(method = "liblsoda"),
  dop853 = list(method = "dop853", atol = 1e-10, rtol = 1e-10)
)

.relErr <- function(a, ref) {
  .ok <- !is.na(a)
  if (!any(.ok)) {
    return(NA_real_)
  }
  max(abs(a[.ok] - ref[.ok]) / pmax(abs(ref[.ok]), 1e-8 + 1e-6 * max(abs(ref))))
}

set.seed(1762)
.rows <- list()
for (.s in .scen) {
  .ui <- rxode2(.s$model)
  .ini <- .ui$theta
  .ini[names(.s$par)] <- .s$par
  .p <- as.data.frame(lapply(.ini, function(x) x * exp(rnorm(NSUB, 0, 0.05))))
  if (!is.null(.p$vm)) {
    .p$vm <- .ini[["vm"]]
  }
  .p$id <- seq_len(NSUB)
  .ev <- if (is.null(.s$dur)) {
    et(amt = .s$amt, ii = .ii, ss = 1, id = seq_len(NSUB))
  } else {
    et(amt = .s$amt, ii = .ii, ss = 1, dur = .s$dur, id = seq_len(NSUB))
  }
  .ev <- et(.ev, seq(0, .ii, by = 2), id = seq_len(NSUB))
  .out <- if (any(.ui$mv0$lhs == "resp") || "resp" %in% .ui$mv0$state) "resp" else "cp"
  .solve <- function(m, ...) {
    suppressWarnings(do.call(
      rxSolve,
      c(list(.ui, .ev, .p, returnType = "data.frame"), m, list(...))
    ))[[.out]]
  }
  .ref <- if (isTRUE(.s$noSS)) {
    NULL
  } else {
    .solve(.methods$dop853, cores = REFCORES, ssAtol = 1e-12, ssRtol = 1e-12, maxSS = 1e7)
  }
  for (.mn in names(.methods)) {
    .m <- .methods[[.mn]]
    .tIt <- .time(function() .solve(.m, cores = 1))
    .tAa <- .time(function() .solve(.m, cores = 1, ssMethod = "anderson"))
    .it <- .solve(.m, cores = 1)
    .aa <- .solve(.m, cores = 1, ssMethod = "anderson")
    if (is.null(.ref)) {
      .eIt <- NA_real_
      .eAa <- NA_real_
      .note <- if (identical(.it, .aa)) "identical to iterate" else "DIFFERS from iterate"
    } else {
      .eIt <- .relErr(.it, .ref)
      .eAa <- .relErr(.aa, .ref)
      .note <- ""
    }
    .rows[[length(.rows) + 1L]] <- data.frame(
      scenario = .s$name,
      method = .mn,
      msIterate = 1000 * .tIt,
      msAnderson = 1000 * .tAa,
      speedup = .tIt / .tAa,
      errIterate = .eIt,
      errAnderson = .eAa,
      naIterate = sum(is.na(.it)),
      naAnderson = sum(is.na(.aa)),
      note = .note
    )
    message(sprintf(
      "%-42s %-8s iterate %8.1f ms  anderson %8.1f ms  x%5.2f  err %.1e / %.1e  NA %d / %d %s",
      .s$name,
      .mn,
      1000 * .tIt,
      1000 * .tAa,
      .tIt / .tAa,
      .eIt,
      .eAa,
      sum(is.na(.it)),
      sum(is.na(.aa)),
      .note
    ))
  }
}
.res <- do.call(rbind, .rows)
print(.res, digits = 3, row.names = FALSE)
if (nzchar(Sys.getenv("BENCH_OUT"))) {
  saveRDS(.res, Sys.getenv("BENCH_OUT"))
}
