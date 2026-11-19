# Time population simulations with the GPU methods (OpenCL and their CPU
# build) against host methods, LSODA and the AutoSwitch composites included.
# Unlike bench/gpu-solve-julia.R (one integration per subject), these have
# dosing, covariates and dense sampling, so each subject needs one GPU round
# per record.
#
#   pk2   two-compartment PK, oral doses then IV infusions, weight and a
#         time-varying creatinine clearance (non-stiff)
#   tmdd  target-mediated drug disposition with fast binding (stiff)
#
# Usage: Rscript bench/gpu-poppk.R [sizes] [reps]
#   sizes  comma-separated numbers of subjects (default 1000,10000,50000)
#   reps   timed repetitions after a warm-up (default 3)
library(rxode2)

.args <- commandArgs(TRUE)
.sizes <- as.integer(strsplit(if (length(.args) >= 1) .args[1] else "1000,10000,50000", ",")[[1]])
.reps <- if (length(.args) >= 2) as.integer(.args[2]) else 3L
## threads: OMP_NUM_THREADS when set (eg the cores a batch job was given)
.cores <- as.integer(Sys.getenv("OMP_NUM_THREADS", parallel::detectCores()))
## all of them (rxode2 uses half the cores by default)
setRxThreads(.cores)
.tol <- c(atol = 1e-8, rtol = 1e-6)

.pk2 <- function() {
  ini({
    tka <- log(1.1)
    tcl <- log(4)
    tv <- log(30)
    tq <- log(2)
    tvp <- log(60)
    eta.ka ~ 0.2
    eta.cl ~ 0.1
    eta.v ~ 0.1
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl) * (wt / 70)^0.75 * (crcl / 100)^0.5
    v <- exp(tv + eta.v) * wt / 70
    q <- exp(tq)
    vp <- exp(tvp)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center - q / v * center + q / vp * periph
    d/dt(periph) <- q / v * center - q / vp * periph
    cp <- center / v
  })
}

.tmdd <- function() {
  ini({
    tcl <- log(0.2)
    tv <- log(3)
    tkint <- log(0.04)
    tkdeg <- log(0.1)
    tr0 <- log(10)
    eta.cl ~ 0.1
    eta.v ~ 0.1
    eta.r0 ~ 0.1
  })
  model({
    cl <- exp(tcl + eta.cl) * (wt / 70)^0.75
    v <- exp(tv + eta.v)
    kint <- exp(tkint)
    kdeg <- exp(tkdeg)
    r0 <- exp(tr0 + eta.r0)
    kon <- 50
    koff <- 1
    ksyn <- kdeg * r0
    target(0) <- r0
    d/dt(drug) <- -cl / v * drug - kon * drug * target + koff * cplx * v
    d/dt(target) <- ksyn - kdeg * target - kon * drug / v * target + koff * cplx
    d/dt(cplx) <- kon * drug / v * target - (koff + kint) * cplx
    cp <- drug / v
  })
}

## events: pk2 has 3 oral doses then 4 one-hour infusions (q12h) and hourly
## samples for a week; tmdd has weekly 2-hour infusions and samples every
## 6 hours for 4 weeks
.events <- function(model, n) {
  set.seed(42)
  if (model == "pk2") {
    .ev <- et(amt = 300, ii = 12, addl = 2, cmt = "depot") |>
      et(time = 36, amt = 200, rate = 200, ii = 12, addl = 3, cmt = "center") |>
      et(seq(0, 168, by = 1)) |>
      et(id = seq_len(n)) |>
      as.data.frame()
    .wt <- stats::runif(n, 50, 110)
    .ev$wt <- .wt[.ev$id]
    ## creatinine clearance falling over the week
    .ev$crcl <- stats::runif(n, 70, 130)[.ev$id] * (1 - 0.002 * .ev$time)
  } else {
    .ev <- et(amt = 100, rate = 50, ii = 168, addl = 3, cmt = "drug") |>
      et(seq(0, 672, by = 6)) |>
      et(id = seq_len(n)) |>
      as.data.frame()
    .ev$wt <- stats::runif(n, 50, 110)[.ev$id]
  }
  .ev
}

## each subject's etas, drawn once so every method solves the same subjects
.etas <- function(model, n) {
  set.seed(7)
  .nm <- if (model == "pk2") c("eta.ka", "eta.cl", "eta.v") else c("eta.cl", "eta.v", "eta.r0")
  .om <- if (model == "pk2") c(0.2, 0.1, 0.1) else c(0.1, 0.1, 0.1)
  .p <- data.frame(id = seq_len(n))
  for (.i in seq_along(.nm)) {
    .p[[.nm[.i]]] <- stats::rnorm(n, 0, sqrt(.om[.i]))
  }
  .p
}

## an OpenCL device with double precision (not Mesa's rusticl)
.dev <- NA_integer_
if (requireNamespace("OpenCL", quietly = TRUE) && rxGpuInfo()$available) {
  .d <- rxGpuInfo()$devices
  .dev <- which(.d$fp64 & !grepl("rusticl", .d$platform))[1]
}

## method, backend ("" for host methods)
.cases <- list(
  c("liblsoda", ""),
  c("dop853", ""),
  c("t54", ""),
  c("ros54p", ""),
  c("t54+ros54p", ""),
  c("vern76+ros54p", ""),
  c("gpuT54", "cpu"),
  c("gpuRos54p", "cpu"),
  c("gpuT54+gpuRos54p", "cpu"),
  c("gpuVern76+gpuRos54p", "cpu")
)
if (!is.na(.dev)) {
  .cases <- c(
    .cases,
    list(
      c("gpuT54", "opencl"),
      c("gpuRos54p", "opencl"),
      c("gpuT54+gpuRos54p", "opencl"),
      c("gpuVern76+gpuRos54p", "opencl")
    )
  )
}

.solve <- function(model, par, ev, case) {
  .a <- list(
    model,
    par,
    ev,
    method = case[1],
    atol = .tol[["atol"]],
    rtol = .tol[["rtol"]],
    cores = .cores,
    returnType = "data.frame",
    covsInterpolation = "linear"
  )
  if (case[2] != "") {
    .a$gpuBackend <- case[2]
    if (case[2] == "opencl") .a$gpuDevice <- .dev
  }
  suppressMessages(do.call(rxSolve, .a))
}

.out <- NULL
for (.mn in c("pk2", "tmdd")) {
  .model <- if (.mn == "pk2") .pk2 else .tmdd
  for (.n in .sizes) {
    .ev <- .events(.mn, .n)
    .par <- .etas(.mn, .n)
    .ref <- NULL
    for (.c in .cases) {
      ## explicit methods crawl on the stiff TMDD model: time them at the
      ## smallest size only
      if (.mn == "tmdd" && .n > min(.sizes) && .c[1] %in% c("dop853", "t54", "gpuT54")) {
        next
      }
      .r <- .solve(.model, .par, .ev, .c) # warm-up (compiles, builds kernels)
      .t <- stats::median(vapply(
        seq_len(.reps),
        function(i) {
          system.time(.r <<- .solve(.model, .par, .ev, .c))[["elapsed"]]
        },
        double(1)
      ))
      if (is.null(.ref)) {
        .ref <- .r$cp
      }
      .out <- rbind(
        .out,
        data.frame(
          model = .mn,
          n = .n,
          records = nrow(.ev) / .n,
          method = .c[1],
          backend = if (.c[2] == "") "host" else .c[2],
          ms = round(1000 * .t),
          reldiff = signif(max(abs(.r$cp - .ref) / pmax(abs(.ref), 1e-3 * max(abs(.ref)))), 2)
        )
      )
      cat(.mn, .n, .c, round(1000 * .t), "\n")
    }
  }
}
options(width = 200)
print(.out, row.names = FALSE)
