# Time nlmixr2 estimation (SAEM and FOCEi) of a simulated population PK
# model with the GPU methods (OpenCL and their CPU build) against host ODE
# methods.  Every fit uses the same data, model and starting estimates; only
# rxControl(method=, gpuBackend=) changes.  Covariance and tables are off, so
# the time is the estimation (and the SAEM -2LL by quadrature).
#
# Usage: Rscript bench/gpu-nlmixr2.R [sizes] [est]
#   sizes  comma-separated numbers of subjects (default 500)
#   est    comma-separated estimation methods (default saem,focei)
library(rxode2)
library(nlmixr2)

.args <- commandArgs(TRUE)
.sizes <- as.integer(strsplit(if (length(.args) >= 1) .args[1] else "500", ",")[[1]])
.ests <- strsplit(if (length(.args) >= 2) .args[2] else "saem,focei", ",")[[1]]
.cores <- parallel::detectCores()

## one-compartment oral PK with an Emax effect on elimination (ODE, so every
## method integrates it)
.model <- function() {
  ini({
    tka <- log(1.2)
    tcl <- log(3)
    tv <- log(30)
    temax <- log(0.5)
    eta.ka ~ 0.2
    eta.cl ~ 0.1
    eta.v ~ 0.1
    prop.sd <- 0.1
    add.sd <- 0.05
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax <- exp(temax)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * (1 + emax * center / (center + 50)) * center
    cp <- center / v
    cp ~ add(add.sd) + prop(prop.sd)
  })
}

## an OpenCL device with double precision (not Mesa's rusticl)
.dev <- NA_integer_
if (requireNamespace("OpenCL", quietly = TRUE)) {
  .d <- rxGpuInfo()$devices
  .dev <- which(.d$fp64 & !grepl("rusticl", .d$platform))[1]
}

## ODE method and gpuBackend; liblsoda and the AutoSwitch composites switch
## between non-stiff and stiff steps
.cases <- list(
  c("liblsoda", "auto"),
  c("t54", "auto"),
  c("ros54p", "auto"),
  c("t54+ros54p", "auto"),
  c("gpuT54", "cpu"),
  c("gpuRos54p", "cpu"),
  c("gpuT54+gpuRos54p", "cpu")
)
if (!is.na(.dev)) {
  .cases <- c(.cases, list(c("gpuT54", "opencl"), c("gpuRos54p", "opencl"), c("gpuT54+gpuRos54p", "opencl")))
}

.out <- NULL
for (.n in .sizes) {
  set.seed(42)
  .ev <- et(amt = 300, ii = 12, addl = 2, cmt = "depot") |>
    et(c(0.5, 1, 2, 4, 6, 12, 24, 30, 36)) |>
    et(id = seq_len(.n))
  .sim <- rxSolve(.model, .ev, addDosing = TRUE, returnType = "data.frame")
  .dat <- .sim[, c("id", "time", "evid", "amt", "cmt", "sim")]
  names(.dat)[names(.dat) == "sim"] <- "dv"
  .dat$dv[.dat$evid != 0] <- 0
  for (.est in .ests) {
    for (.c in .cases) {
      .ctl <- if (.est == "saem") {
        saemControl(
          print = 0, calcTables = FALSE, covMethod = "",
          rxControl = rxControl(method = .c[1], gpuBackend = .c[2], gpuDevice = if (is.na(.dev)) NULL else .dev, cores = .cores)
        )
      } else {
        foceiControl(
          print = 0, calcTables = FALSE, covMethod = "",
          rxControl = rxControl(method = .c[1], gpuBackend = .c[2], gpuDevice = if (is.na(.dev)) NULL else .dev, cores = .cores)
        )
      }
      .t <- system.time(.fit <- suppressMessages(suppressWarnings(nlmixr2(.model, .dat, .est, control = .ctl))))[["elapsed"]]
      .out <- rbind(
        .out,
        data.frame(
          n = .n,
          est = .est,
          method = .c[1],
          backend = .c[2],
          seconds = round(.t, 1),
          objf = round(.fit$objf, 2),
          tcl = signif(exp(.fit$theta[["tcl"]]), 4),
          tv = signif(exp(.fit$theta[["tv"]]), 4)
        )
      )
      cat(.n, .est, .c, round(.t, 1), "\n")
    }
  }
}
options(width = 200)
print(.out, row.names = FALSE)
