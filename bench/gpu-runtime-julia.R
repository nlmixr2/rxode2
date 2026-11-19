# Time rxode2's batched rounds (OpenCL kernel and the OpenMP CPU backend)
# against DiffEqGPU's EnsembleGPUKernel (GPUTsit5, GPUVern7, GPURodas5P) and
# OrdinaryDiffEq's EnsembleThreads on the same ensembles.
#
# Each ensemble integrates every trajectory over one interval and keeps only
# the end state (rxode2: one round; Julia: save_everystep = false).  Kernel
# builds and JIT compilation are excluded (each side is warmed up first);
# times are medians of 5.
#
# Usage: Rscript bench/gpu-runtime-julia.R [julia] [sizes] [csv]
#   julia  path of the julia binary (default: julia on the PATH), run with as
#          many threads as rxode2's CPU backend uses
#   sizes  comma-separated ensemble sizes (default 1000,10000,100000)
#   csv    results of an earlier run of bench/gpu-runtime-julia.jl (skips
#          running julia)
library(rxode2)
.rxGpuRun <- utils::getFromNamespace(".rxGpuRun", "rxode2")
.rxGpuRequests <- utils::getFromNamespace(".rxGpuRequests", "rxode2")
.rxGpuRound <- utils::getFromNamespace(".rxGpuRound", "rxode2")

.args <- commandArgs(TRUE)
.julia <- if (length(.args) >= 1) .args[1] else Sys.which("julia")
.sizes <- if (length(.args) >= 2) .args[2] else "1000,10000,100000"
## threads: OMP_NUM_THREADS when set (eg the cores a batch job was given)
.cores <- as.integer(Sys.getenv("OMP_NUM_THREADS", parallel::detectCores()))
## all of them (rxode2 uses half the cores by default)
setRxThreads(.cores)
.tol <- 1e-8

## julia first: started after R has opened OpenCL contexts it can crash
.csv <- if (length(.args) >= 3) .args[3] else tempfile(fileext = ".csv")
if (length(.args) < 3) {
  .st <- system2(
    .julia,
    c("-t", .cores, file.path("bench", "gpu-runtime-julia.jl"), .csv, .sizes),
    stdout = "",
    stderr = ""
  )
  if (.st != 0) stop("julia failed")
}
.jl <- utils::read.csv(.csv)

.info <- rxGpuInfo()
.dev <- which(.info$devices$fp64 & !grepl("rusticl", .info$devices$platform))[1]
if (is.na(.dev)) {
  stop("no OpenCL device with double precision")
}
cat("OpenCL device:", .info$devices$device[.dev], "\n")

## problems as in bench/gpu-runtime-julia.jl; parameters spread evenly
.probs <- list(
  lorenz = list(
    model = rxode2({
      d/dt(x) <- s * (y - x)
      d/dt(y) <- x * (r - z) - y
      d/dt(z) <- x * y - b * z
    }),
    par = function(g) cbind(s = 10, r = 10 + 20 * g, b = 8 / 3),
    t1 = 1
  ),
  robertson = list(
    model = rxode2({
      d/dt(y1) <- -k1 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- k1 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    }),
    par = function(g) cbind(k1 = 0.02 + 0.06 * g),
    t1 = 1e3
  )
)
.cases <- list(c("lorenz", "rkt54"), c("lorenz", "vern76"), c("lorenz", "ros54p"), c("robertson", "ros54p"))

.time <- function(f, reps = 5) {
  f()
  stats::median(vapply(seq_len(reps), function(i) system.time(f())[["elapsed"]], double(1)))
}

.rx <- NULL
for (.n in as.integer(strsplit(.sizes, ",")[[1]])) {
  .g <- if (.n == 1) 0 else (seq_len(.n) - 1) / (.n - 1)
  for (.c in .cases) {
    .p <- .probs[[.c[1]]]
    .pm <- .p$par(.g)[, rxModelVars(.p$model)$params, drop = FALSE]
    .y0 <- matrix(c(1, 0, 0), .n, 3, byrow = TRUE)
    for (.be in c("opencl", "cpu")) {
      .run <- .rxGpuRun(.p$model, .c[2], .n, backend = .be, device = .dev, cores = .cores, atol = .tol, rtol = .tol)
      .q <- .rxGpuRequests(.run, seq_len(.n), 0, .p$t1, .y0, .pm, init = TRUE)
      .r <- NULL
      .s <- .time(function() .r <<- .rxGpuRound(.run, .q))
      stopifnot(all(.r$istate == 2L))
      .rx <- rbind(
        .rx,
        data.frame(
          problem = .c[1],
          method = .c[2],
          backend = if (.be == "opencl") "OpenCL-pocl" else paste0("OpenMP-", .cores),
          n = .n,
          seconds = .s,
          u1 = .r$y[.n, 1],
          u2 = .r$y[.n, 2],
          u3 = .r$y[.n, 3]
        )
      )
      cat(.c, .be, .n, .s, "\n")
    }
  }
}

.jl$backend <- ifelse(.jl$backend == "EnsembleThreads", paste0("EnsembleThreads-", .cores), .jl$backend)

## one row per problem, method and size; the end state of the last trajectory
## gives the largest relative difference from the rxode2 OpenCL result
## a Julia backend that did not load (eg no OpenCL.jl) leaves no row: NA
.one <- function(x) if (length(x) > 0L) x[1] else NA_real_
.ms <- function(x) sprintf("%.2f", 1000 * .one(x))
.u <- function(r) if (nrow(r) > 0L) unlist(r[1, c("u1", "u2", "u3")]) else rep(NA_real_, 3)
.out <- NULL
for (.i in seq_len(nrow(.rx))) {
  if (.rx$backend[.i] != "OpenCL-pocl") {
    next
  }
  .k <- .rx$problem == .rx$problem[.i] & .rx$method == .rx$method[.i] & .rx$n == .rx$n[.i]
  .j <- .jl[.jl$problem == .rx$problem[.i] & .jl$method == .rx$method[.i] & .jl$n == .rx$n[.i], ]
  .ref <- unlist(.rx[.i, c("u1", "u2", "u3")])
  .rd <- function(u) max(abs(u - .ref) / pmax(abs(.ref), 1e-6 * max(abs(.ref))))
  .jg <- .j[.j$backend == "OpenCL-pocl", ]
  .jt <- .j[grepl("EnsembleThreads", .j$backend), ]
  .out <- rbind(
    .out,
    data.frame(
      problem = .rx$problem[.i],
      method = paste0(.rx$method[.i], " / ", .one(.j$julia[.j$backend %in% c("OpenCL-pocl", "KA-CPU")])),
      n = .rx$n[.i],
      rx_opencl_ms = .ms(.rx$seconds[.i]),
      jl_opencl_ms = .ms(.jg$seconds),
      jl_cuda_ms = .ms(.j$seconds[.j$backend == "CUDA"]),
      jl_kacpu_ms = .ms(.j$seconds[.j$backend == "KA-CPU"]),
      rx_openmp_ms = .ms(.rx$seconds[.k & .rx$backend != "OpenCL-pocl"]),
      jl_threads_ms = .ms(.jt$seconds),
      reldiff_opencl = sprintf("%.1e", .rd(.u(.jg))),
      reldiff_cuda = sprintf("%.1e", .rd(.u(.j[.j$backend == "CUDA", ]))),
      reldiff_threads = sprintf("%.1e", .rd(.u(.jt)))
    )
  )
}
options(width = 200)
print(.out, row.names = FALSE)
