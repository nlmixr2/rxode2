# Time whole rxSolve() calls with the GPU methods (OpenCL and their CPU
# build) and their host counterparts against DiffEqGPU's EnsembleGPUKernel
# and OrdinaryDiffEq's EnsembleThreads on the ensembles of
# bench/gpu-runtime-julia.jl.  Unlike bench/gpu-runtime-julia.R (one bare
# round), these times include rxSolve()'s setup, the per-subject event
# handling and the output data frame.
#
# Usage: Rscript bench/gpu-solve-julia.R [julia] [sizes] [csv]
#   julia  path of the julia binary (default: julia on the PATH)
#   sizes  comma-separated ensemble sizes (default 1000,10000,100000)
#   csv    results of an earlier run of bench/gpu-runtime-julia.jl (skips
#          running julia)
library(rxode2)

.args <- commandArgs(TRUE)
.julia <- if (length(.args) >= 1) .args[1] else Sys.which("julia")
.sizes <- if (length(.args) >= 2) .args[2] else "1000,10000,100000"
.cores <- parallel::detectCores()
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

.probs <- list(
  lorenz = list(
    model = rxode2({
      d/dt(x) <- s * (y - x)
      d/dt(y) <- x * (r - z) - y
      d/dt(z) <- x * y - b * z
    }),
    par = function(g) data.frame(s = 10, r = 10 + 20 * g, b = 8 / 3),
    t1 = 1
  ),
  robertson = list(
    model = rxode2({
      d/dt(y1) <- -k1 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- k1 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    }),
    par = function(g) data.frame(k1 = 0.02 + 0.06 * g),
    t1 = 1e3
  )
)
## rxode2 step method (as in the Julia CSV), GPU method, host method
.cases <- list(
  c("lorenz", "rkt54", "gpuRkt54", "t54"),
  c("lorenz", "vern76", "gpuVern76", "vern76"),
  c("lorenz", "ros54p", "gpuRos54p", "ros54p"),
  c("robertson", "ros54p", "gpuRos54p", "ros54p")
)

.time <- function(f, reps = 3) {
  f()
  stats::median(vapply(seq_len(reps), function(i) system.time(f())[["elapsed"]], double(1)))
}

.ms <- function(x) if (length(x) == 0L || is.na(x[1])) "NA" else sprintf("%.0f", 1000 * x[1])
.out <- NULL
for (.n in as.integer(strsplit(.sizes, ",")[[1]])) {
  .g <- if (.n == 1) 0 else (seq_len(.n) - 1) / (.n - 1)
  for (.c in .cases) {
    .p <- .probs[[.c[1]]]
    .par <- .p$par(.g)
    .ev <- et(.p$t1)
    .ini <- if (.c[1] == "lorenz") c(x = 1, y = 0, z = 0) else c(y1 = 1, y2 = 0, y3 = 0)
    .run <- function(method, backend = "auto") {
      .r <- NULL
      .s <- .time(function() {
        .r <<- rxSolve(
          .p$model,
          .par,
          .ev,
          inits = .ini,
          method = method,
          atol = .tol,
          rtol = .tol,
          cores = .cores,
          gpuBackend = backend,
          gpuDevice = .dev,
          returnType = "data.frame"
        )
      })
      list(s = .s, u = unlist(.r[nrow(.r), names(.ini)]))
    }
    .gcl <- .run(.c[3], "opencl")
    .gcpu <- .run(.c[3], "cpu")
    .host <- .run(.c[4])
    .j <- .jl[.jl$problem == .c[1] & .jl$method == .c[2] & .jl$n == .n, ]
    .rd <- function(u) {
      if (length(u) == 0L || anyNA(u)) {
        return("NA")
      }
      sprintf("%.1e", max(abs(u - .gcl$u) / pmax(abs(.gcl$u), 1e-6 * max(abs(.gcl$u)))))
    }
    .jt <- .j[.j$backend == "EnsembleThreads", ]
    .out <- rbind(
      .out,
      data.frame(
        problem = .c[1],
        method = paste0(.c[3], " | ", .c[4]),
        n = .n,
        rx_gpu_opencl = .ms(.gcl$s),
        jl_gpu_opencl = .ms(.j$seconds[.j$backend == "OpenCL-pocl"]),
        rx_gpu_cpu = .ms(.gcpu$s),
        jl_gpu_kacpu = .ms(.j$seconds[.j$backend == "KA-CPU"]),
        rx_host = .ms(.host$s),
        jl_threads = .ms(.jt$seconds),
        reldiff_host = .rd(.host$u),
        reldiff_jl = .rd(if (nrow(.jt) > 0L) unlist(.jt[1, c("u1", "u2", "u3")]) else NA)
      )
    )
    cat(.c, .n, "\n")
  }
}
options(width = 200)
print(.out, row.names = FALSE)
