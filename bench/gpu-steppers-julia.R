## Compare rxode2's device integrators (built as host C) with Julia's
## OrdinaryDiffEq on the same problems and tolerances: error against a tight
## reference, and steps / derivative evaluations.  Run from the package root:
##   julia bench/gpu-steppers-julia.jl /tmp/jl.csv
##   Rscript bench/gpu-steppers-julia.R /tmp/jl.csv
devtools::load_all(quiet = TRUE)
.rxGpuHostLib <- utils::getFromNamespace(".rxGpuHostLib", "rxode2")
.rxGpuHostSolve <- utils::getFromNamespace(".rxGpuHostSolve", "rxode2")
.args <- commandArgs(TRUE)
.jl <- utils::read.csv(.args[1])

.models <- list(
  lorenz = list(
    m = rxode2({
      d/dt(x) <- 10 * (y - x)
      d/dt(y) <- x * (28 - z) - y
      d/dt(z) <- x * y - 8 / 3 * z
    }),
    y0 = c(1, 0, 0),
    tf = 1
  ),
  robertson = list(
    m = rxode2({
      d/dt(y1) <- -0.04 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- 0.04 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    }),
    y0 = c(1, 0, 0),
    tf = 1e5
  ),
  pk2 = list(
    m = rxode2({
      d/dt(depot) <- -1.5 * depot
      d/dt(central) <- 1.5 * depot - (0.3 + 0.8) * central / 10 + 0.8 * periph / 30
      d/dt(periph) <- 0.8 * central / 10 - 0.8 * periph / 30
    }),
    y0 = c(100, 0, 0),
    tf = 48
  ),
  dde = list(
    m = rxode2({
      d/dt(x) <- -1.2 * delay(x, 0.7) + 0.1 * w
      d/dt(w) <- -0.5 * w
      d/dt(v) <- delay(x, 1.4) - v
    }),
    y0 = c(1, 2, 0),
    tf = 8
  )
)

## reference: rxode2's dop853 (stiff Robertson: liblsoda) at tight tolerance;
## the delay problem also runs Julia's DelayDiffEq (MethodOfSteps)
.ref <- lapply(names(.models), function(.n) {
  .x <- .models[[.n]]
  .s <- rxSolve(
    .x$m,
    et(c(0, .x$tf)),
    inits = setNames(.x$y0, rxState(.x$m)),
    method = if (.n == "robertson") "liblsoda" else "dop853",
    atol = 1e-14,
    rtol = 1e-13
  )
  unlist(.s[2, rxState(.x$m)])
})
names(.ref) <- names(.models)

## relative, with states below 1e-6 of the largest measured against that
.relErr <- function(y, r) max(abs(y - r) / pmax(abs(r), 1e-6 * max(abs(r))))

.rx <- do.call(
  rbind,
  lapply(seq_len(nrow(.jl)), function(.i) {
    .r <- .jl[.i, ]
    .x <- .models[[.r$problem]]
    .lib <- .rxGpuHostLib(.x$m)
    .s <- .rxGpuHostSolve(
      .lib,
      .r$method,
      c(0, .x$tf),
      .x$y0,
      numeric(0),
      atol = .r$tol,
      rtol = .r$tol,
      mxstep = 10000000L
    )
    .ref0 <- .ref[[.r$problem]]
    data.frame(
      problem = .r$problem,
      method = .r$method,
      julia = .r$julia,
      tol = .r$tol,
      errRx = .relErr(.s$y[2, ], .ref0),
      errJl = .relErr(c(.r$y1, .r$y2, .r$y3), .ref0),
      stepsRx = .s$cnt[["step"]],
      stepsJl = .r$steps,
      rejRx = .s$cnt[["reject"]],
      rejJl = .r$reject,
      fevRx = .s$cnt[["fev"]],
      fevJl = .r$fev,
      jacRx = .s$cnt[["jac"]],
      jacJl = .r$jac,
      stringsAsFactors = FALSE
    )
  })
)
print(.rx, digits = 3, row.names = FALSE)
if (length(.args) > 1) {
  utils::write.csv(.rx, .args[2], row.names = FALSE)
}
