## Order test: integrate dx/dt = p*t^(p-1) over [0,1] with x(0)=0.
## A method of order p integrates this exactly (within machine epsilon).
## Mirrors rklib's rk_test_order.F90 test.
##
## NOTE: Fixed-step methods use hmin=1 (single step over [0,1]).
##       Adaptive methods use atol=rtol=1 (single coarse step).
##
## Flagged methods needing extra validation (transcription-error risk):
##   ★ rkf108, rkf1210, rkf1412: very many stages; also tested to tighter
##     tolerance in the decay test.
##   ⚑ FSAL methods: incorrect cache logic caught by decay test (multi-step).
##   ⚑ Low-storage (rkls44, rkls54): custom 2-register scheme, order tested here.
rxTest({
  .eps <- .Machine$double.eps
  .tol <- 1e4 * .eps   # 10000*eps, same threshold as rklib

  ## -- Fixed-step methods ---------------------------------------------------
  ## Each entry: list(method, order). Uses hmin=1 for a single step.
  .fixed_cases <- list(
    list(method = "euler",   order = 1L),
    list(method = "midpoint",order = 2L),
    list(method = "heun",    order = 2L),
    list(method = "rkssp22", order = 2L),
    list(method = "rk3",     order = 3L),
    list(method = "rkssp53", order = 3L),
    list(method = "rks4",    order = 4L),
    list(method = "rkr4",    order = 4L),
    list(method = "rkls44",  order = 4L),
    list(method = "rkls54",  order = 4L),
    list(method = "rkssp54", order = 4L),
    list(method = "rks5",    order = 5L),
    list(method = "rk5",     order = 5L),
    list(method = "rkc5",    order = 5L),
    list(method = "rkl5",    order = 5L),
    list(method = "rklk5a",  order = 5L),
    list(method = "rklk5b",  order = 5L),
    list(method = "rkb6",    order = 6L),
    list(method = "rk7",     order = 7L),
    list(method = "rk8_10",  order = 8L),
    list(method = "rkcv8",   order = 8L),
    list(method = "rk8_12",  order = 8L),
    list(method = "rks10",   order = 10L),
    list(method = "rkz10",   order = 10L),
    list(method = "rko10",   order = 10L),
    list(method = "rkh10",   order = 10L)
  )

  for (.cfg in .fixed_cases) {
    local({
      .meth  <- .cfg$method
      .order <- .cfg$order
      .mod <- rxode2::rxode2(paste0("d/dt(x) <- ", .order, "*t^", .order - 1L))
      .ev  <- rxode2::et(1)
      test_that(paste("order test (fixed):", .meth, "order", .order), {
        .s <- rxode2::rxSolve(.mod, .ev, inits = c(x = 0),
                               method = .meth, hmin = 1.0, cores = 1)
        expect_lt(abs(.s$x[1] - 1.0), .tol,
                  label = paste(.meth, "order test error"))
      })
    })
  }

  ## -- Variable-step methods ------------------------------------------------
  ## Use atol=rtol=1 so one coarse step is taken (mimics fixed-step mode).
  .var_cases <- list(
    ## aliases
    list(method = "rkck54",   order = 5L),
    list(method = "rkdp54",   order = 5L),
    list(method = "rkssp33",  order = 3L),
    list(method = "rkdp87",   order = 8L),
    list(method = "rkv65e",   order = 6L),
    list(method = "rkv76e",   order = 7L),
    list(method = "rkv98e",   order = 9L),
    ## new variable-step
    list(method = "rkbs32",   order = 3L),
    list(method = "rkf45",    order = 4L),
    list(method = "rkt54",    order = 5L),
    list(method = "rks54",    order = 5L),
    list(method = "rkpp54",   order = 5L),
    list(method = "rkpp54b",  order = 5L),
    list(method = "rkbs54",   order = 5L),
    list(method = "rkss54",   order = 5L),
    list(method = "rkdp65",   order = 6L),
    list(method = "rkc65",    order = 6L),
    list(method = "rktp64",   order = 6L),
    list(method = "rkv65r",   order = 6L),
    list(method = "rkv65",    order = 6L),
    list(method = "dverk65",  order = 6L),
    list(method = "rktf65",   order = 6L),
    list(method = "rktp75",   order = 7L),
    list(method = "rktmy7",   order = 7L),
    list(method = "rktmy7s",  order = 7L),
    list(method = "rkv76r",   order = 7L),
    list(method = "rkss76",   order = 7L),
    list(method = "rkdp85",   order = 8L),
    list(method = "rktp86",   order = 8L),
    list(method = "rkv87e",   order = 8L),
    list(method = "rkv87r",   order = 8L),
    list(method = "rkev87",   order = 8L),
    list(method = "rkk87",    order = 8L),
    list(method = "rkt98a",   order = 9L),
    list(method = "rkv98r",   order = 9L),
    list(method = "rks98",    order = 9L),
    list(method = "rkf108",   order = 10L),   # ★ extra transcription risk
    list(method = "rkc108",   order = 10L),
    list(method = "rkb109",   order = 10L),
    list(method = "rks1110a", order = 11L),
    list(method = "rkf1210",  order = 12L),   # ★ extra transcription risk
    list(method = "rko129",   order = 12L),
    list(method = "rkf1412",  order = 14L)    # ★ extra transcription risk
  )

  for (.cfg in .var_cases) {
    local({
      .meth  <- .cfg$method
      .order <- .cfg$order
      .mod <- rxode2::rxode2(paste0("d/dt(x) <- ", .order, "*t^", .order - 1L))
      .ev  <- rxode2::et(1)
      test_that(paste("order test (adaptive):", .meth, "order", .order), {
        .s <- rxode2::rxSolve(.mod, .ev, inits = c(x = 0),
                               method = .meth, atol = 1e10, rtol = 1e10, cores = 1)
        expect_lt(abs(.s$x[1] - 1.0), .tol,
                  label = paste(.meth, "order test error"))
      })
    })
  }
})
