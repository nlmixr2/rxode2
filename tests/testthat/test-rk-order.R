## Order test: integrate dx/dt = p*t^(p-1) over [0,1] with x(0)=0.
## A method of order p integrates this exactly (within machine epsilon).
## Mirrors rklib's rk_test_order.F90 test.
##
## NOTE: Fixed-step methods use hmin=1 (single step over [0,1]).
##       Adaptive methods use atol=rtol=1 (single coarse step).
##
## Flagged methods needing extra validation (transcription-error risk):
##   ★ f108, f1210, f1412: very many stages; also tested to tighter
##     tolerance in the decay test.
##   ⚑ FSAL methods: incorrect cache logic caught by decay test (multi-step).
##   ⚑ Low-storage (ls44, ls54): custom 2-register scheme, order tested here.
rxTest({
  .eps <- .Machine$double.eps
  .tol <- 1e4 * .eps   # 10000*eps, same threshold as rklib

  ## -- Fixed-step methods ---------------------------------------------------
  ## Each entry: list(method, order). Uses hmin=1 for a single step.
  .fixedCases <- list(
    list(method = "euler", order = 1L),
    list(method = "midpoint",order = 2L),
    list(method = "heun",  order = 2L),
    list(method = "ssp22", order = 2L),
    list(method = "rk3",   order = 3L),
    list(method = "ssp53", order = 3L),
    list(method = "s4",    order = 4L),
    list(method = "r4",    order = 4L),
    list(method = "ls44",  order = 4L),
    list(method = "ls54",  order = 4L),
    list(method = "ssp54", order = 4L),
    list(method = "s5",    order = 5L),
    list(method = "rk5",   order = 5L),
    list(method = "c5",    order = 5L),
    list(method = "l5",    order = 5L),
    list(method = "lk5a",  order = 5L),
    list(method = "lk5b",  order = 5L),
    list(method = "b6",    order = 6L),
    list(method = "s7",    order = 7L),
    list(method = "rk8_10",order = 8L),
    list(method = "cv8",   order = 8L),
    list(method = "s8_12", order = 8L),
    list(method = "s10",   order = 10L),
    list(method = "z10",   order = 10L),
    list(method = "o10",   order = 10L),
    list(method = "h10",   order = 10L)
  )

  for (.cfg in .fixedCases) {
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
    list(method = "ck54",   order = 5L),
    list(method = "dp54",   order = 5L),
    list(method = "ssp33",  order = 3L),
    list(method = "dp87",   order = 8L),
    list(method = "v65e",   order = 6L),
    list(method = "v76e",   order = 7L),
    list(method = "v98e",   order = 9L),
    ## new variable-step
    list(method = "bs32",   order = 3L),
    list(method = "f45",    order = 4L),
    list(method = "t54",    order = 5L),
    list(method = "s54",    order = 5L),
    list(method = "pp54",   order = 5L),
    list(method = "pp54b",  order = 5L),
    list(method = "bs54",   order = 5L),
    list(method = "ss54",   order = 5L),
    list(method = "dp65",   order = 6L),
    list(method = "c65",    order = 6L),
    list(method = "tp64",   order = 6L),
    list(method = "v65r",   order = 6L),
    list(method = "v65",    order = 6L),
    list(method = "dverk65",  order = 6L),
    list(method = "tf65",   order = 6L),
    list(method = "tp75",   order = 7L),
    list(method = "tmy7",   order = 7L),
    list(method = "tmy7s",  order = 7L),
    list(method = "v76r",   order = 7L),
    list(method = "ss76",   order = 7L),
    list(method = "dp85",   order = 8L),
    list(method = "tp86",   order = 8L),
    list(method = "v87e",   order = 8L),
    list(method = "v87r",   order = 8L),
    list(method = "ev87",   order = 8L),
    list(method = "k87",    order = 8L),
    list(method = "t98a",   order = 9L),
    list(method = "v98r",   order = 9L),
    list(method = "s98",    order = 9L),
    list(method = "f108",   order = 10L),   # ★ extra transcription risk
    list(method = "c108",   order = 10L),
    list(method = "b109",   order = 10L),
    list(method = "s1110a", order = 11L),
    list(method = "f1210",  order = 12L),   # ★ extra transcription risk
    list(method = "o129",   order = 12L),
    list(method = "f1412",  order = 14L)    # ★ extra transcription risk
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
