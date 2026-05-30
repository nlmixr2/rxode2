## Decay test: dx/dt = -x, x(0)=1 over [0,1]. Exact solution: x(1) = exp(-1).
## Mirrors rklib's rk_decay.F90 test. Tolerance 1e-4 (same as rklib).
rxTest({
  .mod <- rxode2::rxode2({ d/dt(x) <- -x })
  .ev  <- rxode2::et(1)
  .target <- exp(-1)

  ## --- Fixed-step methods (hmin = step size) --------------------------------
  .fixed_methods <- list(
    euler    = list(method = "euler",    hmin = 1e-4),  # O(h): needs h<=2e-4 for err<1e-4
    midpoint = list(method = "midpoint", hmin = 1e-3),
    heun     = list(method = "heun",     hmin = 1e-3),
    rkssp22  = list(method = "rkssp22",  hmin = 1e-3),
    rk3      = list(method = "rk3",      hmin = 1e-2),
    rkssp53  = list(method = "rkssp53",  hmin = 1e-2),
    rks4     = list(method = "rks4",     hmin = 1e-2),
    rkr4     = list(method = "rkr4",     hmin = 1e-2),
    rkls44   = list(method = "rkls44",   hmin = 1e-2),
    rkls54   = list(method = "rkls54",   hmin = 1e-2),
    rkssp54  = list(method = "rkssp54",  hmin = 1e-2),
    rks5     = list(method = "rks5",     hmin = 1e-1),
    rk5      = list(method = "rk5",      hmin = 1e-1),
    rkc5     = list(method = "rkc5",     hmin = 1e-1),
    rkl5     = list(method = "rkl5",     hmin = 1e-1),
    rklk5a   = list(method = "rklk5a",   hmin = 1e-1),
    rklk5b   = list(method = "rklk5b",   hmin = 1e-1),
    rkb6     = list(method = "rkb6",     hmin = 1e-1),
    rk7      = list(method = "rk7",      hmin = 1e-1),
    rk8_10   = list(method = "rk8_10",   hmin = 1e-1),
    rkcv8    = list(method = "rkcv8",    hmin = 1e-1),
    rk8_12   = list(method = "rk8_12",   hmin = 1e-1),
    rks10    = list(method = "rks10",    hmin = 1.0),
    rkz10    = list(method = "rkz10",    hmin = 1.0),
    rko10    = list(method = "rko10",    hmin = 1.0),
    rkh10    = list(method = "rkh10",    hmin = 1.0)
  )

  for (.nm in names(.fixed_methods)) {
    .cfg <- .fixed_methods[[.nm]]
    test_that(paste("decay test:", .nm), {
      .s <- rxode2::rxSolve(.mod, .ev, inits = c(x = 1),
                             method = .cfg$method, hmin = .cfg$hmin, cores = 1)
      expect_lt(abs(.s$x[1] - .target), 1e-4,
                label = paste(.nm, "decay error"))
    })
  }

  ## --- Adaptive (variable-step) methods: atol=rtol=1e-6 --------------------
  .adaptive_methods <- c(
    ## aliases for existing implementations
    "rkck54", "rkdp54", "rkv65e", "rkv76e", "rkdp87", "rkv98e", "rkssp33",
    ## new variable-step methods
    "rkbs32", "rkssp43", "rkf45",
    "rkt54", "rks54", "rkpp54", "rkpp54b", "rkbs54", "rkss54",
    "rkdp65", "rkc65", "rktp64", "rkv65r", "rkv65", "dverk65", "rktf65",
    "rktp75", "rktmy7", "rktmy7s", "rkv76r", "rkss76", "rkv78", "dverk78",
    "rkdp85", "rktp86", "rkv87e", "rkv87r", "rkev87", "rkk87", "rkf89", "rkv89",
    "rkt98a", "rkv98r", "rks98",
    "rkf108", "rkc108", "rkb109", "rks1110a", "rkf1210", "rko129", "rkf1412"
  )

  for (.meth in .adaptive_methods) {
    test_that(paste("decay test:", .meth), {
      .s <- rxode2::rxSolve(.mod, .ev, inits = c(x = 1),
                             method = .meth, atol = 1e-8, rtol = 1e-6, cores = 1)
      expect_lt(abs(.s$x[1] - .target), 1e-4,
                label = paste(.meth, "decay error"))
    })
  }
})
