## Decay test: dx/dt = -x, x(0)=1 over [0,1]. Exact solution: x(1) = exp(-1).
## Mirrors rklib's rk_decay.F90 test. Tolerance 1e-4 (same as rklib).
rxTest({
  .mod <- rxode2::rxode2({ d/dt(x) <- -x })
  .ev  <- rxode2::et(1)
  .target <- exp(-1)

  ## --- Fixed-step methods (hmin = step size) --------------------------------
  .fixedMethods <- list(
    euler    = list(method = "euler",    hmin = 1e-4),  # O(h): needs h<=2e-4 for err<1e-4
    midpoint = list(method = "midpoint", hmin = 1e-3),
    heun     = list(method = "heun",     hmin = 1e-3),
    ssp22  = list(method = "ssp22",  hmin = 1e-3),
    rk3      = list(method = "rk3",      hmin = 1e-2),
    ssp53  = list(method = "ssp53",  hmin = 1e-2),
    s4     = list(method = "s4",     hmin = 1e-2),
    r4     = list(method = "r4",     hmin = 1e-2),
    ls44   = list(method = "ls44",   hmin = 1e-2),
    ls54   = list(method = "ls54",   hmin = 1e-2),
    ssp54  = list(method = "ssp54",  hmin = 1e-2),
    s5     = list(method = "s5",     hmin = 1e-1),
    rk5    = list(method = "rk5",    hmin = 1e-1),
    c5     = list(method = "c5",     hmin = 1e-1),
    l5     = list(method = "l5",     hmin = 1e-1),
    lk5a   = list(method = "lk5a",   hmin = 1e-1),
    lk5b   = list(method = "lk5b",   hmin = 1e-1),
    b6     = list(method = "b6",     hmin = 1e-1),
    s7     = list(method = "s7",     hmin = 1e-1),
    s8_10  = list(method = "s8_10",  hmin = 1e-1),
    cv8    = list(method = "cv8",    hmin = 1e-1),
    s8_12  = list(method = "s8_12",  hmin = 1e-1),
    s10    = list(method = "s10",    hmin = 1.0),
    z10    = list(method = "z10",    hmin = 1.0),
    o10    = list(method = "o10",    hmin = 1.0),
    h10    = list(method = "h10",    hmin = 1.0)
  )

  for (.nm in names(.fixedMethods)) {
    .cfg <- .fixedMethods[[.nm]]
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
    "ck54", "dp54", "v65e", "v76e", "dp87", "v98e", "ssp33",
    ## new variable-step methods
    "bs32", "ssp43", "f45",
    "t54", "s54", "pp54", "pp54b", "bs54", "ss54",
    "dp65", "c65", "tp64", "v65r", "v65", "dverk65", "tf65",
    "tp75", "tmy7", "tmy7s", "v76r", "ss76", "v78", "dverk78",
    "dp85", "tp86", "v87e", "v87r", "ev87", "k87", "f89", "v89",
    "t98a", "v98r", "s98",
    "f108", "c108", "b109", "s1110a", "f1210", "o129", "f1412"
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
