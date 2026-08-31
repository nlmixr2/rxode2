rxTest({
  # rxode2#1298: `rxSensMatExp()` splits the system term wise as
  # `dX/dt = A.X + F(X)` and keeps whatever `rhs - A.X` leaves as the indLin()
  # forcing.  symengine holds `A_ij * X_j` as a product of a sum and a symbol
  # and does not distribute it, so from two compartments up the subtraction
  # left a residual that prints as non-zero and is algebraically zero.  A
  # structurally non-zero forcing is what classifies the model as state
  # dependent, and that is what put the whole sensitivity solve on the
  # fixed-point iteration instead of one cached exponential per interval.
  #
  # These assert the MECHANISM -- nothing emitted, `doIndLin == 1`, no adaptive
  # attempt recorded -- and not only that the values are unchanged, which they
  # were the whole time (the dropped terms were zero).

  .mexp <- list(
    "1cmt" = paste("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v",
                   "cp <- central/v", sep = "\n"),
    "2cmt" = paste("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v",
                   "k_central_periph <- q/v", "k_periph_central <- q/vp",
                   "cp <- central/v", sep = "\n"),
    "3cmt" = paste("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v",
                   "k_central_periph <- q/v", "k_periph_central <- q/vp",
                   "k_central_periph2 <- q2/v", "k_periph2_central <- q2/vp2",
                   "cp <- central/v", sep = "\n"))
  .th <- c(ka = 1.1, cl = 4, v = 30, q = 8, vp = 40, q2 = 2, vp2 = 100)
  .obs <- exp(seq(log(0.05), log(24), length.out = 60))

  .lines <- function(code) trimws(strsplit(code, "\n")[[1L]])
  .forcings <- function(code) grep("^indLin\\(", .lines(code), value = TRUE)
  # `attempt` is bumped only by `indLinDriveAdaptive()`, which `doIndLin` 1 and
  # 2 never reach: zero attempts is the iterative path not having run.
  .attempts <- function() .Call("_rxode2_rxIndLinSteps", PACKAGE = "rxode2")[["attempt"]]

  test_that("a pure linear matExp sensitivity model emits no indLin() forcing", {
    for (.nm in names(.mexp)) {
      .code <- rxSensMatExp(model = .mexp[[.nm]], calcSens = c("ka", "cl", "v"))
      expect_equal(.forcings(.code), character(0), info = .nm)
      # 1 == pure matrix exponential; 3 or 4 is the fixed-point iteration.
      expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))),
                   1L, info = .nm)
    }
  })

  test_that("the second- and third-order blocks emit no forcing either", {
    # The higher-order blocks differentiate the first-order forcing once and
    # twice more, so a residual that survives first order is multiplied
    # through the whole `calcSens x calcSens2 (x calcSens3)` grid.
    .code <- rxSensMatExp(model = .mexp[["2cmt"]], calcSens = c("ka", "cl", "v"),
                          calcSens2 = c("cl", "v"))
    expect_equal(.forcings(.code), character(0))
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))), 1L)
    .code3 <- rxSensMatExp(model = .mexp[["2cmt"]], calcSens = c("cl", "v"),
                           calcSens2 = c("cl", "v"), calcSens3 = "v")
    expect_equal(.forcings(.code3), character(0))
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code3)))), 1L)
  })

  test_that("the second-order sensitivities are right on the exponential path", {
    # These models now solve under a DIFFERENT driver than they used to (one
    # cached exponential per interval, not the fixed-point iteration), so the
    # Hessian block has to be checked on the path it actually takes now.
    # Second order is differenced against FIRST order rather than the primal,
    # which is only worth something because the test above has already pinned
    # first order to the primal: the two together are the chain.
    .code <- rxSensMatExp(model = .mexp[["2cmt"]], calcSens = c("cl", "v"),
                          calcSens2 = c("cl", "v"))
    .m <- suppressMessages(rxode2(.code))
    .m1 <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[["2cmt"]],
                                                calcSens = c("cl", "v"))))
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    .run <- function(m, th) {
      suppressMessages(rxSolve(m, th, .ev, method = "indLin",
                               atol = 1e-12, rtol = 1e-12, cores = 1L))
    }
    .s <- .run(.m, .th)
    # d/dq of the first-order sensitivity wrt p is the (p, q) second-order one.
    for (.p in c("cl", "v")) {
      for (.q in c("cl", "v")) {
        .h <- .th[[.q]] * 1e-4
        .up <- .th; .up[[.q]] <- .th[[.q]] + .h
        .dn <- .th; .dn[[.q]] <- .th[[.q]] - .h
        .nm1 <- paste0("rx__sens_central_BY_", .p, "__")
        expect_equal(.s[[paste0("rx__sens_central_BY_", .p, "_BY_", .q, "__")]],
                     (.run(.m1, .up)[[.nm1]] - .run(.m1, .dn)[[.nm1]]) / (2 * .h),
                     tolerance = 1e-4, info = paste(.p, .q))
      }
    }
  })

  test_that("a residual whose state terms cancel is not called state dependent", {
    # An expansion that takes the forcing from reading a compartment to reading
    # none is kept however long it gets: here the state terms cancel but the
    # state-free part expands to 25 of them, so a rule that only compared
    # lengths would keep `central` in the forcing text and land the model on
    # the iterating driver (doIndLin 4) rather than the state-free one (2).
    .m <- paste("d/dt(central) = (p1+p2+p3+p4+p5)*(p6+p7+p8+p9+p10)",
                "- (a+b)*central - a*central - b*central")
    .code <- rxSensMatExp(model = .m, calcSens = c("p1", "a"))
    .rhs <- sub("^indLin\\([^)]*\\) *<- *", "", .forcings(.code))
    expect_false(any(grepl("central", .rhs)))
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))), 2L)
  })

  test_that("an expansion that buys no reclassification is not taken", {
    # The other side of that rule.  `central` stays in the forcing whatever is
    # done to it, so the model iterates either way -- and expanding the compact
    # product into its 16 terms would only inflate what `ME()` re-evaluates on
    # every call.  The compact form is kept.
    .m <- paste("d/dt(central) = (p1+p2+p3+p4)*(p5+p6+p7+p8)*central*central",
                "- (a+b)*y + a*y + b*y", "d/dt(y) = 0", sep = "\n")
    .code <- rxSensMatExp(model = .m, calcSens = c("p1", "a"))
    .rhs <- sub("^indLin\\([^)]*\\) *<- *", "", .forcings(.code))
    expect_true(any(grepl("(p1+p2+p3+p4)", .rhs, fixed = TRUE)))
    expect_false(any(grepl("p1*p5", .rhs, fixed = TRUE)))  # not distributed
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))), 4L)
  })

  test_that("the elimination constant is emitted in its cancelled form", {
    # `-q/v-(-q/v-cl/v)` is the same number, but the generated model
    # re-evaluates it on every ME() call.
    .code <- rxSensMatExp(model = .mexp[["2cmt"]], calcSens = c("ka", "cl", "v"))
    expect_true("k_central_output = cl/v" %in% .lines(.code))
  })

  test_that("a pure linear sensitivity solve never enters the iteration", {
    .ev <- as.data.frame(et(amt = 100, cmt = "depot", ii = 8, addl = 2) |> et(.obs))
    for (.nm in names(.mexp)) {
      .m <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[[.nm]],
                                                 calcSens = c("ka", "cl", "v"))))
      invisible(.attempts())                   # read to reset
      .s <- suppressMessages(rxSolve(.m, .th, .ev, method = "indLin",
                                     atol = 1e-10, rtol = 1e-10, cores = 1L))
      expect_equal(.attempts(), 0, info = .nm)
      expect_true(all(is.finite(.s$cp)))
    }
  })

  test_that("the sensitivities are still the derivatives of the primal", {
    # The dropped terms were zero, so this has to be unchanged -- it is the
    # guard against a cancellation that goes too far.
    .m <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[["2cmt"]],
                                               calcSens = c("ka", "cl", "v"))))
    .p <- suppressMessages(rxode2(.mexp[["2cmt"]]))
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    .s <- suppressMessages(rxSolve(.m, .th, .ev, method = "indLin",
                                   atol = 1e-12, rtol = 1e-12, cores = 1L))
    .fd <- function(nm, h) {
      .up <- .th; .up[[nm]] <- .th[[nm]] + h
      .dn <- .th; .dn[[nm]] <- .th[[nm]] - h
      (suppressMessages(rxSolve(.p, .up, .ev, method = "indLin",
                                atol = 1e-12, rtol = 1e-12))$cp -
         suppressMessages(rxSolve(.p, .dn, .ev, method = "indLin",
                                  atol = 1e-12, rtol = 1e-12))$cp) / (2 * h)
    }
    for (.nm in c("ka", "cl", "v")) {
      .an <- .s[[paste0("rx__sens_central_BY_", .nm, "__")]] / .th[["v"]]
      if (.nm == "v") .an <- .an - .s$central / .th[["v"]]^2
      expect_equal(.an, .fd(.nm, .th[[.nm]] * 1e-4), tolerance = 1e-5)
    }
  })

  test_that("the plain conversion cancels its elimination constants too", {
    # The non-sensitivity `indLin()` path builds its `k_<cmt>_output` from the
    # same column sum and is expanded for the same reason.  A linear ODE model
    # has to convert to a pure matrix exponential with no forcing at all.
    .ode <- paste("d/dt(depot) = -ka*depot",
                  "d/dt(central) = ka*depot - (cl/v)*central - (q/v)*central + (q/vp)*periph",
                  "d/dt(periph) = (q/v)*central - (q/vp)*periph",
                  "cp = central/v", sep = "\n")
    .code <- indLin(.ode)
    expect_equal(.forcings(.code), character(0))
    expect_true("k_central_output = cl/v" %in% .lines(.code))
    expect_false(any(grepl("^k_[a-z_]*_output = 0", .lines(.code))))
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))), 1L)
  })

  test_that("the cross-term accumulator sums and cancels", {
    # The `_nd` coefficients are summed by (from, to) because two families
    # collapse onto the same pair whenever the differentiated parameters
    # coincide, and the sum is now expanded before it is stored: a pair whose
    # contributions cancel has to emit nothing, not a line that prints as
    # non-zero.  The second-order model above reaches this branch, but only its
    # totals are visible there.
    .acc <- .rxIndLinNdAccumulator()
    .x <- symengine::S("a") / symengine::S("b")
    .acc$add("from", "to", .x)
    .acc$add("from", "to", -.x)          # cancels
    .acc$add("f2", "t2", .x)
    .acc$add("f2", "t2", .x)             # sums
    expect_equal(.acc$emit(), "k_f2_t2_nd = 2*a/b")
  })

  test_that("a nonlinear model keeps the forcing it needs", {
    # The expansion must cancel only what is algebraically zero: a
    # Michaelis-Menten elimination cannot leave a state-free rate matrix, so
    # every one of its forcings has to survive and the model has to stay on the
    # iterative path.
    .mm <- paste("d/dt(depot) = -ka*depot",
                 "d/dt(central) = ka*depot - vm*central/(km + central)",
                 "cp = central/v", sep = "\n")
    .code <- rxSensMatExp(model = .mm, calcSens = c("ka", "vm", "km"))
    expect_equal(length(.forcings(.code)), 4L) # central, plus one per parameter
    .m <- suppressMessages(rxode2(.code))
    expect_equal(.rxMemDoIndLin(rxModelVars(.m)), 4L)
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    invisible(.attempts())
    .s <- suppressMessages(rxSolve(.m, c(ka = 1.1, vm = 20, km = 5, v = 30), .ev,
                                   method = "indLin", atol = 1e-10, rtol = 1e-10,
                                   cores = 1L))
    expect_gt(.attempts(), 0)                  # it really did iterate
    expect_true(all(is.finite(.s$cp)))
    # ... and its sensitivities are still right, which is what a cancellation
    # that went too far would break.
    .th2 <- c(ka = 1.1, vm = 20, km = 5, v = 30)
    .p <- suppressMessages(rxode2(.mm))
    .fd <- function(nm, h) {
      .up <- .th2; .up[[nm]] <- .th2[[nm]] + h
      .dn <- .th2; .dn[[nm]] <- .th2[[nm]] - h
      (suppressMessages(rxSolve(.p, .up, .ev, atol = 1e-12, rtol = 1e-12))$cp -
         suppressMessages(rxSolve(.p, .dn, .ev, atol = 1e-12, rtol = 1e-12))$cp) / (2 * h)
    }
    for (.nm in c("ka", "vm", "km")) {
      expect_equal(.s[[paste0("rx__sens_central_BY_", .nm, "__")]] / .th2[["v"]],
                   .fd(.nm, .th2[[.nm]] * 1e-4), tolerance = 1e-4, info = .nm)
    }
  })
})
