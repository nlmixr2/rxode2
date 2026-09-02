rxTest({
  # rxode2#1298, the decision half: `.rxIndLinExpand()` is handed two
  # algebraically equal forms of the same expression -- what the term-wise
  # `rhs - A.X` split wrote, and its expansion -- and has to pick one.  The
  # rule is the classification boundary: an expansion that takes a forcing from
  # reading a compartment to reading none is kept however long it gets, because
  # that is the difference between one cached exponential per interval and the
  # fixed-point iteration; anything else is a rewrite of the same dependencies
  # and is kept only when it does not grow.
  #
  # The models here are contrived on purpose -- each one is the smallest thing
  # that puts the rule on one side of that boundary.  For the ordinary linear
  # compartment models the fix was about, see test-ind-lin-1298.R.

  .lines <- function(code) trimws(strsplit(code, "\n")[[1L]])
  .forcings <- function(code) grep("^indLin\\(", .lines(code), value = TRUE)
  .rhsOf <- function(code) sub("^indLin\\([^)]*\\) *<- *", "", .forcings(code))
  # `attempt` is bumped only by `indLinDriveAdaptive()`, which `doIndLin` 1 and
  # 2 never reach: zero attempts is the iterative path not having run.
  .attempts <- function() .Call("_rxode2_rxIndLinSteps", PACKAGE = "rxode2")[["attempt"]]
  .obs <- exp(seq(log(0.05), log(24), length.out = 60))

  test_that("a residual whose state terms cancel is not called state dependent", {
    # The state terms cancel but the state-free part expands to 25 of them, so
    # a rule that only compared lengths would keep `central` in the forcing text
    # and land the model on the iterating driver (doIndLin 4) rather than the
    # state-free one (2).
    .m <- paste("d/dt(central) = (p1+p2+p3+p4+p5)*(p6+p7+p8+p9+p10)",
                "- (a+b)*central - a*central - b*central")
    .code <- rxSensMatExp(model = .m, calcSens = c("p1", "a"))
    expect_false(any(grepl("central", .rhsOf(.code))))
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))), 2L)
  })

  test_that("an expansion that buys no reclassification is not taken", {
    # The other side of the rule.  `central` stays in the forcing whatever is
    # done to it, so the model iterates either way -- and expanding the compact
    # product into its 16 terms would only inflate what `ME()` re-evaluates on
    # every call.  The compact form is kept.
    .m <- paste("d/dt(central) = (p1+p2+p3+p4)*(p5+p6+p7+p8)*central*central",
                "- (a+b)*y + a*y + b*y", "d/dt(y) = 0", sep = "\n")
    .code <- rxSensMatExp(model = .m, calcSens = c("p1", "a"))
    .rhs <- .rhsOf(.code)
    expect_true(any(grepl("(p1+p2+p3+p4)", .rhs, fixed = TRUE)))
    expect_false(any(grepl("p1*p5", .rhs, fixed = TRUE)))  # not distributed
    expect_equal(.rxMemDoIndLin(rxModelVars(suppressMessages(rxode2(.code)))), 4L)
  })

  test_that("the cross-term accumulator sums and cancels", {
    # The `_nd` coefficients are summed by (from, to) because two families
    # collapse onto the same pair whenever the differentiated parameters
    # coincide, and the sum is expanded before it is stored: a pair whose
    # contributions cancel has to emit nothing, not a line that prints as
    # non-zero.  A second-order model reaches this branch, but only its totals
    # are visible there.
    .acc <- .rxIndLinNdAccumulator()
    .x <- symengine::S("a") / symengine::S("b")
    .acc$add("from", "to", .x)
    .acc$add("from", "to", -.x)          # cancels
    .acc$add("f2", "t2", .x)
    .acc$add("f2", "t2", .x)             # sums
    expect_equal(.acc$emit(), "k_f2_t2_nd = 2*a/b")
  })

  test_that("a nonlinear model keeps the forcing it needs", {
    # The cancellation must reach only what is algebraically zero: a
    # Michaelis-Menten elimination cannot leave a state-free rate matrix, so
    # every one of its forcings has to survive, the model has to stay on the
    # iterative path, and its sensitivities have to stay right.
    .mm <- paste("d/dt(depot) = -ka*depot",
                 "d/dt(central) = ka*depot - vm*central/(km + central)",
                 "cp = central/v", sep = "\n")
    .code <- rxSensMatExp(model = .mm, calcSens = c("ka", "vm", "km"))
    expect_equal(length(.forcings(.code)), 4L) # central, plus one per parameter
    .m <- suppressMessages(rxode2(.code))
    expect_equal(.rxMemDoIndLin(rxModelVars(.m)), 4L)
    .th <- c(ka = 1.1, vm = 20, km = 5, v = 30)
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    invisible(.attempts())                     # read to reset
    .s <- suppressMessages(rxSolve(.m, .th, .ev, method = "indLin",
                                   atol = 1e-10, rtol = 1e-10, cores = 1L))
    expect_gt(.attempts(), 0)                  # it really did iterate
    expect_true(all(is.finite(.s$cp)))
    .p <- suppressMessages(rxode2(.mm))
    .fd <- function(nm, h) {
      .up <- .th; .up[[nm]] <- .th[[nm]] + h
      .dn <- .th; .dn[[nm]] <- .th[[nm]] - h
      (suppressMessages(rxSolve(.p, .up, .ev, atol = 1e-12, rtol = 1e-12))$cp -
         suppressMessages(rxSolve(.p, .dn, .ev, atol = 1e-12, rtol = 1e-12))$cp) / (2 * h)
    }
    for (.nm in c("ka", "vm", "km")) {
      expect_equal(.s[[paste0("rx__sens_central_BY_", .nm, "__")]] / .th[["v"]],
                   .fd(.nm, .th[[.nm]] * 1e-4), tolerance = 1e-4, info = .nm)
    }
  })
})
