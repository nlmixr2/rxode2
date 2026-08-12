rxTest({
  # rxode2 issue #1230: floor()/ceil()/round()/trunc()/sign() (and fround(),
  # fprec(), fsign()) parse and solve, but could not be loaded into a symengine
  # environment, so any model using them was unusable with every nlmixr2
  # estimation method.  They are locally constant, so the derivative is 0 almost
  # everywhere.

  .locallyConstant <- c("floor(p/24)", "ceil(p/24)", "round(p/24)",
                        "trunc(p/24)", "sign(p-12)", "fround(p,2)",
                        "fprec(p,3)")

  test_that("locally constant functions load into symengine", {
    for (.e in c(.locallyConstant, "fsign(p-12,q)")) {
      .m <- paste0("fl=", .e, "\nd/dt(A)=-fl*A\n")
      expect_error(rxS(.m, TRUE, promoteLinSens = TRUE), NA, info = .e)
    }
  })

  test_that("locally constant functions have a zero derivative", {
    for (.e in .locallyConstant) {
      # rxFromSE() captures its argument, so build the text first
      .d <- paste0("Derivative(", .e, ", eta1)")
      expect_equal(rxFromSE(.d), "0", info = .e)
    }
    # ftrunc() cannot be written in a model today: its arity table says 2
    # arguments while C's Rf_ftrunc takes 1, so neither spelling builds.  Only
    # the derivative rule is reachable, and only with the declared arity
    expect_equal(rxFromSE("Derivative(ftrunc(p, 1), eta1)"), "0")
    # the Subs(Derivative(...)) form symengine actually produces when the
    # argument is not a bare symbol
    expect_equal(rxFromSE("Subs(Derivative(floor(_xi_1), _xi_1), (_xi_1), (p/24))"),
                 "0")
    # higher-order derivatives collapse to zero too
    expect_equal(rxFromSE("Derivative(floor(_xi_1), _xi_1, _xi_1)"), "0")
  })

  test_that("the delay family still has zero derivatives at every order", {
    # the same .rxFromSE() branch handles the delay family; guard it against a
    # regression from sharing the name list
    for (.f in c("delay", "lag", "lead", "rxDelayD", "rxDelayD2", "rxDelayD3")) {
      .d1 <- paste0("Derivative(", .f, "(A, tau), eta1)")
      expect_equal(rxFromSE(.d1), "0", info = .f)
      .d2 <- paste0("Derivative(", .f, "(_xi_1, tau), _xi_1, _xi_1)")
      expect_equal(rxFromSE(.d2), "0", info = .f)
    }
  })

  test_that("rounding-family functions evaluate numerically for dose duals", {
    # .rxAdjEvalNum() evaluates a dosing modifier (alag/rate/dur) and its
    # parameter derivatives at parameter values; a name it cannot find is
    # swallowed into NA, which silently zeroes the dose duals
    expect_equal(rxode2:::.rxAdjEvalNum("floor(p)", c(p = 1.26)), 1)
    expect_equal(rxode2:::.rxAdjEvalNum("ceil(p)", c(p = 1.26)), 2)
    expect_equal(rxode2:::.rxAdjEvalNum("round(p)", c(p = 1.26)), 1)
    expect_equal(rxode2:::.rxAdjEvalNum("trunc(p)", c(p = 1.26)), 1)
    expect_equal(rxode2:::.rxAdjEvalNum("sign(p)", c(p = 1.26)), 1)
    expect_equal(rxode2:::.rxAdjEvalNum("ftrunc(p)", c(p = 1.26)), 1)
    expect_equal(rxode2:::.rxAdjEvalNum("fround(p,1)", c(p = 1.26)), 1.3)
    expect_equal(rxode2:::.rxAdjEvalNum("fprec(p,2)", c(p = 1.26)), 1.3)
    expect_equal(rxode2:::.rxAdjEvalNum("fsign(p,-1)", c(p = 1.26)), -1.26)
  })

  test_that("fsign(x, y) = abs(x)*sign(y) derivatives", {
    expect_equal(rxFromSE("Derivative(fsign(x, y), x)"), "sign(x)*sign(y)")
    expect_equal(rxFromSE("Derivative(fsign(x, y), y)"), "0")
    # the first derivative is itself locally constant
    expect_equal(rxFromSE("Derivative(fsign(x, y), x, x)"), "0")
  })

  test_that("symengine differentiation of a locally constant lhs", {
    .s <- rxS("fl=floor(p/2)*sin(p)\nd/dt(A)=-fl*A\n")
    .d <- with(.s, D(fl, p))
    expect_equal(rxFromSE(.d), "0.5*sin(p)*0+cos(p)*floor(0.5*p)")
  })

  test_that("sensitivities build for a model with a locally constant switch", {
    .m <- "ka=exp(tka+eta1)
cl=exp(tcl)
v=exp(tv)
fl=0
if ((t - floor(t/24)*24) > 12) {
  fl <- 1
}
d/dt(depot)=-ka*depot
d/dt(center)=ka*depot - cl/v*center*(1+fl)
cp=center/v
"
    expect_error(suppressMessages(rxode2(.m, calcSens = TRUE, calcJac = TRUE)), NA)
    expect_error(suppressMessages(rxode2(.m, calcSens = "eta1", calcSens2 = "eta1",
                                         calcJac = TRUE)), NA)
  })
})
