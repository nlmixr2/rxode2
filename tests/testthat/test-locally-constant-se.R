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
    # the Subs(Derivative(...)) form symengine actually produces when the
    # argument is not a bare symbol
    expect_equal(rxFromSE("Subs(Derivative(floor(_xi_1), _xi_1), (_xi_1), (p/24))"),
                 "0")
    # higher-order derivatives collapse to zero too
    expect_equal(rxFromSE("Derivative(floor(_xi_1), _xi_1, _xi_1)"), "0")
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
