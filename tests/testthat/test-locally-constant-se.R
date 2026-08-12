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
      # and with respect to the argument actually differentiated
      .d <- paste0("Derivative(", .e, ", p)")
      expect_equal(rxFromSE(.d), "0", info = .e)
    }
    # including the second argument of the two-argument forms
    expect_equal(rxFromSE("Derivative(fround(p, n), n)"), "0")
    expect_equal(rxFromSE("Derivative(fprec(p, n), n)"), "0")
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
    # each of these disagrees with the base R spelling and must match the C
    # solver: a half rounds away from zero, and fsign() takes 0 as positive
    expect_equal(rxode2:::.rxAdjEvalNum("round(p)", c(p = 2.5)), 3)
    expect_equal(rxode2:::.rxAdjEvalNum("round(p)", c(p = -2.5)), -3)
    expect_equal(rxode2:::.rxAdjEvalNum("fsign(p,q)", c(p = 2.5, q = 0)), 2.5)
  })

  test_that("the numeric shims agree with the compiled model", {
    .m <- suppressMessages(rxode2(paste0(
      "r=round(p)\nc1=ceil(p)\nfl=floor(p)\ntr=trunc(p)\nsg=sign(p)\n",
      "fr=fround(p,1)\nfp=fprec(p,2)\nfs=fsign(p,q)\nd/dt(A)=0\n")))
    .txt <- c(r = "round(p)", c1 = "ceil(p)", fl = "floor(p)", tr = "trunc(p)",
              sg = "sign(p)", fr = "fround(p,1)", fp = "fprec(p,2)",
              fs = "fsign(p,q)")
    for (.p in list(c(p = 2.5, q = 0), c(p = -2.5, q = 0), c(p = 1.26, q = -1),
                    c(p = -3.5, q = 2))) {
      .s <- rxSolve(.m, .p, et(0), returnType = "data.frame", addDosing = FALSE)
      for (.n in names(.txt)) {
        expect_equal(rxode2:::.rxAdjEvalNum(.txt[[.n]], .p), .s[[.n]],
                     info = paste(.txt[[.n]], paste(.p, collapse = ",")))
      }
    }
  })

  test_that("fsign(x, y) transfers the sign of y onto abs(x)", {
    # `(y >= 0) ? fabs(x) : -fabs(x)`, so y == 0 counts as positive and the
    # derivative cannot be written with sign(y), whose value there is 0
    .m <- suppressMessages(rxode2("fs=fsign(p,q)\nd/dt(A)=0\n"))
    .s <- rxSolve(.m, c(p = 2.5, q = 0), et(0), returnType = "data.frame",
                  addDosing = FALSE)
    expect_equal(.s$fs, 2.5)

    expect_equal(rxFromSE("Derivative(fsign(x, y), x)"), "sign(x)*fsign(1, y)")
    expect_equal(rxFromSE("Derivative(fsign(x, y), y)"), "0")
    # the first derivative is itself locally constant
    expect_equal(rxFromSE("Derivative(fsign(x, y), x, x)"), "0")
    # the derivative round-trips back into symengine
    expect_error(rxS("fl=sign(p)*fsign(1,q)\nd/dt(A)=-fl*A\n"), NA)
  })

  test_that("Subs(Derivative(...)) collapses in every argument position", {
    # symengine emits one _xi_<i> per differentiated position, each in its own
    # single-element Subs -- including the second argument of a two-argument form
    expect_equal(
      rxFromSE("Subs(Derivative(fround(_xi_1, n), _xi_1), (_xi_1), (3.0*p))"),
      "0")
    expect_equal(
      rxFromSE("Subs(Derivative(fsign(1.0 + p, _xi_2), _xi_2), (_xi_2), (2.0*p))"),
      "0")
    # the same variable in both arguments gives one Subs per position
    # (rxFromSE() captures its argument, so build the text first)
    .both <- paste0("Subs(Derivative(fsign(_xi_1, 2.0*p), _xi_1), (_xi_1), (1.0 + p))",
                    " + 2.0*Subs(Derivative(fsign(1.0 + p, _xi_2), _xi_2), (_xi_2), (2.0*p))")
    expect_equal(rxFromSE(.both), "sign(1+p)*fsign(1,2*p)+0")
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
