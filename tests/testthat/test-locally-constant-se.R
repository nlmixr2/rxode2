rxTest({
  # rxode2 issue #1230: floor()/ceil()/round()/trunc()/sign() (and fround(),
  # fprec(), fsign(), ftrunc()) parse and solve, but could not be loaded into a
  # symengine environment, so any model using them was unusable with every
  # nlmixr2 estimation method.  They are locally constant, so the derivative is
  # 0 almost everywhere.  The same load bug hit every other function symengine
  # has no method for; see "every parser-known function loads into symengine".

  .locallyConstant <- c("floor(p/24)", "ceil(p/24)", "round(p/24)",
                        "trunc(p/24)", "sign(p-12)", "fround(p,2)",
                        "fprec(p,3)", "ftrunc(p)")

  test_that("locally constant functions load into symengine", {
    for (.e in c(.locallyConstant, "fsign(p-12,q)")) {
      .m <- paste0("fl=", .e, "\nd/dt(A)=-fl*A\n")
      expect_error(rxS(.m, TRUE, promoteLinSens = TRUE), NA, info = .e)
    }
  })

  test_that("every parser-known function loads into symengine", {
    # the same bug hit a whole family (bessel_*, gammaq, fmax2, logspace_add,
    # the llik*D* derivative helpers, dSELU, ...): symengine's Math group
    # generic has no method for them, so the assignment stored a non-Basic and
    # the model was silently emitted with `<var>=.expr`.  rxS() now loads every
    # .rxSEeq function with a known arity except the ones symengine
    # differentiates itself (.rxSEnative), so this guards the split
    .tbl <- rxode2:::.rxSEeq
    .tbl <- .tbl[!is.na(.tbl) & .tbl >= 1 & .tbl <= 5]
    # linCmtA/linCmtB need a solved-system pointer, not a plain lhs, and the
    # internal-only spellings are deliberately not accepted by the parser
    .tbl <- .tbl[!(names(.tbl) %in% c("linCmtA", "linCmtB",
                                      rxode2:::.rxSEinternalOnly))]
    for (.nm in names(.tbl)) {
      .args <- paste(paste0("p", seq_len(.tbl[[.nm]])), collapse = ",")
      .m <- paste0("fl=", .nm, "(", .args, ")\nd/dt(A)=-fl*A\n")
      .s <- suppressWarnings(try(rxS(.m, TRUE, promoteLinSens = FALSE),
                                 silent = TRUE))
      expect_false(inherits(.s, "try-error"), info = .nm)
      # and the loaded value really is a symengine object, not a try-error
      # character vector emitted into the model as `fl=.expr`
      if (!inherits(.s, "try-error")) {
        expect_true(any(grepl(paste0("^fl=", .nm, "\\("), .s$..lhs)) ||
                      !any(grepl("\\.expr", .s$..lhs)), info = .nm)
      }
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
    expect_equal(rxFromSE("Derivative(ftrunc(p), eta1)"), "0")
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

  test_that("ftrunc() takes one argument, like C's Rf_ftrunc()", {
    .m <- suppressMessages(rxode2("ft=ftrunc(p)\ntr=trunc(p)\nd/dt(A)=0\n"))
    for (.p in c(2.7, -2.7, 0, 1)) {
      .s <- rxSolve(.m, c(p = .p), et(0), returnType = "data.frame",
                    addDosing = FALSE)
      expect_equal(.s$ft, trunc(.p), info = as.character(.p))
      expect_equal(.s$ft, .s$tr, info = as.character(.p))
    }
    # the arity table said 2, so neither spelling built: one argument tripped
    # the parser and two tripped the C compiler
    expect_error(suppressMessages(rxode2("ft=ftrunc(p,1)\nd/dt(A)=0\n")))
  })

  test_that("dSwish() expands to a balanced symengine expression", {
    # the expansion was missing its closing paren, so the text could not be
    # parsed back and any model using dSwish() failed to load
    .t <- "dSwish(p1)"
    .se <- rxToSE(.t)
    expect_error(str2lang(.se), NA)
    .ev <- new.env(parent = baseenv())
    for (.p in c(-2.3, -0.7, 0, 0.4, 1.9)) {
      assign("p1", .p, envir = .ev)
      expect_equal(eval(str2lang(.se), envir = .ev), dSwish(.p), info = as.character(.p))
    }
    expect_error(rxS("fl=dSwish(p1)\nd/dt(A)=-fl*A\n"), NA)
  })

  test_that("the parser only advertises functions that compile", {
    # a name in the arity table but with no C implementation was accepted by the
    # parser and then generated C that could not compile ("implicit declaration
    # of function 'abs0'"), which rxode2 asked the user to report as a bug.
    # abs0() and polygamma() exist only between rxToSE() and rxFromSE();
    # d2PReLU() had no implementation anywhere
    for (.e in c("abs0(p)", "polygamma(1,p)", "d2PReLU(p,q)")) {
      .m <- paste0("fl=", .e, "\nd/dt(A)=-A\n")
      expect_error(suppressMessages(rxode2(.m)), info = .e)
    }
    # ... while both directions still convert them
    expect_equal(rxToSE("digamma(a)"), "polygamma(0,a)")
    expect_equal(rxFromSE("polygamma(0, a)"), "digamma(a)")
    expect_equal(rxFromSE("polygamma(1, a)"), "trigamma(a)")
    expect_equal(rxToSE("psigamma(a,2)"), "polygamma(2,a)")
    expect_equal(rxToSE("abs0(a)"), rxToSE("fabs(a)"))
  })
})
