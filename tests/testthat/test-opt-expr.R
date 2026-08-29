rxTest({
  test_that("simple expression optimization", {
    exp1 <- "rx_yj_~2;\nrx_lambda_~1;\nrx_pred_=10*exp(-THETA[1]*t*exp(ETA[1]));\nrx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*exp(ETA[1])*exp(-THETA[1]*t*exp(ETA[1]));\nrx_r_=100*Rx_pow_di(THETA[2],2)*exp(-2*THETA[1]*t);\ndvid(3,4)\n"
    suppressMessages(expect_equal(
      rxOptExpr(exp1),
      "rx_yj_~2\nrx_lambda_~1\nrx_expr_0~exp(ETA[1])\nrx_expr_1~exp(-THETA[1]*t*rx_expr_0)\nrx_pred_=10*rx_expr_1\nrx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*rx_expr_0*rx_expr_1\nrx_r_=100*Rx_pow_di(THETA[2], 2)*exp(-2*THETA[1]*t)\ndvid(3, 4)"
    ))
  })

  test_that("expression optimization errors", {
    suppressMessages(expect_error(
      rxOptExpr("A1=exp(-k10*(tau - tinf))*r1*(1.0 - exp(-k10*tinf))/(k10*(1.0 - exp(-tau*k10)))"),
      NA
    ))
    suppressMessages(expect_error(
      rxOptExpr("A1=r1/ka\nA1ka=-r1/ka^2\nA1k20=0\nA1b1=0\nA1r1=ka^(-1)\nA2=r1/k20\nA2ka=0\nA2k20=-r1/k20^2\nA2b1=0\nA2r1=k20^(-1)"),
      NA
    ))
  })

  suppressMessages(rxOptExpr("a=1+(-1/2)*b"))

  suppressMessages(rxOptExpr("a=-1*exp(b)"))

  suppressMessages(rxOptExpr("a=1+(((-1/2)))*b"))

  suppressMessages(rxOptExpr("a=1+(1/2)*b; c=d^(1/2); e=(1/2)*f^(1/2)"))

  test_that("simple expression optimization", {
    expect_equal(length(..rxOpt(quote(exp(ETA[1] + THETA[4]) + 0))), 1L)
  })

  test_that("a delay() state with no d/dt() anywhere raises an error", {
    .pad <- vapply(1:60, function(i) {
      sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)", i, i)
    }, character(1))
    .m <- paste(c(.pad, "a=delay(x, exp(THETA[2]))+1", "rx_pred_=a"), collapse = "\n")
    .err <- tryCatch({
      .z <- capture.output(suppressMessages(rxOptExpr(.m, "model")), type = "output")
      NA_character_
    }, error = function(e) conditionMessage(e))
    expect_false(is.na(.err))
  })









  # A past() duration is an ordinary expression, not a left-hand side; rendering it with
  # ..rxOptLhs() only ever worked for a name, a number, `(x)` and `x/y` (the last two by
  # accident -- they are there for d/dt(x)) and stopped on anything else (#1192).
  .pastModel <- function(tau) {
    paste(c("lT=1.2", "a=1", "b=0.5", "k3=5", "kg=0.4",
            "G(0)=a", "d/dt(G)=k3-kg*G",
            sprintf("past(G,%s)=a*exp(b*t)", tau),
            sprintf("z1=delay(G,%s)", tau),
            sprintf("z2=2*delay(G,%s)", tau)), collapse = "\n")
  }

  test_that("a past() duration that is an expression optimizes (#1192)", {
    for (.tau in c("exp(lT)", "lT*2", "2^lT", "exp(THETA[1]+ETA[1])")) {
      .o <- suppressMessages(rxOptExpr(.pastModel(.tau), "model", chunkLines = 0L))
      expect_error(rxModelVars(.o), NA)
      # the duration picked up the same temporary the delay() calls did, so the
      # past() history still matches its delay() -- see .rxValidatePast()
      expect_error(.rxValidatePast(rxModelVars(.o)), NA)
      expect_true(grepl("past\\(G,rx_expr_[0-9]+\\)", .o))
      expect_true(grepl("delay\\(G, *rx_expr_[0-9]+\\)", .o))
    }
  })

  test_that("a degenerate past() duration is rendered exactly as before", {
    for (.tau in c("lT", "12.8", "(lT)")) {
      .o <- suppressMessages(rxOptExpr(.pastModel(.tau), "model", chunkLines = 0L))
      expect_true(grepl(sprintf("past(G,%s)=", .tau), .o, fixed = TRUE))
      expect_error(.rxValidatePast(rxModelVars(.o)), NA)
    }
  })



  test_that("an unsupported lhs names itself and does not print (#1192)", {
    # the branch is only reachable directly -- an unsupported lhs is already a parse
    # error in rxModelVars() -- but the stray print() it used to do landed in the
    # middle of the progress bar
    expect_error(..rxOptLhs(quote(foo(bar, baz))),
                 "foo(bar, baz)", fixed = TRUE)
    expect_output(try(..rxOptLhs(quote(foo(bar, baz))), silent = TRUE), NA)
  })
})
