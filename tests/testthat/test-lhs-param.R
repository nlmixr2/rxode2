rxTest({
  # Dual lhs/param values (Issue #135)

  test_that("Two defined variables", {
    mod1 <- rxode2({
      k <- k + 3
      km <- km + 4
    })

    expect_equal(mod1$lhs, c("k", "km"))
    expect_equal(mod1$params, c("k", "km"))

    expect_equal(as.vector(rxSolve(mod1, et(0), params = c(k = 3, km = 4), returnType = "matrix")), as.double(c(0, 6, 8)))
  })


  test_that("Two defined variables with ini", {
    mod1 <- rxode2({
      k <- 3
      k <- k + 3
      km <- 4
      km <- km + 4
    })
    expect_equal(mod1$lhs, c("k", "km"))
    expect_equal(mod1$params, c("k", "km"))
    expect_equal(rxInits(mod1), c(k = 3, km = 4))
    expect_equal(as.vector(rxSolve(mod1, et(0), returnType = "matrix")), as.double(c(0, 6, 8)))
  })

  test_that("lhs/params changes", {
    mod4 <- rxode2({
      j <- k + m
      k <- j + 3
    })
    expect_equal(mod4$lhs, c("j", "k"))
    expect_equal(mod4$params, c("k", "m"))
  })

  test_that("one variable/param", {
    mod1 <- rxode2({
      k <- k + 3
    })
    expect_equal(mod1$lhs, "k")
    expect_equal(mod1$params, "k")
  })

  test_that("Sc is not lhs", {
    tmp <- rxode2({
      Sc <- L^2 * ((g * e) / (g + e)) * (1 + (km * L / v))
      d / dt(L) <- ((1 / (3 * L^2))) * (((v / g) * Sc) - km * L^3)
    })
    expect_false(any(tmp$params == "Sc"))
  })


  test_that("Last item of last line still counts", {
    mod4 <- rxode2({
      j <- k
      k <- j + 3
    })
    expect_equal(mod4$lhs, c("j", "k"))
    expect_equal(mod4$params, "k")

    mod4 <- rxode2({
      j <- k + 4
      k <- j + 3
    })
    expect_equal(mod4$lhs, c("j", "k"))
    expect_equal(mod4$params, "k")
  })

  test_that("Make sure CL is correctly identified as hidden lhs not a dual", {
    mod2 <- rxode2({
      C2 <- centr / V2
      C3 ~ peri / V3
      CL ~ TCL * exp(eta.Cl)
      d / dt(depot) ~ -KA * depot
      d / dt(centr) ~ KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) ~ Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1000
      e1 <- err1
      e2 <- err2
      resp <- eff + e1
      pk <- C2 * exp(e2)
    })
    expect_false(any("CL" == mod2$params))
    expect_false(any("CL" == mod2$lhs))
  })

  test_that("newind variables not identified as dual parameters", {
    ode.1c <- rxode2({
      V <- 20
      Cl <- 1
      fc <- 1
      C2 <- center / V
      ni <- newind
      ni2 <- NEWIND
      d / dt(center) ~ -Cl * C2
      f(center) <- fc
    })

    expect_equal(ode.1c$params, c("V", "Cl", "fc"))
    expect_equal(ode.1c$lhs, c("C2", "ni", "ni2"))
  })

  test_that("suppressed assignments give correct variables", {
    mod1 <- rxode2({
      k ~ k + 3
      km ~ km + 4
      ret <- k + km
    })

    expect_equal(mod1$lhs, "ret")
    expect_equal(mod1$params, c("k", "km"))
    expect_equal(rxSolve(mod1, c(k = 1, km = 2), et(0))$ret, as.double(10))
  })

  test_that("a=NA gives correct variables", {
    mod1 <- rxode2("a=NA;\nb=2;\nc=a+b")
    expect_equal(mod1$params, "b")
    expect_equal(mod1$lhs, c("a", "c"))

    mod1 <- rxode2("a=2;\nb=NA;\nc=a+b")
    expect_equal(mod1$params, "a")
    expect_equal(mod1$lhs, c("b", "c"))

    mod1 <- rxode2("a~NA;\nb~2;\nc=a+b")
    expect_equal(mod1$params, character(0))
    expect_equal(mod1$lhs, "c")

    mod1 <- rxode2("a~2;\nb~NA;\nc=a+b")
    expect_equal(mod1$params, character(0))
    expect_equal(mod1$lhs, "c")
  })

  test_that("dual order lhs mixed with non dual-order gives right order", {

    m <- rxode2({
      param(THETA[1], THETA[2], THETA[3], THETA[4], Nominal)
      rx_yj_ ~ 162
      rx_lambda_ ~ 1
      rx_hi_ ~ 1
      rx_low_ ~ 0
      rx_expr_0 ~ exp(THETA[2])
      rx_expr_1 ~ exp(THETA[3])
      rx_expr_2 ~ exp(THETA[1])
      rx_pred_ = linCmtA(rx__PTR__, t, 2, 1, 1, -1, 1, rx_expr_0,
                         rx_expr_1, 0, 0, 0, 0, rx_expr_2)
      rx_r_ = Rx_pow_di(THETA[4], 2)
      tka = THETA[1]
      tcl = THETA[2]
      tv = THETA[3]
      add.sd = THETA[4]
      ka = rx_expr_2
      cl = rx_expr_0
      v = rx_expr_1
      Nominal = Nominal
      tad = tad()
      dosenum = dosenum()
      cmt(rxLinCmt)
      dvid(3)
    })

    expect_equal(m$lhs,
                 c("rx_pred_", "rx_r_", "tka", "tcl", "tv", "add.sd", "ka", "cl", "v", "Nominal", "tad", "dosenum"))

    theo_sd2 <- nlmixr2data::theo_sd
    theo_sd2$Nominal <- 300

    s <- rxSolve(m, theo_sd2, c(`THETA[1]` = 0.440455057067531, `THETA[2]` = 0.962169566880609,
                                `THETA[3]` = 3.49144561224211, `THETA[4]` = 1.39422305101919),
                 addCov=FALSE)

    expect_true(all(s$Nominal == 300))
    expect_false(any(s$rx_pred_ == 300))

  })
})
