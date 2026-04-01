rxTest({

  test_that("test lag-time information parsing", {

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    alag(depot) = alagDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    eff(0) = 1")
    expect_equal(m1$alag, 1L)

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    alag(centr) = alagDepot
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    eff(0) = 1")
    expect_equal(m1$alag, 2L)

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    alag(peri) = alagDepot
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    eff(0) = 1")
    expect_equal(m1$alag, 3L)

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    alag(eff) = alagDepot
    eff(0) = 1")
    expect_equal(m1$alag, 4L)

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    alag(depot) = alagDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    alag(centr) = alagDepot
    d/dt(peri)  =                    Q*C2 - Q*C3;
    alag(peri) = alagDepot
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    alag(eff) = alagDepot
    eff(0) = 1")
    expect_equal(m1$alag, 1:4)

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    alag(depot) = alagDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    alag(peri) = alagDepot
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    alag(eff) = alagDepot
    eff(0) = 1")
    expect_equal(m1$alag, c(1L, 3:4))

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    alag(centr) = alagDepot
    d/dt(peri)  =                    Q*C2 - Q*C3;
    alag(peri) = alagDepot
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    alag(eff) = alagDepot
    eff(0) = 1")
    expect_equal(m1$alag, 2:4)

    m1 <- rxode2parse("KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    fdepot = 1;
    durDepot = 8;
    rateDepot = 1250;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    f(depot) = fdepot
    dur(depot) = durDepot
    rate(depot) = rateDepot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    eff(0) = 1")
    expect_equal(m1$alag, integer(0))

  })


  et <- et(1:10)
  et$b <- 1:10

  test_that("lag()", {

    suppressMessages(expect_error(rxode2({
      a <- lag()
    })))

    suppressMessages(expect_error(rxode2({
      a <- lag(b, c)
    })))

    m1 <- rxode2({
      c <- b + 2
      a <- lag(b, 3)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, NA, NA, 1, 2, 3, 4, 5, 6, 7))

    m1 <- rxode2({
      a <- lag(b, 3)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, NA, NA, 1, 2, 3, 4, 5, 6, 7))

    m1 <- rxode2({
      a <- lag(b, -1)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(2, 3, 4, 5, 6, 7, 8, 9, 10, NA))

    m1 <- rxode2({
      a <- lag(b, 0)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, 1:10)

    m1 <- rxode2({
      a <- lag(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    m1 <- rxode2({
      a <- b
      c <- lag(a)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$c, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    m1 <- rxode2({
      a <- b
      c <- lag(a, 1)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$c, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    m1 <- rxode2({
      a <- b
      c <- lead(a, -1)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$c, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    m1 <- rxode2({
      c <- b + 3
      a <- lag(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  })

  test_that("lead()", {
    suppressMessages(expect_error(rxode2({
      a <- lead()
    })))

    suppressMessages(expect_error(rxode2({
      a <- lead(b, c)
    })))

    m1 <- rxode2({
      c <- b + 2
      a <- lead(b, 3)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(4, 5, 6, 7, 8, 9, 10, NA, NA, NA))

    m1 <- rxode2({
      a <- lead(b, 3)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(4, 5, 6, 7, 8, 9, 10, NA, NA, NA))

    m1 <- rxode2({
      a <- lead(b, -1)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    m1 <- rxode2({
      a <- lead(b, 0)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, 1:10)

    m1 <- rxode2({
      a <- lead(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(2:10, NA))

    m1 <- rxode2({
      c <- b + 3
      a <- lead(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(2:10, NA))
  })

  test_that("first()", {
    suppressMessages(expect_error(rxode2({
      a <- first()
    })))

    suppressMessages(expect_error(rxode2({
      a <- first(b, 1)
    })))

    suppressMessages(expect_error(rxode2({
      a <- first(b, 1, 2)
    })))

    m1 <- rxode2({
      a <- first(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_true(all(x1$a == 1))

    m1 <- rxode2({
      c <- b + 3
      a <- first(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_true(all(x1$a == 1))
  })

  test_that("last()", {
    suppressMessages(expect_error(rxode2({
      a <- last()
    })))

    suppressMessages(expect_error(rxode2({
      a <- last(b, 1)
    })))

    suppressMessages(expect_error(rxode2({
      a <- last(b, 1, 2)
    })))

    m1 <- rxode2({
      a <- last(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_true(all(x1$a == 10))

    m1 <- rxode2({
      c <- b + 3
      a <- last(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_true(all(x1$a == 10))
  })


  et <- et(1:10)
  et$b <- 2^(1:10)

  test_that("diff()", {
    suppressMessages(expect_error(rxode2({
      a <- diff()
    })))

    suppressMessages(expect_error(rxode2({
      a <- diff(b, 1, 2)
    })))

    suppressMessages(expect_error(rxode2({
      a <- diff(b, 1.2)
    })))

    suppressMessages(expect_error(rxode2({
      a <- diff(b, c)
    })))

    suppressMessages(expect_error(rxode2({
      a <- diff(b, -1)
    })))

    suppressMessages(expect_error(rxode2({
      a <- diff(b, 0)
    })))

    m1 <- rxode2({
      a <- diff(b)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, 2, 4, 8, 16, 32, 64, 128, 256, 512))

    m1 <- rxode2({
      c <- b
      a <- diff(c)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, 2, 4, 8, 16, 32, 64, 128, 256, 512))

    m1 <- rxode2({
      c <- b
      a <- diff(c, 1)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, 2, 4, 8, 16, 32, 64, 128, 256, 512))

    m1 <- rxode2({
      a <- diff(b, 2)
    })

    expect_s3_class(m1, "rxode2")

    x1 <- m1 |> rxSolve(et)

    expect_equal(x1$a, c(NA, NA, 6, 12, 24, 48, 96, 192, 384, 768))
  })

  test_that("bad lag() types", {
    suppressMessages(expect_error(rxode2({
      a ~ c + d
      b <- lag(a)
    })))

    suppressMessages(expect_error(rxode2({
      d / dt(a) <- 3
      b <- lag(a)
    })))

    suppressMessages(expect_error(rxode2({
      a <- a + 3
      b <- lag(a)
    })))

    suppressMessages(expect_error(rxode2({
      a <- 13 + b
      b <- lag(a, 3)
    })))

    suppressMessages(expect_error(rxode2({
      a <- 13 + b
      b <- lead(a)
    })))
  })

  test_that("test sticky lhs", {
    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      isna <- is.na(amt)
      if (!is.na(amt)) {
        tdose <- time
      } else {
        tad <- time - tdose
      }
    })

    ev <- et(amountUnits = "mg", timeUnits = "hours") |>
      et(amt = 10000, addl = 9, ii = 12, cmt = "depot") |>
      et(time = 120, amt = 2000, addl = 4, ii = 14, cmt = "depot") |>
      et(0, 240)

    r1 <- rxSolve(mod1, ev, addDosing = TRUE)

    expect_equal(max(r1$tad, na.rm = TRUE), 64)

    r2 <- rxSolve(mod1, ev, addDosing = FALSE)

    expect_equal(max(r2$tad, na.rm = TRUE), 64)
  })

  test_that("newind", {
    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      if (!is.na(amt)) {
        tdose <- time
      } else {
        tad <- time - tdose
      }
      if (newind <= 1) {
        first <- 0
      } else if (tad > 24) {
        first <- 24
      }
    })

    ev <- et(amountUnits = "mg", timeUnits = "hours") |>
      et(amt = 10000, addl = 9, ii = 12, cmt = "depot") |>
      et(time = 120, amt = 2000, addl = 4, ii = 14, cmt = "depot") |>
      et(0, 240)

    r1 <- rxSolve(mod1, ev, addDosing = TRUE)
    expect_equal(unique(r1$first), c(0, 24))

    r1 <- rxSolve(mod1, ev, addDosing = FALSE)
    expect_equal(unique(r1$first), c(0, 24))
  })
})
