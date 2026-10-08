rxTest({
  test_that("rxS() keeps the value an lhs reads from a reassigned lagged variable (#1435)", {
    .ode <- "d/dt(central) = -0.1*central"
    m <- rxode2(paste(
      .ode,
      "c0 = central/10",
      "c1 = 2*c0 + lag(c0)",
      "c0 = c0*3",
      "c2 = c0 + c1",
      "c0 = c0 + 1",
      "cp = c1 + c2 + lag(c0)",
      sep = "\n"
    ))
    s <- rxS(m)
    expect_equal(
      s$..lhs,
      c(
        "c0=0.1*central",
        "rx_lagv1_c0=c0",
        "c1=2*rx_lagv1_c0+lag(c0)",
        "c0=3*rx_lagv1_c0",
        "rx_lagv2_c0=c0",
        "c2=2*rx_lagv1_c0+rx_lagv2_c0+lag(c0)",
        "c0=1+rx_lagv2_c0",
        "cp=4*rx_lagv1_c0+rx_lagv2_c0+3*lag(c0)"
      )
    )
    m2 <- rxode2(paste(c(.ode, s$..lhs), collapse = "\n"))
    e <- et(amt = 100) |> et(0:5)
    r1 <- rxSolve(m, e)
    r2 <- rxSolve(m2, e)
    expect_equal(r2$cp, r1$cp)
    expect_equal(r2$c0, r1$c0)
  })
  test_that("rxS() keeps diff() of a reassigned lagged variable at its point of use (#1435)", {
    .ode <- "d/dt(central) = -0.1*central"
    e <- et(amt = 100) |> et(0:5)
    for (.f in c("diff(c0)", "diff0(c0)", "diff(c0,1)", "lag0(c0)")) {
      m <- rxode2(paste(
        .ode,
        "c0 = central/10",
        paste0("c1 = 2*c0 + ", .f),
        "c0 = c0*3",
        paste0("c2 = ", .f),
        "cp = c1 + 2*c2",
        sep = "\n"
      ))
      r1 <- rxSolve(m, e)
      s <- rxS(m)
      m2 <- rxode2(paste(c(.ode, s$..lhs), collapse = "\n"))
      expect_equal(rxSolve(m2, e)$cp, r1$cp, info = .f)
      # loading the generated model again keeps its meaning
      m3 <- rxode2(paste(c(.ode, rxS(m2)$..lhs), collapse = "\n"))
      expect_equal(rxSolve(m3, e)$cp, r1$cp, info = .f)
    }
  })
  test_that("rxS() snapshots a reassigned lagged variable named like a symengine constant (#1435)", {
    m <- rxode2(paste(
      "d/dt(central) = -0.1*central",
      "pi = central/10",
      "y = 2*pi + lag(pi)",
      "pi = 3*pi",
      "cp = y",
      sep = "\n"
    ))
    expect_equal(
      rxS(m)$..lhs,
      c(
        "pi=0.1*central",
        "rx_lagv1_pi=pi",
        "y=2*rx_lagv1_pi+lag(pi)",
        "pi=3*rx_lagv1_pi",
        "cp=2*rx_lagv1_pi+lag(pi)"
      )
    )
  })
  test_that("rxS() binds the snapshot in a d/dt() between assignments of a lagged variable (#1445)", {
    m <- rxode2(paste(
      "ka = 1.5; cl = 2.7; v = 30; ke = 0.3",
      "d/dt(depot) = -ka*depot",
      "d/dt(central) = ka*depot - cl/v*central",
      "c0 = central/v",
      "d/dt(eff) = ke*(c0 - eff)",
      "c0 = c0*1.4",
      "cp = eff + lag(c0)",
      sep = "\n"
    ))
    s <- rxS(m)
    expect_equal(s$..ddt[3], "d/dt(eff)=0.3*(-eff+rx_lagv1_c0)")
    expect_equal(
      s$..lhs,
      c("c0=0.0333333333333333*central", "rx_lagv1_c0=c0", "c0=1.4*rx_lagv1_c0", "cp=eff+lag(c0)")
    )
    # the snapshot is defined before the ODEs that read it
    m2 <- rxode2(paste(c(s$..lhs, s$..ddt), collapse = "\n"))
    e <- et(amt = 100) |> et(seq(0, 24, by = 2))
    r1 <- rxSolve(m, e)
    r2 <- rxSolve(m2, e)
    expect_equal(r2$eff, r1$eff)
    expect_equal(r2$cp, r1$cp)
  })
  test_that("a d/dt() after the last assignment of a lagged variable reads the variable (#1445)", {
    s <- rxS(rxode2(paste(
      "c0 = central/10",
      "c0 = c0*3",
      "d/dt(central) = -0.1*c0",
      "cp = lag(c0)",
      sep = "\n"
    )))
    expect_equal(s$..ddt, "d/dt(central)=-0.1*c0")
  })
})
