rxTest({
  test_that("lag()/diff() of an lhs variable return the previous record value", {
    f <- function() {
      ini({tcl <- 1})
      model({
        cl <- exp(tcl)
        myr <- cl * time
        plag <- lag(myr, 1)
        plag1 <- lag(myr)
        pdiff <- diff(myr, 1)
        y <- plag
      })
    }
    ui <- rxode2(f)
    r <- rxSolve(ui, et(c(0, 2, 5, 9)), returnType = "data.frame")
    # previous record's myr, NA on the first record
    expect_equal(r$plag, c(NA, r$myr[1:3]))
    expect_equal(r$plag1, c(NA, r$myr[1:3]))
    # diff = current - previous
    expect_equal(r$pdiff, c(NA, diff(r$myr)))
  })

  test_that("lag()/diff() reset per individual", {
    f <- function() {
      ini({tcl <- 1})
      model({
        cl <- exp(tcl)
        myr <- cl * time
        plag <- lag(myr, 1)
        y <- plag
      })
    }
    ui <- rxode2(f)
    ev <- et(c(0, 2, 5)) %>% et(id = 1:2)
    r <- rxSolve(ui, ev, returnType = "data.frame")
    # each individual starts with NA (no carry-over across subjects)
    expect_true(all(is.na(r$plag[r$time == 0])))
    for (.id in unique(r$id)) {
      .s <- r[r$id == .id, ]
      expect_equal(.s$plag, c(NA, .s$myr[1:2]))
    }
  })

  test_that("lag() of a computed lhs survives the estimation (symengine) path", {
    f <- function() {
      ini({tcl <- 1; add.sd <- 0.5})
      model({
        cl <- exp(tcl)
        myr <- cl * time
        p <- lag(myr, 1)
        cp <- cl + p
        cp ~ add(add.sd)
      })
    }
    ui <- rxode2(f)
    # the pruned/symengine estimation model must still contain the lag
    expect_error(ui$symengineModelPrune, NA)
  })

  test_that("lag(x, n) with n > 1 is rejected for an lhs variable", {
    f <- function() {
      ini({tcl <- 1})
      model({cl <- exp(tcl); myr <- cl * time; p <- lag(myr, 2); y <- p})
    }
    expect_error(rxode2(f))
  })

  test_that("a variable may lag itself (first-order recurrence)", {
    # b = phi*lag0(b, 1) + 1 is a legal recurrence: lag0() reads the PREVIOUS
    # record's stored value, not the current uninitialized one.
    f <- function() {
      ini({phi <- 0.5})
      model({
        b <- phi * lag0(b, 1) + 1
        y <- b
      })
    }
    ui <- rxode2(f)
    r <- rxSolve(ui, et(1:6), returnType = "data.frame")
    # b_1 = 1 (lag0 = 0 on first record); b_i = 0.5*b_{i-1} + 1
    .expect <- Reduce(function(prev, .) 0.5 * prev + 1, seq_len(5), accumulate = TRUE, 1)
    expect_equal(r$y, .expect)
  })

  test_that("self-lag resets per individual", {
    f <- function() {
      ini({phi <- 0.5})
      model({b <- phi * lag0(b, 1) + 1; y <- b})
    }
    ui <- rxode2(f)
    r <- rxSolve(ui, et(1:3) %>% et(id = 1:2), returnType = "data.frame")
    for (.id in unique(r$id)) {
      expect_equal(r$y[r$id == .id], c(1, 1.5, 1.75))
    }
  })

  test_that("a non-lag self-reference is still treated as a parameter", {
    # b = b*2 (no lag) reads the current value -> b is a required input param,
    # NOT a recurrence; must not be silently turned into a self-lag.
    f <- function() {
      ini({tcl <- 1})
      model({cl <- exp(tcl); b <- b * 2; y <- b})
    }
    ui <- rxode2(f)
    expect_true("b" %in% ui$mv0$params)
  })

  test_that("lag()/diff() of a time-varying covariate return the previous value", {
    f <- function() {
      ini({tcl <- 1})
      model({
        cl <- exp(tcl)
        pw <- lag(WT, 1)
        dw <- diff(WT, 1)
        y <- cl + WT
      })
    }
    ui <- rxode2(f)
    ev <- et(c(0, 2, 5, 9))
    ev$WT <- c(70, 72, 75, 80)
    r <- rxSolve(ui, ev, returnType = "data.frame")
    expect_equal(r$pw, c(NA, 70, 72, 75))
    expect_equal(r$dw, c(NA, 2, 3, 5))
  })

  test_that("lag()/diff()/lead() of time use the record times (#1434)", {
    m <- rxode2({
      d/dt(a) <- -0.1 * a
      l1 <- lag(time)
      l2 <- lag(t)
      l3 <- lag(time, 2)
      l0 <- lag0(time)
      d1 <- diff(time)
      ld <- lead(time)
      f1 <- first(time)
      la <- last(time)
      lz <- lag(time, 0)
      d2 <- diff(time, 2)
      d0 <- diff0(time)
      ld2 <- lead(time, 2)
      ld0 <- lead0(time)
      l02 <- lag0(time, 2)
    })
    ev <- et(c(0, 1, 3, 7)) |> et(id = 1:2)
    r <- rxSolve(m, ev, returnType = "data.frame")
    for (.id in 1:2) {
      .s <- r[r$id == .id, ]
      expect_equal(.s$l1, c(NA, 0, 1, 3))
      expect_equal(.s$l2, c(NA, 0, 1, 3))
      expect_equal(.s$l3, c(NA, NA, 0, 1))
      expect_equal(.s$l0, c(0, 0, 1, 3))
      expect_equal(.s$d1, c(NA, 1, 2, 4))
      expect_equal(.s$ld, c(1, 3, 7, NA))
      expect_equal(.s$f1, rep(0, 4))
      expect_equal(.s$la, rep(7, 4))
      expect_equal(.s$lz, c(0, 1, 3, 7))
      expect_equal(.s$d2, c(NA, NA, 3, 6))
      expect_equal(.s$d0, c(0, 1, 2, 4))
      expect_equal(.s$ld2, c(3, 7, NA, NA))
      expect_equal(.s$ld0, c(1, 3, 7, 0))
      expect_equal(.s$l02, c(0, 0, 0, 1))
    }
    expect_true(grepl("l1=lag(t);", rxNorm(m), fixed = TRUE))
    expect_true(grepl("lz=t;", rxNorm(m), fixed = TRUE))
  })

  test_that("lag(x, 0) normalizes to x", {
    expect_true(grepl("l=cv;", rxNorm(rxode2("cv=x;l=lag(cv,0)")), fixed = TRUE))
  })

  test_that("lag() of a reserved name is a syntax error, not a crash (#1434)", {
    expect_error(rxode2("y = lag(amt)"), "syntax errors")
    expect_error(rxode2("y = lag(tlast)"), "syntax errors")
    expect_error(rxode2("y = diff(M_PI)"), "syntax errors")
  })
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
})
