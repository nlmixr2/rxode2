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

})
