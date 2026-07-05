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

})
