# Make sure the right state is ignored.
## https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf p11

## 6.1
mod <- rxode2({
  a <- 6
  b <- 0.6
  d / dt(intestine) <- -a * intestine
  d / dt(blood) <- a * intestine - b * blood
})

test_that("Nothing ignored", {
  expect_equal(rxModelVars(mod)$state.ignore, c(0L, 0L))
})


.rxWithOptions(list(rxode2.syntax.require.ode.first = FALSE), {
  mod <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })
  
  mod.f <- rxode2({
    a <- 6
    b <- 0.6
    f(intestine) <- 1
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })
  
  mod.alag <- rxode2({
    a <- 6
    b <- 0.6
    alag(intestine) <- 0
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })
  
  mod.rate <- rxode2({
    a <- 6
    b <- 0.6
    rate(intestine) <- 2
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })
  
  
  mod.dur <- rxode2({
    a <- 6
    b <- 0.6
    dur(intestine) <- 2
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })
  
  test_that("Ignore first comparment", {
    expect_equal(rxModelVars(mod)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.f)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.alag)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.rate)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.dur)$state.ignore, c(1L, 0L))
  })
  
  mod <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.f <- rxode2({
    a <- 6
    b <- 0.6
    f(blood) <- 1
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.alag <- rxode2({
    a <- 6
    b <- 0.6
    alag(blood) <- 0
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.rate <- rxode2({
    a <- 6
    b <- 0.6
    rate(blood) <- 2
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.dur <- rxode2({
    a <- 6
    b <- 0.6
    dur(blood) <- 2
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  test_that("Nothing ignored", {
    expect_equal(rxModelVars(mod)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.f)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.f)$state, c("blood", "intestine"))
    expect_equal(rxModelVars(mod.alag)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.alag)$state, c("blood", "intestine"))
    expect_equal(rxModelVars(mod.rate)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.rate)$state, c("blood", "intestine"))
    expect_equal(rxModelVars(mod.dur)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.dur)$state, c("blood", "intestine"))
  })
  
  mod.f <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    f(blood) <- 1
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.alag <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    alag(blood) <- 0
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.rate <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    rate(blood) <- 2
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  mod.dur <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    dur(blood) <- 2
    d / dt(blood) ~ a * intestine - b * blood
  })
  
  
  test_that("Ignore Compartment #2", {
    expect_equal(rxModelVars(mod)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.f)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.f)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.alag)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.alag)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.rate)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.rate)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.dur)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.dur)$state, c("intestine", "blood"))
  })
})



.rxWithOptions(list(rxode2.syntax.require.ode.first = TRUE), {
  test_that("errors usually occur", {
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      d / dt(intestine) <- -a * intestine
      alag(blood) <- 0
      d / dt(blood) ~ a * intestine - b * blood
    }))
    
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      d / dt(intestine) <- -a * intestine
      rate(blood) <- 2
      d / dt(blood) ~ a * intestine - b * blood
    }))
    
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      d / dt(intestine) <- -a * intestine
      dur(blood) <- 2
      d / dt(blood) ~ a * intestine - b * blood
    }))
    
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      f(blood) <- 1
      d / dt(intestine) <- -a * intestine
      d / dt(blood) ~ a * intestine - b * blood
    }))
    
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      alag(blood) <- 0
      d / dt(intestine) <- -a * intestine
      d / dt(blood) ~ a * intestine - b * blood
    }))
    
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      rate(blood) <- 2
      d / dt(intestine) <- -a * intestine
      d / dt(blood) ~ a * intestine - b * blood
    }))
    
    expect_error(rxode2({
      a <- 6
      b <- 0.6
      dur(blood) <- 2
      d / dt(intestine) <- -a * intestine
      d / dt(blood) ~ a * intestine - b * blood
    }))
  })
})
