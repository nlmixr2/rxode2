rxTest({
  test_that("a covariate named value gets an informative error (#1386)", {
    d <- data.frame(ID = 1L, TIME = 1:4, value = c(1, 2, 3, 4))
    m <- function() {
      ini({
        a <- 1
      })
      model({
        y <- a * value
      })
    }
    expect_error(rxSolve(m, d), "'value' is read as the 'amt' alias")
    s <- rxSolve(m, transform(d, AMT = 0), returnType = "data.frame")
    expect_equal(s$y, d$value)
  })

  test_that("an upper-case VALUE covariate gets the same error (#1386)", {
    d <- data.frame(ID = 1L, TIME = 1:4, VALUE = c(1, 2, 3, 4))
    m <- function() {
      ini({
        a <- 1
      })
      model({
        y <- a * VALUE
      })
    }
    expect_error(rxSolve(m, d), "'VALUE' is read as the 'amt' alias")
    s <- rxSolve(m, transform(d, AMT = 0), returnType = "data.frame")
    expect_equal(s$y, d$VALUE)
  })

  test_that("a value covariate passes through wherever the amt column is (#1386)", {
    m <- function() {
      ini({
        a <- 1
      })
      model({
        y <- a * value
      })
    }
    d1 <- data.frame(ID = 1L, TIME = 1:4, AMT = 0, value = c(1, 2, 3, 4))
    d2 <- d1[, c("ID", "TIME", "value", "AMT")]
    expect_equal(rxSolve(m, d1, returnType = "data.frame")$y, d1$value)
    expect_equal(rxSolve(m, d2, returnType = "data.frame")$y, d1$value)
  })

  test_that("value is still the amt alias when it is not a covariate (#1386)", {
    m <- function() {
      ini({
        ka <- 1
        kel <- 0.1
      })
      model({
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - kel * central
      })
    }
    dAmt <- data.frame(ID = 1L, TIME = c(0, 1, 2), EVID = c(1, 0, 0), CMT = 1, AMT = c(100, 0, 0))
    dValue <- dAmt
    names(dValue)[5] <- "value"
    sAmt <- rxSolve(m, dAmt, returnType = "data.frame")
    sValue <- rxSolve(m, dValue, returnType = "data.frame")
    expect_equal(sValue$central, sAmt$central)
    expect_true(all(sAmt$central[-1] > 0))
  })

  test_that("a value covariate with an EVID column still solves (#1386)", {
    d <- data.frame(ID = 1L, TIME = 1:4, EVID = 0L, value = c(1, 2, 3, 4))
    m <- function() {
      ini({
        a <- 1
      })
      model({
        y <- a * value
      })
    }
    s <- rxSolve(m, d, returnType = "data.frame")
    expect_equal(s$y, d$value)
  })
})
