rxTest({
  test_that("mix() in normal rxode2 model", {
    expect_error(rxModelVars("a = mix(a)"))
    expect_error(rxModelVars("a = mix(a, b)"))
    expect_error(rxModelVars("a = mix(a, b, c)"), NA)
    expect_error(rxModelVars("a = mix(a, b, c, d)"))
    expect_error(rxModelVars("a = mix(a, p1, c); c = mix(a, p1, c, p2, e)"))
    expect_error(rxModelVars("a = mix(a, p1, c); c = mix(a, p1, d)"), NA)
  })

  test_that("good mix() in ui model", {
    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
        eta.a ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a
        b <- mix(b1, b2, b3)
      })
    }

    expect_error(rxModelVars(f()), NA)
  })

  test_that("mix() requires between subject variability", {
    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
      })
      model({
        a <- mix(a1, p1, a2, p2, a3)
        b <- mix(b1, b2, b3)
      })
    }
    expect_error(f())
  })

  test_that("mix() requires require probabilities in the ini block", {
    f <- function() {
      ini({
        a1 <- 0.1
        a2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, b2, b3)
      })
    }

    expect_error(f())
  })

  test_that("mix() requires require probabilities to sum to a number between 0 and 1", {

    f <- function() {
      ini({
        p1 <- -10
        p2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, b2, b3)
      })
    }

    expect_error(f())
  })


})
