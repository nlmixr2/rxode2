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

    f <- f()
    expect_equal(f$mixProbs, c("p1", "p2"))
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

  test_that("mix() requires the same probabilities in each proportion", {

    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, p2, b2, p1, b3)
      })
    }

    expect_error(f())

    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, p1, b2, p2, b3)
      })
    }

    expect_error(f(), NA)

    f <- f()

    # Gets the probabilities
    expect_equal(f$mixProbs, c("p1", "p2"))

    # Throws error that this is a mixed model
    expect_error(assertRxUiNoMix(f))

    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    expect_error(assertRxUiNoMix(one.cmt), NA)

  })

  test_that("test dsl to change mix()", {
    expect_equal(rxToSE("mix(a1, p1, b)"),
                 "(rxEq(mixest, 1)*(a1)+rxEq(mixest, 2)*(b))")

    expect_equal(rxToSE("mix(a1, p1, b, p2, c)"),
                 "(rxEq(mixest, 1)*(a1)+rxEq(mixest, 2)*(b)+rxEq(mixest, 3)*(c))")
  })


})
