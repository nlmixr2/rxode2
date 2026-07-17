rxTest({
  test_that("fixef()", {
    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd) + dnorm()
      })
    }

    expect_equal(nlme::fixef(one.cmt),
                 c(tka = 0.45, tcl = 0.993251773010283, tv = 3.45, add.sd = 0.7))

    f <- one.cmt()

    expect_equal(nlme::fixef(f),
                 c(tka = 0.45, tcl = 0.993251773010283, tv = 3.45, add.sd = 0.7))
  })

  test_that("coef()", {
    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd) + dnorm()
      })
    }

    f <- one.cmt()

    .theta <- c(tka = 0.45, tcl = 0.993251773010283, tv = 3.45, add.sd = 0.7)

    # default and explicit fixed levels return theta
    expect_equal(coef(f), .theta)
    expect_equal(coef(f, level = "theta"), .theta)
    expect_equal(coef(f, level = "fixed"), .theta)
    # coef() also works on the model function directly
    expect_equal(coef(one.cmt), .theta)

    # random-effect level returns the omega matrix
    expect_equal(coef(f, level = "omega"), f$omega)
    expect_equal(coef(f, level = "random"), f$omega)

    # all/both returns a list of fixed and random effects
    .all <- coef(f, level = "all")
    expect_equal(.all$theta, .theta)
    expect_equal(.all$omega, f$omega)
    expect_equal(coef(f, level = "both"), .all)

    # invalid level is an error
    expect_error(coef(f, level = "matt"))

    # models without random effects return NULL for omega
    no.re <- function() {
      ini({
        tka <- 0.5
      })
      model({
        ka <- tka
        d/dt(depot) <- -ka * depot
      })
    }
    expect_null(coef(no.re(), level = "omega"))
  })
})
