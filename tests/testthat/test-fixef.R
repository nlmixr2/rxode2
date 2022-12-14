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
})
