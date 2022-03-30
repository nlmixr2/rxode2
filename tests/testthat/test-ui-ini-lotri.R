test_that("ini will strip covariances and renumber if needed", {

  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45; label("log V")
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

  f <- rxode2(one.cmt) %>%
    ini(eta.v + eta.ka + eta.cl ~ c(1,
                                    0.01, 1,
                                    -0.01, 0.01, 1))

  expect_equal(dimnames(f$omega)[[1]],
               c("eta.v", "eta.ka", "eta.cl"))


  f <- rxode2(one.cmt) %>%
    ini(eta.v + eta.cl ~ c(1,
                           0.01, 1))

  expect_equal(dimnames(f$omega)[[1]],
               c("eta.ka", "eta.v", "eta.cl"))

  expect_message(expect_message(
    f2 <-
      f %>%
      ini(eta.v + eta.cl ~ c(1,
                             0.01, 1)),
    regexp="some correlations may have been dropped for the variables: `eta.v`, `eta.cl`"),
    regexp="the piping should specify the needed covariances directly"
  )
})
