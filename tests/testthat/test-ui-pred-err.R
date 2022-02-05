test_that("pred model errors", {

  fn1 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ pois()
    })
  }

  expect_error(rxode2(fn1), "linCmt()")

  fn2 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      Prop.Err2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ pow(Prop.Err, Prop.Err2)
    })
  }

  expect_error(rxode2(fn2), "Prop.Err")

  fn2 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      Prop.Err <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ pow(Prop.Err, Prop.Err2)
    })
  }

  expect_error(rxode2(fn2), "Prop.Err2")

  fn2 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      Prop.Err <- 1
      Prop.Err2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ pow(Prop.Err, Prop.Err2)
    })
  }

  expect_error(rxode2(fn2), NA)

  fn2 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      Prop.Err <- 1
      Prop.Err2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ prop(Prop.Err, Prop.Err2)
    })
  }

  expect_error(suppressMessages(rxode2(fn2)))


  fn3 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ prop()
    })
  }

  expect_error(rxode2(fn3))

  fn4 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      p1 <- 1
      p2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ pois(p1, p2)
    })
  }

  expect_error(suppressMessages(rxode2(fn4)))

  fn5 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ pois()
    })
  }

  expect_error(rxode2(fn5))

  fn6 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ dpois(1, 2)
    })
  }

  expect_error(rxode2(fn6))

  fn7 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      linCmt() ~ dbinom()
    })
  }

  expect_error(rxode2(fn7))

  fn8 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dbinom(13)
    })
  }

  expect_error(rxode2(f8))

  fn9 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dbinom(13, 0.5, 32)
    })
  }

  expect_error(rxode2(fn9))

  fn10 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dbeta()
    })
  }

  expect_error(rxode2(fn10))

  fn11 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dbeta(par1)
    })
  }

  expect_error(expect_message(rxode2(fn11)))

  fn12 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
      par3 <- 1
      par4 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dbeta(par1, par2, par3, par4)
    })
  }

  expect_error(expect_message(rxode2(fn12)))

  fn13 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dt()
    })
  }

  expect_error(expect_message(rxode2(fn13)))

  fn14 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dt(par1, par2, par3)
    })
  }

  expect_error(expect_message(rxode2(fn14)))

  fn15 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ binom()
    })
  }

  expect_error(expect_message(rxode2(fn15)))

  fn15 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dexp()
    })
  }

  expect_error(rxode2(fn15), NA)

  fn16 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ binom(par1)
    })
  }

  expect_error(rxode2(fn16), NA)

  fn17 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
      par3 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ binom(par1, par2, par3)
    })
  }

  expect_error(expect_message(rxode2(fn17)))

  fn18 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ beta()
    })
  }

  expect_error(rxode2(fn18))

  fn19 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ beta(par1)
    })
  }

  expect_error(expect_message(rxode2(fn19)))

  fn20 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
      par3 <- 1
      par4 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ beta(par1, par2, par3, par4)
    })
  }

  expect_error(expect_message(rxode2(fn20)))

  fn21 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ t()
    })
  }

  expect_error(rxode2(fn21))

  fn22 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
      par3 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ t(par1, par2, par3)
    })
  }

  expect_error(rxode2(fn22))

  fn23 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ add()
    })
  }

  expect_error(rxode2(fn23))

  fn24 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ add(par1, par2)
    })
  }

  expect_error(expect_message(rxode2(fn24)))

  fn25 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ prop()
    })
  }

  expect_error(rxode2(fn25))

  fn26 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ prop(par1, par2)
    })
  }

  expect_error(expect_message(rxode2(fn26)))

  fn27 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ norm()
    })
  }

  expect_error(rxode2(fn27))

  fn28 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ norm(par1, par2)
    })
  }

  expect_error(expect_message(rxode2(fn28)))

  fn29 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dnorm()
    })
  }

  expect_error(rxode2(fn29))

  fn30 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ dnorm(par1, par2)
    })
  }
  expect_error(rxode2(fn30))

  fn31 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ pois()
    })
  }
  expect_error(rxode2(fn31))

  fn32 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ add(par1) + pois(par2)
    })
  }

  expect_error(expect_message(rxode2(fn32)))

  fn33 <- function() {
    ini({
      tKA <- 1
      tCL <- 1
      eta.CL ~ 0.1
      eta.KA ~ 0.1
      eta.V ~ 0.1
      par1 <- 1
      par2 <- 1
    })
    model({
      KA <- tKA + eta.KA
      CL <- tCL + eta.CL
      V <- tV + eta.V
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      linCmt() ~ prop(par1) + add(par2) + pois(par3)
    })
  }

  expect_error(expect_message(rxode2(fn33)))

  fn1 <- function() {
    ini({
      KA <- c(0, 1)
      CL <- c(0, 0.5)
    })
    model({
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      v1 <- 1
      linCmt() ~ pois()
    })
  }

  expect_error(rxode2(fn1))

  fn34 <- function() {
    ini({
      KA <- c(0, 1)
      CL <- c(0, 0.5)
      par1 <- 1
      par2 <- 2
    })
    model({
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      v1 <- 1
      linCmt() ~ add(par1) + prop(par2)
    })
  }

  expect_error(rxode2(fn34), NA)

  fn35 <- function() {
    ini({
      KA <- c(0, 1)
      CL <- c(0, 0.5)
      par1 <- 1
      par2 <- 2
    })
    model({
      KA <- KA + eta.KA
      CL <- CL + eta.CL
      v1 <- 1
      linCmt() ~ prop(par1) + add(par2)
    })
  }

  expect_error(rxode2(fn35), NA)

})
