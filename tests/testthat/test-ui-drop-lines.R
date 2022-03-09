.rx <- loadNamespace("rxode2")


test_that("drop support functions", {
  expect_equal(.rx$.getModelLineEquivalentLhsExpression(quote(-cl)), quote(cl))

  expect_true(.rx$.isDropExpression(quote(-v)))
  expect_false(.rx$.isDropExpression(quote(-v+3)))
  expect_false(.rx$.isDropExpression(quote(-3)))

  expect_false(.rx$.isDropExpression(quote(x <- y)))
  expect_false(.rx$.isDropExpression(quote(x + y ~ c(1, 0.01, 1))))
})

test_that("drop from model before single endpoint model", {

  one.compartment <- function() {
    ini({
      tka <- 0.45 ; label("Log Ka")
      tcl <- 1 ; label("Log Cl")
      tv <- 3.45 ; label("Log V")
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.err <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      f(depot) <- 3
      cp <- center / v
      cp2 <- cp * cl
      cp ~ add(add.err)
    })
  }

  f2 <- one.compartment %>% model(-cp2)

  expect_equal(f2$lstExpr[[8]], quote(cp ~ add(add.err)))
  expect_length(f2$lstExpr, 8L)

})



test_that("drop from model after single endpoint model", {

  one.compartment <- function() {
    ini({
      tka <- 0.45 ; label("Log Ka")
      tcl <- 1 ; label("Log Cl")
      tv <- 3.45 ; label("Log V")
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.err <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      f(depot) <- 3
      cp <- center / v
      cp ~ add(add.err)
      cp2 <- cp * cl
    })
  }

  f2 <- one.compartment %>% model(-cp2)
  expect_equal(f2$lstExpr[[8]], quote(cp ~ add(add.err)))
  expect_length(f2$lstExpr, 8L)

})



test_that("drop endpoint from  multiple endpoint model", {

  pk.turnover.emax <- function() {
    ini({
      tktr <- log(1)
      tka <- log(1)
      tcl <- log(0.1)
      tv <- log(10)
      ##
      eta.ktr ~ 1
      eta.ka ~ 1
      eta.cl ~ 2
      eta.v ~ 1
      prop.err <- 0.1
      pkadd.err <- 0.1
      ##
      temax <- logit(0.8)
      #temax <- 7.5
      tec50 <- log(0.5)
      tkout <- log(0.05)
      te0 <- log(100)
      ##
      eta.emax ~ .5
      eta.ec50  ~ .5
      eta.kout ~ .5
      eta.e0 ~ .5
      ##
      pdadd.err <- 10
    })
    model({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      ##
      #poplogit = log(temax/(1-temax))
      emax=expit(temax+eta.emax)
      #logit=temax+eta.emax
      ec50 =  exp(tec50 + eta.ec50)
      kout = exp(tkout + eta.kout)
      e0 = exp(te0 + eta.e0)
      ##
      DCP = center/v
      PD=1-emax*DCP/(ec50+DCP)
      ##
      effect(0) = e0
      kin = e0*kout
      ##
      d/dt(depot) = -ktr * depot
      d/dt(gut) =  ktr * depot -ka * gut
      d/dt(center) =  ka * gut - cl / v * center
      d/dt(effect) = kin*PD -kout*effect
      ##
      cp = center / v
      cp ~ prop(prop.err) + add(pkadd.err)
      effect ~ add(pdadd.err)
    })
  }

  f2 <- pk.turnover.emax %>% model(-cp)

  expect_length(f2$predDf$cond, 1)
  expect_equal(f2$predDf$cond, "effect")
  expect_length(f2$lstExpr, 17)

})
