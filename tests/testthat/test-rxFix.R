test_that("rxFix", {

  One.comp.transit.allo <- function() {
    ini({
      # Where initial conditions/variables are specified
      lktr <- log(1.15)  #log k transit (/h)
      lcl  <- log(0.15)  #log Cl (L/hr)
      lv   <- log(7)     #log V (L)
      ALLC <- fix(0.75)  #allometric exponent cl
      ALLV <- fix(1.00)  #allometric exponent v
      prop.err <- 0.15   #proportional error (SD/mean)
      add.err <- 0.6     #additive error (mg/L)
      eta.ktr ~ 0.5
      eta.cl ~ 0.1
      eta.v ~ 0.1
    })
    model({
      #Allometric scaling on weight
      cl <- exp(lcl + eta.cl + ALLC * logWT70)
      v  <- exp(lv + eta.v + ALLV * logWT70)
      ktr <- exp(lktr + eta.ktr)
      # RxODE-style differential equations are supported
      d/dt(depot)   = -ktr * depot
      d/dt(central) =  ktr * trans - (cl/v) * central
      d/dt(trans)   =  ktr * depot - ktr * trans
      ## Concentration is calculated
      cp = central/v
      # And is assumed to follow proportional and additive error
      cp ~ prop(prop.err) + add(add.err)
    })
  }

  tmp <- rxFixPop(One.comp.transit.allo)

  expect_equal(tmp$theta,
               c(lktr = 0.139761942375159, lcl = -1.89711998488588, lv = 1.94591014905531,
                 prop.err = 0.15, add.err = 0.6))

  tmp2 <- rxFixPop(tmp)

  expect_equal(tmp2$theta,
               c(lktr = 0.139761942375159, lcl = -1.89711998488588, lv = 1.94591014905531,
                 prop.err = 0.15, add.err = 0.6))

  tmp3 <- rxFixPop(tmp2, returnNull = TRUE)

  expect_true(is.null(tmp3))

})
