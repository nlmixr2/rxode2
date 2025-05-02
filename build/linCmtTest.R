

## devtools::load_all("~/src/rxode2")

## devtools::load_all("~/src/nlmixr2est")
library(nlmixr2)

f <- function() {
  ini({ # Where initial conditions/variables are specified
    # '<-' or '=' defines population parameters
    # Simple numeric expressions are supported
    lCl <- 1.6      #log Cl (L/hr)
    lVc <- 4.5      #log V (L)
    # Bounds may be specified by c(lower, est, upper), like NONMEM:
    # Residuals errors are assumed to be population parameters
    prop.err <- c(0, 0.3, 1)
    # Between subject variability estimates are specified by '~'
    # Semicolons are optional
    eta.Vc ~ 0.1   #IIV V
    eta.Cl ~ 0.1   #IIV Cl
  })
  model({ # Where the model is specified
    # The model uses the ini-defined variable names
    Vc <- exp(lVc + eta.Vc)
    Cl <- exp(lCl + eta.Cl)
    cp <- linCmt()
    cp ~ prop(prop.err)
  })
}

datr <- nlmixr2data::Bolus_1CPT
dat <- datr[datr$SD == 1, ]
dat <- dat[, names(dat) != "SS"]

fit <- nlmixr2(f, dat, est="focei")


## f2 <- function() {
##   ini({ # Where initial conditions/variables are specified
##     # '<-' or '=' defines population parameters
##     # Simple numeric expressions are supported
##     lCl <- 1.6      #log Cl (L/hr)
##     lVc <- 4.5      #log V (L)
##     # Bounds may be specified by c(lower, est, upper), like NONMEM:
##     # Residuals errors are assumed to be population parameters
##     prop.err <- c(0, 0.3, 1)
##     # Between subject variability estimates are specified by '~'
##     # Semicolons are optional
##     eta.Vc ~ 0.1   #IIV V
##     eta.Cl ~ 0.1   #IIV Cl
##   })
##   model({ # Where the model is specified
##     # The model uses the ini-defined variable names
##     Vc <- exp(lVc + eta.Vc)
##     Cl <- exp(lCl + eta.Cl)
##     d/dt(central) <- -Cl/Vc * central
##     cp <- central/Vc
##     cp ~ prop(prop.err)
##   })
## }

## fit2 <- nlmixr2(f2, dat, est="focei")


## print(c("linear"=sum(fit$time), "ode"=sum(fit2$time)))


## theta <- setNames(fit$theta,paste0("THETA[", seq_along(fit$theta), "]"))

## eta <- fit$eta[,-1]
## names(eta) <- paste0("ETA[", seq_along(eta), "]")

## qs::qsave(list(theta, eta), "linCmtTest.qs")


## ###################################################################


## devtools::load_all("~/src/rxode2")

## datr <- nlmixr2data::Bolus_1CPT
## dat <- datr[datr$SD == 1, ]
## dat <- dat[, names(dat) != "SS"]


## p <- qs::qread("linCmtTest.qs")

## p <- qs::qread("../linCmtTest.qs")

## theta <- p[[1]]

## eta <- p[[2]]

## pars <- data.frame(as.data.frame(t(theta), check.names=FALSE),eta, check.names=FALSE)

## rx <- rxode2({
##   param(THETA[1], THETA[2], THETA[3], ETA[1], ETA[2])
##   rx_yj_ ~ 2
##   rx_lambda_ ~ 1
##   rx_hi_ ~ 1
##   rx_low_ ~ 0
##   rx_expr_0 ~ ETA[2] + THETA[1]
##   rx_expr_1 ~ ETA[1] + THETA[2]
##   rx_expr_2 ~ exp(rx_expr_0)
##   rx_expr_3 ~ exp(rx_expr_1)
##   rx_pred_ = linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, rx_expr_2,
##                      rx_expr_3, 0, 0, 0, 0, 0)
##   rx__sens_rx_pred__BY_ETA_1___ = rx_expr_3 * linCmtB(rx__PTR__,
##                                                       t, 1, 1, 0, -2, 1, 1, rx_expr_2, rx_expr_3, 0, 0, 0,
##                                                       0, 0)
##   rx__sens_rx_pred__BY_ETA_2___ = rx_expr_2 * linCmtB(rx__PTR__,
##                                                       t, 1, 1, 0, -2, 0, 1, rx_expr_2, rx_expr_3, 0, 0, 0,
##                                                       0, 0)
##   rx_r_ = Rx_pow_di((linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1,
##                              1, rx_expr_2, rx_expr_3, 0, 0, 0, 0, 0) * THETA[3]),
##                     2)
##   rx__sens_rx_r__BY_ETA_1___ = 2 * rx_expr_3 * (linCmtB(rx__PTR__,
##                                                         t, 1, 1, 0, -1, -1, 1, rx_expr_2, rx_expr_3, 0, 0, 0,
##                                                         0, 0) * THETA[3]) * linCmtB(rx__PTR__, t, 1, 1, 0, -2,
##                                                                                     1, 1, rx_expr_2, rx_expr_3, 0, 0, 0, 0, 0) * THETA[3]
##   rx__sens_rx_r__BY_ETA_2___ = 2 * rx_expr_2 * (linCmtB(rx__PTR__,
##                                                         t, 1, 1, 0, -1, -1, 1, rx_expr_2, rx_expr_3, 0, 0, 0,
##                                                         0, 0) * THETA[3]) * linCmtB(rx__PTR__, t, 1, 1, 0, -2,
##                                                                                     0, 1, rx_expr_2, rx_expr_3, 0, 0, 0, 0, 0) * THETA[3]
##   cmt(cp)
##   dvid(2)
## })


## tmp <- rxSolve(rx, pars, dat)
