
#devtools::install("~/src/rxode2")

library(nlmixr2)

## one.cmt <- function() {
##   ini({
##     ## You may label each parameter with a comment
##     tka <- 0.45 # Log Ka
##     tcl <- log(c(0, 2.7, 100)) # Log Cl
##     ## This works with interactive models
##     ## You may also label the preceding line with label("label text")
##     tv <- 3.45; label("log V")
##     ## the label("Label name") works with all models
##     eta.ka ~ 0.6
##     eta.cl ~ 0.3
##     eta.v ~ 0.1
##     add.sd <- 0.7
##   })
##   model({
##     ka <- exp(tka + eta.ka)
##     cl <- exp(tcl + eta.cl)
##     v <- exp(tv + eta.v)
##     linCmt() ~ add(add.sd)
##   })
## }

## fit.c3 <- nlmixr(one.cmt, theo_sd, est="focei",
##                  control=foceiControl(rxControl=rxControl(linCmtSensType="forward3")))

## fit <- nlmixr(one.cmt, theo_sd, est="focei")

## fit.ad <- nlmixr(one.cmt, theo_sd, est="focei",
##  control=foceiControl(rxControl=rxControl(linCmtSensType="AD")))

## fit.c5 <- nlmixr(one.cmt, theo_sd, est="focei",
##   control=foceiControl(rxControl=rxControl(linCmtSensType="endpoint5")))


## f <- function(){
##   ini({ # Where initial conditions/variables are specified
##     # '<-' or '=' defines population parameters
##     # Simple numeric expressions are supported
##     lCl <- 1.8      #log Cl (L/hr)
##     lVc <- 4.7      #log V (L)
##     lKA <- 0.2      #log V (L)
##     # Bounds may be specified by c(lower, est, upper), like NONMEM:
##     # Residuals errors are assumed to be population parameters
##     prop.err <- c(0, 0.3, 1)
##     # Between subject variability estimates are specified by '~'
##     # Semicolons are optional
##     eta.Cl ~ 0.15
##     eta.Vc ~ 0.15
##     eta.KA ~ 0.15
##   })
##   model({ # Where the model is specified
##     # The model uses the ini-defined variable names
##     Cl <- exp(lCl + eta.Cl)
##     Vc <- exp(lVc + eta.Vc)
##     KA <- exp(lKA + eta.KA)
##     cp <- linCmt()
##     cp ~ prop(prop.err)
##   })
## }

## datr <- nlmixr2data::Oral_1CPT
## dat <- datr[datr$SD == 1, ]
## dat <- dat[, names(dat) != "SS"]

## fit1 <- nlmixr2(f, dat, est="focei")

## fit1.c3 <- nlmixr2(f, dat, est="focei",
##                    control=foceiControl(rxControl=rxControl(linCmtSensType="forward3")))

## fit1.c5 <- nlmixr2(f, dat, est="focei",
##                    control=foceiControl(rxControl=rxControl(linCmtSensType="endpoint5")))

## fit1.ad <- nlmixr2(f, dat, est="focei",  control=foceiControl(rxControl=rxControl(linCmtSensType="AD")))

## c(fit1=sum(fit1$time),
##   fit1.c3=sum(fit1.c3$time),
##   fit1.c5=sum(fit1.c5$time),
##   fit1.ad=sum(fit1.ad$time))


f <- function() {
  ini({ # Where initial conditions/variables are specified
    # '<-' or '=' defines population parameters
    lCl <- 1.6      #log Cl (L/hr)
    lVc <- 4.5      #log Vc (L)
    lQ  <- 1.6      #log Q (L/hr)
    lVp <- 4        #log Vp (L)
    lKA <- 0.2      #log V (L)
    # Bounds may be specified by c(lower, est, upper), like NONMEM:
    # Residuals errors are assumed to be population parameters
    prop.err <- c(0, 0.3, 1)
    # Between subject variability estimates are specified by '~'
    # Semicolons are optional
    eta.Vc ~ 0.15
    eta.Cl ~ 0.15
    eta.Vp ~ 0.15
    eta.Q  ~ 0.15
    eta.KA ~ 0.15
  })
  model({ # Where the model is specified
    # The model uses the ini-defined variable names
    Vc <- exp(lVc + eta.Vc)
    Cl <- exp(lCl + eta.Cl)
    Vp <- exp(lVp + eta.Vp)
    Q  <- exp(lQ + eta.Q)
    KA <- exp(lKA + eta.KA)
    # And is assumed to follow proportional error estimated by prop.err
    cp <- linCmt()
    cp ~ prop(prop.err)
  })
}

datr <- nlmixr2data::Oral_2CPT
dat <- datr[datr$SD == 1, ]
dat <- dat[, names(dat) != "SS"]

## fit2 <- nlmixr2(f, dat, est="focei")

## fit2.ad <- nlmixr2(f, dat, est="focei",
##  control=foceiControl(rxControl=rxControl(linCmtSensType="AD")))

## fit2.c3 <- nlmixr2(f, dat, est="focei",
##                    control=foceiControl(rxControl=rxControl(linCmtSensType="forward3")))

## fit2.c <- nlmixr2(f, dat, est="focei",
##                   control=foceiControl(rxControl=rxControl(linCmtSensType="central")))

## fit2.ch <- nlmixr2(f, dat, est="focei",
##                   control=foceiControl(rxControl=rxControl(linCmtSensType="centralH")))

fit2.f <- nlmixr2(f, dat, est="focei",
                  control=foceiControl(rxControl=rxControl(linCmtSensType="forward"),
                                       outerOpt="lbfgsb3c"))

fit2.f3 <- nlmixr2(f, dat, est="focei",
                   control=foceiControl(rxControl=rxControl(linCmtSensType="forward3"),
                                        outerOpt="nlminb"))


fit2.fg <- nlmixr2(f, dat, est="focei",                  control=foceiControl(rxControl=rxControl(linCmtSensType="forwardG"), outerOpt="lbfgsb3c"))

fit2n.f <- nlmixr2(f, dat, est="focei",
                  control=foceiControl(rxControl=rxControl(linCmtSensType="forward"),
                                       outerOpt="nlminb"))

fit2n.fg <- nlmixr2(f, dat, est="focei",                  control=foceiControl(rxControl=rxControl(linCmtSensType="forwardG"), outerOpt="nlminb"))

## fit2.c <- nlmixr2(f, dat, est="focei",                  control=foceiControl(rxControl=rxControl(linCmtSensType="central")))


#fit2 <- nlmixr2(f, dat, est="focei")


## fit2.c5 <- nlmixr2(f, dat, est="focei",
##  control=foceiControl(rxControl=rxControl(linCmtSensType="endpoint5")))

## c(fit2=sum(fit2$time),
##   fit2.c3=sum(fit2.c3$time),
##   fit2.c5=sum(fit2.c5$time),
##   fit2.ad=sum(fit2.ad$time))

## ## fit <- nlmixr2(f, dat, est="focei")


## f <- function() {
##   ini({ # Where initial conditions/variables are specified
##     # '<-' or '=' defines population parameters
##     lCl <- 1.6      #log Cl (L/hr)
##     lVc <- 4.5      #log Vc (L)
##     lQ  <- 1.6      #log Q (L/hr)
##     lVp <- 4        #log Vp (L)
##     lQ2 <- 2
##     lVp2 <- 6
##     lKA <- 0.2      #log V (L)
##     # Bounds may be specified by c(lower, est, upper), like NONMEM:
##     # Residuals errors are assumed to be population parameters
##     prop.err <- c(0, 0.3, 1)
##     # Between subject variability estimates are specified by '~'
##     # Semicolons are optional
##     eta.Vc ~ 0.15
##     eta.Cl ~ 0.15
##     eta.KA ~ 0.15
##   })
##   model({ # Where the model is specified
##     # The model uses the ini-defined variable names
##     Vc <- exp(lVc + eta.Vc)
##     Cl <- exp(lCl + eta.Cl)
##     Vp <- exp(lVp)
##     Q  <- exp(lQ)
##     KA <- exp(lKA + eta.KA)
##     Q2 <- exp(lQ2)
##     Vp2 <- exp(lVp2)
##     # And is assumed to follow proportional error estimated by prop.err
##     cp <- linCmt()
##     cp ~ prop(prop.err)
##   })
## }


## fit3 <- nlmixr2(f, dat, est="focei")

## fit3.ad <- nlmixr2(f, dat, est="focei",
##  control=foceiControl(rxControl=rxControl(linCmtSensType="AD")))

## fit3.c3 <- nlmixr2(f, dat, est="focei",
##  control=foceiControl(rxControl=rxControl(linCmtSensType="forward3")))

## fit3.c5 <- nlmixr2(f, dat, est="focei",
##  control=foceiControl(rxControl=rxControl(linCmtSensType="endpoint5")))


## c(fit1=sum(fit1$time),
##   fit1.c3=sum(fit1.c3$time),
##   fit1.c5=sum(fit1.c5$time),
##   fit1.ad=sum(fit1.ad$time))

## c(fit2=sum(fit2$time),
##   fit2.c3=sum(fit2.c3$time),
##   fit2.c5=sum(fit2.c5$time),
##   fit2.ad=sum(fit2.ad$time))

## c(fit3=sum(fit3$time),
##   fit3.c3=sum(fit3.c3$time),
##   fit3.c5=sum(fit3.c5$time),
##   fit3.ad=sum(fit3.ad$time))
