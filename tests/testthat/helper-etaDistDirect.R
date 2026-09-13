## Models shared by test-etaDistDirect.R.  A helper because testthat gives each
## test file its own frame, so a model defined in one file is invisible in
## another.
.edDirectModel <- function() {
  (function() {
    ini({
      lclm <- 1.63; lclrv <- 0.693; lv <- 1.55
      eta.cl ~ 1
      dist(eta.cl) ~ dgamma(shape = 1/exp(lclrv),
                            rate = 1/(exp(lclrv) * exp(lclm)))
      prop.sd <- 0.316
    })
    model({ cl <- eta.cl; v <- exp(lv); linCmt() ~ prop(prop.sd) })
  })()
}
.edDirectCorModel <- function() {
  (function() {
    ini({
      lclm <- 1.63; lv1m <- 1.55; lclrv <- -2.4; lv1rv <- -2.4
      eta.cl + eta.v1 ~ c(1, 0.5, 1)
      dist(eta.cl) ~ dgamma(shape = 1/exp(lclrv),
                            rate = 1/(exp(lclrv) * exp(lclm)))
      dist(eta.v1) ~ dgamma(shape = 1/exp(lv1rv),
                            rate = 1/(exp(lv1rv) * exp(lv1m)))
      prop.sd <- 0.1
    })
    model({ cl <- eta.cl; v <- eta.v1; linCmt() ~ prop(prop.sd) })
  })()
}
