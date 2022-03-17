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

f <- one.cmt()

cov <- function() {
  ini({
    ## You may label each parameter with a comment
    tka <- 0.45 # Log Ka
    tcl <- log(c(0, 2.7, 100)) # Log Cl
    ## This works with interactive models
    ## You may also label the preceding line with label("label text")
    tv <- 3.45; label("log V")
    tvp <- 3.45; label("log V")
    cl.wt <- 0.1
    v.wt <- 0.1
    cl.sex <- 0.1
    v.sex <- 0.1
    cl.age <- 0.1
    v.age <- 0.1
    vp.wt <- 1
    vp.sex <- 1
    vp.age <- 1
    ## the label("Label name") works with all models
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ add(add.sd)
  })
}

f2 <- cov()

test_that("rxode2 model variables work with ui objects", {
  expect_s3_class(rxModelVars(one.cmt), "rxModelVars")
  expect_s3_class(rxModelVars(f), "rxModelVars")
  expect_s3_class(rxModelVars(cov), "rxModelVars")
  expect_s3_class(rxModelVars(f2), "rxModelVars")
})

test_that("rxode2 rxState works with ui objects", {
  expect_equal(rxState(one.cmt), character(0))
  expect_equal(rxState(f), character(0))
  expect_equal(rxState(cov), c("depot", "center"))
  expect_equal(rxState(f2), c("depot", "center"))
})


