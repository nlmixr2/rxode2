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

f <- one.cmt()

cov <- function() {
  ini({
    tka <- 0.45
    tcl <- log(c(0, 2.7, 100))
    tv <- 3.45; label("log V")
    tvp <- 3.45; label("log Vp")
    cl.wt <- 0.1
    v.wt <- 0.1
    cl.sex <- 0.1
    v.sex <- 0.1
    cl.age <- 0.1
    v.age <- 0.1
    vp.wt <- 1
    vp.sex <- 1
    vp.age <- 1
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


test_that("rxode2 rxLhs works with ui objects", {
  expect_equal(rxLhs(one.cmt), c("ka", "cl", "v"))
  expect_equal(rxLhs(f), c("ka", "cl", "v"))
  expect_equal(rxLhs(cov), c("ka", "cl", "v", "vp", "cp"))
  expect_equal(rxLhs(f2), c("ka", "cl", "v", "vp", "cp"))
})


test_that("rxode2 rxParams works with ui objects", {
  expect_equal(rxParams(one.cmt), c("tka", "eta.ka", "tcl", "eta.cl", "tv", "eta.v"))
  expect_equal(rxParams(f), c("tka", "eta.ka", "tcl", "eta.cl", "tv", "eta.v"))
  expect_equal(rxParams(cov), c("tka", "eta.ka", "tcl", "eta.cl", "wt", "cl.wt", "sex", "cl.sex", "age", "cl.age", "tv", "eta.v", "v.wt", "v.sex", "v.age", "tvp", "vp.wt", "vp.sex", "vp.age"))
  expect_equal(rxParams(f2), c("tka", "eta.ka", "tcl", "eta.cl", "wt", "cl.wt", "sex", "cl.sex", "age", "cl.age", "tv", "eta.v", "v.wt", "v.sex", "v.age", "tvp", "vp.wt", "vp.sex", "vp.age"))
})

test_that("rxInit works with ui obects", {
  expect_equal(rxInits(one.cmt), structure(numeric(0), .Names = character(0)))
  expect_equal(rxInits(f), structure(numeric(0), .Names = character(0)))
  expect_equal(rxInits(cov), structure(numeric(0), .Names = character(0)))
  expect_equal(rxInits(f2), structure(numeric(0), .Names = character(0)))
})


test_that("data frame doesn't work by itself", {
  matt <- data.frame(a=3)
  expect_error(rxModelVars(matt))
})

