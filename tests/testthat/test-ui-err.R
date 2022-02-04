lmat <- lotri({
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
  pow.sd <- 0.2
  pow  <- 1
  lambda  <- 0.5
})

expect_err2 <- function(x, extra=FALSE) {
  if (is.na(extra)){
    expect_false(x$hasErrors)
  } else {
    expect_true(x$hasErrors)
  }
}

.rx <- loadNamespace("rxode2")

test_that("When checking for distributions, don't consider if the parameter is defined", {
  expect_true(.rx$.isErrorExpression(ipre ~ add(f2) + propF(prop.sd, f2)))
})

test_that("error when errors have too many arguments", {
  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ add(add.sd, pow1) + pow(pow.sd, pow2) + boxCox(lambda) | cond
  }), lmat))
})

test_that("error when adding distributions that do not support the additive notation", {
  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ dbinom(add.sd, pow) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat))
})

test_that("error for specifying distributions that have multiple numbers of arguments", {
  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    p <- pow
    l <- lamba
    cp ~ dbinom(add.sd, p, l)
    center ~ pow(pow.sd, p) + boxCox(l) | cond
  }), lmat))
})

test_that("error when adding algebraic expressions to known distributional abbreviations", {
  expect_error(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ add(add.sd) + pow(pow.sd, pow) + boxCox(lambda) + tan(vp)| cond
  }), lmat))
})

test_that("The distribution names will transform to the preferred distributions", {
  expect_equal(rxPreferredDistributionName("dnorm"), "add")
  expect_equal(rxPreferredDistributionName("add"), "add")
  expect_equal(rxPreferredDistributionName("logitNorm"), "logitNorm")
})

test_that("non-numeric bounds for logitNorm and probitNorm", {

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd, lower, upper) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat))

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat), NA)

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd, 0) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat), NA)

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd, 0, 5) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat), NA)

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd, -5, 5) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat), NA)

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd, -Inf, Inf) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat), NA)

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ probitNorm(add.sd, lower, upper) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat))

})

test_that("rxTransformCombine", {

  testCombine0 <- function(transforms) {
    .cmb <- rxErrTypeCombine("matt")
    for (i in transforms) {
      .cmb <- rxErrTypeCombine(.cmb, i)
    }
    .cmb
  }

  testCombine <- function(transforms) {
    .cmb <- testCombine0(transforms)
    .cmb2 <- testCombine0(rev(transforms))
    expect_equal(.cmb, .cmb2)
    .cmb
  }

  ## add + prop
  expect_equal(testCombine(c("add", "prop"))$errType,
               testCombine(c("add", "propT"))$errType)

  expect_equal(testCombine(c("add", "prop"))$errType,
               testCombine(c("add", "propF"))$errType)

  expect_equal(testCombine(c("logitNorm", "prop"))$errType,
               testCombine(c("add", "prop"))$errType)

  expect_equal(testCombine(c("logitNorm", "propT"))$errType,
               testCombine(c("add", "prop"))$errType)

  expect_equal(testCombine(c("logitNorm", "propF"))$errType,
               testCombine(c("add", "prop"))$errType)

  expect_equal(testCombine(c("probitNorm", "prop"))$errType,
               testCombine(c("add", "prop"))$errType)

  expect_equal(testCombine(c("probitNorm", "propT"))$errType,
               testCombine(c("add", "prop"))$errType)

  expect_equal(testCombine(c("probitNorm", "propF"))$errType,
               testCombine(c("add", "prop"))$errType)

  ## add + pow
  expect_equal(testCombine(c("add", "pow"))$errType,
               testCombine(c("add", "powF"))$errType)

  expect_equal(testCombine(c("lnorm", "pow"))$errType,
               testCombine(c("add", "pow"))$errType)

  expect_equal(testCombine(c("logitNorm", "pow"))$errType,
               testCombine(c("add", "pow"))$errType)

  expect_equal(testCombine(c("probitNorm", "pow"))$errType,
               testCombine(c("add", "pow"))$errType)

  # Test Error type F
  expect_equal(testCombine(c("add", "powF"))$errTypeF,
               testCombine(c("add", "propF"))$errTypeF)

  expect_equal(testCombine(c("add", "powT"))$errTypeF,
               testCombine(c("add", "propT"))$errTypeF)

  expect_equal(testCombine(c("add", "pow"))$errTypeF,
               testCombine(c("add", "prop"))$errTypeF)

  expect_equal(testCombine("powF")$errTypeF,
               testCombine("propF")$errTypeF)

  expect_equal(testCombine("powT")$errTypeF,
               testCombine("propT")$errTypeF)

  expect_equal(testCombine("pow")$errTypeF,
               testCombine("prop")$errTypeF)

  # Test Transformation Type
  expect_equal(testCombine(c("add", "prop", "boxCox"))$transform,
               testCombine(c("add", "propT", "boxCox"))$transform)

  expect_equal(testCombine(c("add", "prop", "boxCox"))$transform,
               testCombine(c("add", "propF", "boxCox"))$transform)

  expect_equal(testCombine(c("add", "pow", "boxCox"))$transform,
               testCombine(c("add", "propF", "boxCox"))$transform)

  expect_equal(testCombine(c("pow", "boxCox"))$transform,
               testCombine(c("add", "boxCox"))$transform)

  expect_equal(testCombine(c("prop", "boxCox"))$transform,
               testCombine(c("add", "boxCox"))$transform)

  expect_equal(testCombine(c("lnorm", "prop", "boxCox")),
               testCombine(c("lnorm", "propT", "boxCox")))

  expect_equal(testCombine(c("lnorm", "prop", "boxCox")),
               testCombine(c("lnorm", "propF", "boxCox")))

  expect_equal(testCombine(c("lnorm", "pow", "boxCox")),
               testCombine(c("lnorm", "propF", "boxCox")))

  expect_equal(testCombine(c("logitNorm", "prop"))$transform,
               testCombine(c("logitNorm", "propT"))$transform)

  expect_equal(testCombine(c("logitNorm", "add"))$transform,
               testCombine(c("logitNorm", "propT"))$transform)

  expect_equal(testCombine(c("logitNorm", "prop", "boxCox"))$transform,
               testCombine(c("logitNorm", "propT", "boxCox"))$transform)

  expect_equal(testCombine(c("logitNorm", "prop", "boxCox"))$transform,
               testCombine(c("logitNorm", "propF", "boxCox"))$transform)

  expect_equal(testCombine(c("logitNorm", "pow", "boxCox"))$transform,
               testCombine(c("logitNorm", "propF", "boxCox"))$transform)

  expect_equal(testCombine(c("probitNorm", "prop", "boxCox"))$transform,
               testCombine(c("probitNorm", "propT", "boxCox"))$transform)

  expect_equal(testCombine(c("probitNorm", "prop", "boxCox"))$transform,
               testCombine(c("probitNorm", "propF", "boxCox"))$transform)

  expect_equal(testCombine(c("probitNorm", "pow", "boxCox"))$transform,
               testCombine(c("probitNorm", "propF", "boxCox"))$transform)

  # Yeo Johnson
  expect_equal(testCombine(c("add", "prop", "yeoJohnson"))$transform,
               testCombine(c("add", "propT", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("add", "prop", "yeoJohnson"))$transform,
               testCombine(c("add", "propF", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("add", "pow", "yeoJohnson"))$transform,
               testCombine(c("add", "propF", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("pow", "yeoJohnson"))$transform,
               testCombine(c("add", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("prop", "yeoJohnson"))$transform,
               testCombine(c("add", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("lnorm", "prop", "yeoJohnson")),
               testCombine(c("lnorm", "propT", "yeoJohnson")))

  expect_equal(testCombine(c("lnorm", "prop", "yeoJohnson")),
               testCombine(c("lnorm", "propF", "yeoJohnson")))

  expect_equal(testCombine(c("lnorm", "pow", "yeoJohnson")),
               testCombine(c("lnorm", "propF", "yeoJohnson")))

  expect_equal(testCombine(c("logitNorm", "prop", "yeoJohnson"))$transform,
               testCombine(c("logitNorm", "propT", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("logitNorm", "prop", "yeoJohnson"))$transform,
               testCombine(c("logitNorm", "propF", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("logitNorm", "pow", "yeoJohnson"))$transform,
               testCombine(c("logitNorm", "propF", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("probitNorm", "prop", "yeoJohnson"))$transform,
               testCombine(c("probitNorm", "propT", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("probitNorm", "prop", "yeoJohnson"))$transform,
               testCombine(c("probitNorm", "propF", "yeoJohnson"))$transform)

  expect_equal(testCombine(c("probitNorm", "pow", "yeoJohnson"))$transform,
               testCombine(c("probitNorm", "propF", "yeoJohnson"))$transform)

  # Box Cox and Yeo Johnson are not compatible
  expect_equal(testCombine(c("boxCox", "prop", "yeoJohnson")),
               testCombine(c("boxCox", "propT", "yeoJohnson")))

  # Prop and pow are not compatible
  expect_equal(testCombine(c("pow", "prop")),
               testCombine(c("pow", "prop")))

  # Make sure demotion works
  expect_equal(rxDemoteAddErr(testCombine(c("add", "prop"))),
               testCombine("prop"))

  expect_equal(rxDemoteAddErr(testCombine(c("add", "pow"))),
               testCombine("pow"))

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ logitNorm(add.sd) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat) -> mod

  expect_equal(mod$predDf$errType, testCombine(c("pow", "add"))$errType)

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v + add.sd
    cp ~ logitNorm(NA) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat) -> mod

  expect_equal(mod$predDf$errType, testCombine(c("pow"))$errType)

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v + add.sd + lambda
    cp ~ lnorm(NA) + pow(pow.sd, pow) | cond
  }), lmat) -> mod

  expect_equal(mod$predDf$errType, testCombine(c("pow"))$errType)

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v + add.sd + lambda
    cp ~ lnorm(add.sd) + pow(pow.sd, pow)  | cond
  }), lmat) -> mod

  expect_equal(mod$predDf$errType, testCombine(c("add", "pow"))$errType)

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v + add.sd
    cp ~ probitNorm(NA) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat) -> mod

  expect_equal(mod$predDf$errType, testCombine(c("pow"))$errType)

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ probitNorm(add.sd) + pow(pow.sd, pow) + boxCox(lambda) | cond
  }), lmat) -> mod

  expect_equal(mod$predDf$errType, testCombine(c("add", "pow"))$errType)

})

test_that("categorical expressions", {

  expect_err2(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ c(add.sd, pow.sd, pow, lambda) | cond
  }), lmat), NA)

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v
    cp ~ c(add.sd, pow.sd, pow, lambda) | cond
  }), lmat) -> mod

  testOrd <- mod$iniDf[which(mod$iniDf$condition == "cond"),c("name","err")]
  row.names(testOrd) <- NULL

  expect_equal(
    testOrd,
    structure(
      list(
        name = c("add.sd", "pow.sd", "pow", "lambda"),
        err = c("ordinal", "ordinal2", "ordinal3", "ordinal4")),
      row.names = c(NA, -4L),
      class = "data.frame")
  )

})

test_that("theta/eta/eps problems", {

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v + add.sd + pow.sd + pow + lambda
    # Nonsense parameters just to check calculated parameters being used in the error expression
    a = tka + eta.ka
    b = tka + eta.ka
    c = tka + eta.ka
    d = tka + eta.ka
    e = tka + eta.ka
    f = tka + eta.ka
    l = tka + eta.ka
    cp ~ add(a) + powF(b, c, f) + t(d, e) + boxCox(l)| cond
  }), lmat) -> mod

  expect_equal(mod$predDf[, c("a", "b", "c", "d", "e", "lambda")],
               structure(list(a = "a", b = "b", c = "c", d = "d", e = "e", lambda = "l"), class = "data.frame", row.names = c(NA, -1L)))

  .rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v+ add.sd + pow.sd + pow + lambda
    cp ~ add(a) + powF(b, c, f) + t(d, e) + boxCox(l)| cond
  }), lmat) -> mod

})

test_that("no defined errors throw an error", {

  expect_error(.rx$.errProcessExpression(quote({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
    v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
    vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl/v * center
    cp = center/v + add.sd + pow.sd + pow + lambda
  }), lmat))

})

test_that("multiple endpoint parsing", {

  lmat <- lotri({
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

  .rx$.errProcessExpression(quote({
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
  }), lmat) -> mod

  expect_equal(mod$predDf[, c("cond", "var", "dvid", "cmt")],
               structure(list(cond = c("cp", "effect"), var = c("cp", "effect"), dvid = 1:2, cmt = 5:4), class = "data.frame", row.names = c(NA, -2L)))

  .rx$.errProcessExpression(quote({
    ktr <- exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    ##
    emax=expit(temax+eta.emax)
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
    cp ~ prop(prop.err) + add(pkadd.err) | center
    effect ~ add(pdadd.err)
  }), lmat) -> mod

  expect_equal(mod$predDf[, c("cond", "var", "dvid", "cmt")],
               structure(list(cond = c("center", "effect"), var = c("cp", "effect"), dvid = 1:2, cmt = 3:4),
                         class = "data.frame", row.names = c(NA, -2L)))

  .rx$.errProcessExpression(quote({
    ktr <- exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
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
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(mod$predDf[, c("cond", "var", "dvid", "cmt")],
               structure(list(cond = c("cp", "pca"), var = c("cp", "effect"), dvid = 1:2, cmt = 5:6),
                         class = "data.frame", row.names = c(NA, -2L)))

})

# Try hidden expressions combined with error expression
test_that("test expressions that are hidden with ~ vs error expressions with ~", {

  lmat <- lotri({
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

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(mod$predDf[, c("cond", "var", "dvid", "cmt")],
               structure(list(cond = c("cp", "pca"), var = c("cp", "effect"), dvid = 1:2, cmt = 5:6),
                         class = "data.frame", row.names = c(NA, -2L)))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(DCP)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(mod$predDf[, c("cond", "var", "dvid", "cmt")],
               structure(list(cond = c("cp", "pca"), var = c("cp", "effect"), dvid = 1:2, cmt = 5:6),
                         class = "data.frame", row.names = c(NA, -2L)))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(
    mod$predDf[, c("cond", "var", "dvid", "cmt")],
    structure(
      list(cond = c("cp", "pca"),
           var = c("cp", "effect"),
           dvid = 1:2,
           cmt = 5:6
      ),
      class = "data.frame", row.names = c(NA, -2L)
    )
  )

})

test_that("test different distributions", {

  lmat <- lotri({
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

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ dchisq(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "chisq"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ chisq(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "chisq"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ dexp(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "dexp"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ df(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("f", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ f(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("f", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v + pdadd.err
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ pois(lambda)
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "pois"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v + pdadd.err
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ dpois(lambda)
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "pois"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v + pdadd.err
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ dbinom(pdadd.err)
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "binom"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v + pdadd.err
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ binom(pdadd.err)
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "binom"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ dbeta(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("beta", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ beta(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("beta", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ geom(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "geom"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ dgeom(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("norm", "geom"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ hyper(prop.err, pkadd.err, pdadd.err)
    effect ~ dpois(cp) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("hyper", "pois"))

  ## "unif", #11

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ unif(prop.err, pkadd.err)
    effect ~ add(cp) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("unif", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ dunif(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("unif", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ weibull(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("weibull", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ dweibull(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("weibull", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ c(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("ordinal", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ c(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("ordinal", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ dt(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("t", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ t(prop.err, pkadd.err)
    effect ~ add(pdadd.err) | pca
  }), lmat) -> mod

  expect_equal(paste(mod$predDf$distribution), c("t", "norm"))

  lmat2 <- lotri({
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
    df <- c(1, 10)
  })

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err) + dt(df)
    effect ~ add(pdadd.err) | pca
  }), lmat2) -> mod

  expect_equal(paste(mod$predDf$distribution), c("t", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err) + dt(df)
    effect ~ add(pdadd.err) | pca
  }), lmat2) -> mod

  expect_equal(paste(mod$predDf$distribution), c("t", "norm"))

  .rx$.errProcessExpression(quote({
    ktr ~ exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    emax = expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    tmp ~ exp(eta.tr)
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin ~ e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    n2ll(cp) ~ log(prop.err) + log(cp)
    effect ~ add(pdadd.err) | pca
  }), lmat2) -> mod

})

test_that(".isErrorExpression", {

  expect_false(.rx$.isErrorExpression(quote(lipre~add(add.err) + 3*x | matt)))
  expect_true(.rx$.isErrorExpression(quote(lipre~add(add.err) | matt)))
  expect_true(.rx$.isErrorExpression(quote(lipre~add(add.err) + prop(prop.sd) + boxCox(lambda)| matt)))
  expect_true(.rx$.isErrorExpression(quote(lipre~add(add.err) + prop(prop.sd) + boxCox(lambda))))
  expect_false(.rx$.isErrorExpression(quote(lipre <- add(add.err) + prop(prop.sd) + boxCox(lambda))))
  expect_false(.rx$.isErrorExpression(quote(lipre + 2 <- add(add.err) + prop(prop.sd) + boxCox(lambda))))
  expect_true(.rx$.isErrorExpression(quote(linCmt() ~ add(add.err) + prop(prop.err))))

})
