.rx <- loadNamespace("rxode2")

f <- function() {
  ini({
    tke <- 0.5
    eta.ke ~ 0.04
    prop.sd <- sqrt(0.1)
  })
  model({
    ke <- tke * exp(eta.ke)
    ipre <- 10 * exp(-ke * t)
    f2 <- ipre / (ipre + 5)
    f3 <- f2 * 3
    lipre <- log(ipre)
    ipre ~ prop(prop.sd)
  })
}

v1 <- function(ui) {
  .rx$.rxGetVarianceForErrorType(ui, ui$predDf[1, ])
}

test_that("constant error model tests", {
  # Constant:  y=f+a*err
  # Var = a^2

  f %>%
    model(ipre ~ add(add.sd)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd) ^ 2))

  f %>%
    model(ipre ~ add(f2)) ->
    tmp2


  expect_equal(v1(tmp2),
               quote((f2) ^ 2))
})

test_that("prop model tests", {
  # Proportional:  y=f+b*f*err
  # Var = (f*b)^2

  f %>%
    model(ipre ~ prop(prop.sd)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((rx_pred_f_ * prop.sd) ^ 2))

  f %>%
    model(ipre ~ prop(f2)) ->
    tmp2

  expect_equal(v1(tmp2),
               quote((rx_pred_f_ * f2) ^ 2))

  f %>%
    model(ipre ~ propT(prop.sd)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((rx_pred_ * prop.sd) ^ 2))

  f %>%
    model(ipre ~ propT(f2)) ->
    tmp2

  expect_equal(v1(tmp2),
               quote((rx_pred_ * f2) ^ 2))

  f %>%
    model(ipre ~ propF(prop.sd, f3)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f3 * prop.sd) ^ 2))

  f %>%
    model(ipre ~ propF(f2, f3)) ->
    tmp2

  expect_equal(v1(tmp2),
               quote((f3 * f2) ^ 2))
})

test_that("power models", {
  # Power:  y=f+b*f^c*err
  # Var = (f^c*b)^2

  # pow(sd,pw)
  f %>%
    model(ipre ~ pow(prop.sd, pw)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((rx_pred_f_)^(pw) * prop.sd)^2))

  f %>%
    model(ipre ~ pow(f2, pw)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((rx_pred_f_)^(pw) * f2)^2))

  # powT(sd,pw)
  f %>%
    model(ipre ~ powT(prop.sd, pw)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((rx_pred_)^(pw) * prop.sd)^2))

  f %>%
    model(ipre ~ powT(f2, pw)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((rx_pred_)^(pw) * f2)^2))

  # powF(sd,pw,f)
  f %>%
    model(ipre ~ powF(prop.sd, pw, f3)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f3)^(pw) * prop.sd)^2))

  f %>%
    model(ipre ~ powF(f2, pw, f3)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f3)^(pw) * f2)^2))

  f %>%
    model(ipre ~ powF(f2, lipre, f3)) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f3)^(lipre) * f2)^2))
})

test_that("add+prop combined1", {
  # Combined1:  y=f + (a+b*f^c)*err
  # Var = (a + b*f^c)^2

  f %>%
    model(ipre ~ add(add.sd) + prop(prop.sd) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_f_) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(f2) + prop(prop.sd) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_f_) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(add.sd) + prop(f2) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_f_) * (f2))^2))

  f %>%
    model(ipre ~ add(f2) + prop(f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_f_) * (f3))^2))

  # propT
  f %>%
    model(ipre ~ add(add.sd) + propT(prop.sd) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(f2) + propT(prop.sd) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(add.sd) + propT(f2) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_) * (f2))^2))

  f %>%
    model(ipre ~ add(f2) + propT(f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_) * (f3))^2))

  # propF
  f %>%
    model(ipre ~ add(add.sd) + propF(prop.sd, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (f3) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(lipre) + propF(prop.sd, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((lipre) + (f3) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(add.sd) + propF(lipre, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (f3) * (lipre))^2))

  f %>%
    model(ipre ~ add(lipre) + propF(f2, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((lipre) + (f3) * (f2))^2))
})


test_that("add+pow combined1", {
  # Combined1:  y=f + (a+b*f^c)*err
  # Var = (a + b*f^c)^2

  f %>%
    model(ipre ~ add(add.sd) + pow(prop.sd, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_f_) ^ (c) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(f2) + pow(prop.sd, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_f_) ^ (c) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(add.sd) + pow(f2, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_f_) ^ (c)* (f2))^2))

  f %>%
    model(ipre ~ add(f2) + pow(f3, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_f_) ^ (c) * (f3))^2))

  # propT
  f %>%
    model(ipre ~ add(add.sd) + powT(prop.sd, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_) ^ (c) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(f2) + powT(prop.sd, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_) ^ (c)* (prop.sd))^2))

  f %>%
    model(ipre ~ add(add.sd) + powT(f2, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (rx_pred_) ^ (c)* (f2))^2))

  f %>%
    model(ipre ~ add(f2) + powT(f3, c) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((f2) + (rx_pred_) ^ (c) * (f3))^2))

  # propF
  f %>%
    model(ipre ~ add(add.sd) + powF(prop.sd, c, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (f3) ^ (c) * (prop.sd))^2))

  f %>%
    model(ipre ~ add(lipre) + powF(prop.sd, c, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((lipre) + (f3) ^ (c)* (prop.sd))^2))

  f %>%
    model(ipre ~ add(add.sd) + powF(lipre, c, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((add.sd) + (f3) ^ (c) * (lipre))^2))


  f %>%
    model(ipre ~ add(lipre) + powF(f2, c, f3) + combined1()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote(((lipre) + (f3) ^ (c)* (f2))^2))
})

# combined2
test_that("add+prop combined2", {
  # Combined2:  y=f + sqrt(a^2 + b^2*(f^c)^2)*err
  # Var = a^2 + b^2*(f^c)^2

  f %>%
    model(ipre ~ add(add.sd) + prop(prop.sd) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + (rx_pred_f_)^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(f2) + prop(prop.sd) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + (rx_pred_f_)^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(add.sd) + prop(f2) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + (rx_pred_f_)^2 * (f2)^2))

  f %>%
    model(ipre ~ add(f2) + prop(f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + (rx_pred_f_)^2 * (f3)^2))

  # propT
  f %>%
    model(ipre ~ add(add.sd) + propT(prop.sd) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + (rx_pred_)^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(f2) + propT(prop.sd) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + (rx_pred_)^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(add.sd) + propT(f2) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + (rx_pred_)^2 * (f2)^2))

  f %>%
    model(ipre ~ add(f2) + propT(f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + (rx_pred_)^2 * (f3)^2))

  # propF
  f %>%
    model(ipre ~ add(add.sd) + propF(prop.sd, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + (f3)^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(lipre) + propF(prop.sd, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((lipre)^2 + (f3)^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(add.sd) + propF(lipre, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + (f3)^2 * (lipre)^2))


  f %>%
    model(ipre ~ add(lipre) + propF(f2, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((lipre)^2 + (f3)^2 * (f2)^2))
})


test_that("add+pow combined2", {
  # Combined2:  y=f + (a+b*f^c)*err
  # Var = (a + b*f^c)^2

  f %>%
    model(ipre ~ add(add.sd) + pow(prop.sd, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + ((rx_pred_f_)^(c))^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(f2) + pow(prop.sd, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + ((rx_pred_f_)^(c))^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(add.sd) + pow(f2, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + ((rx_pred_f_)^(c))^2 * (f2)^2))

  f %>%
    model(ipre ~ add(f2) + pow(f3, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + ((rx_pred_f_)^(c))^2 * (f3)^2))

  # propT
  f %>%
    model(ipre ~ add(add.sd) + powT(prop.sd, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + ((rx_pred_)^(c))^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(f2) + powT(prop.sd, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + ((rx_pred_)^(c))^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(add.sd) + powT(f2, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + ((rx_pred_)^(c))^2 * (f2)^2))

  f %>%
    model(ipre ~ add(f2) + powT(f3, c) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((f2)^2 + ((rx_pred_)^(c))^2 * (f3)^2))

  # propF
  f %>%
    model(ipre ~ add(add.sd) + powF(prop.sd, c, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + ((f3)^(c))^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(lipre) + powF(prop.sd, c, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((lipre)^2 + ((f3)^(c))^2 * (prop.sd)^2))

  f %>%
    model(ipre ~ add(add.sd) + powF(lipre, c, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((add.sd)^2 + ((f3)^(c))^2 * (lipre)^2))

  f %>%
    model(ipre ~ add(lipre) + powF(f2, c, f3) + combined2()) ->
    tmp1

  expect_equal(v1(tmp1),
               quote((lipre)^2 + ((f3)^(c))^2 * (f2)^2))
})
