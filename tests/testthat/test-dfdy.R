test_that("Specified jacobian is captured", {

  Vtpol2 <- rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Jacobian
df(y)/dy(dy)  = 1
df(dy)/dy(y)  = -2*dy*mu*y - 1
df(dy)/dy(dy) = mu*(1-y^2)
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
")

  expect_true(any(rxDfdy(Vtpol2) == "df(y)/dy(dy)"))
  expect_true(any(rxDfdy(Vtpol2) == "df(dy)/dy(y)"))
  expect_true(any(rxDfdy(Vtpol2) == "df(dy)/dy(dy)"))
})

## fixme multiple jacobian definitions raise an error.

tmp <- rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Jacobian
df(y)/dy(dy)  = 1
df(dy)/dy(y)  = -2*dy*mu*y - 1
df(dy)/dy(dy) = mu*(1-y^2)
df(dy)/dy(dy) = mu*(1-y^2)
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
")

test_that("Doubled jacobain will compile correctly", {
  expect_s3_class(tmp, "rxode2")
})

test_that("Jacobian and sensitivity not specified.", {
  norm <- rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
")

  expect_false(norm$calcJac)
  expect_false(norm$calcSens)
  rxDelete(norm)
})

test_that("Jacobian specified but sensitivity not specified.", {
  jac <- suppressMessages(rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
", calcJac = TRUE))

  expect_true(jac$calcJac)
  expect_false(jac$calcSens)
  rxDelete(jac)
})

test_that("Sensitivity specified.", {
  sens <- suppressMessages(rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
", calcSens = TRUE))

  expect_false(sens$calcJac)
  expect_true(sens$calcSens)
  rxDelete(sens)
})

test_that("Jac/Sens can be calculated from 'normal' model", {
  norm <- rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
")
  jac <- norm
  jac <- suppressMessages(rxode2(jac, calcJac = TRUE))
  expect_true(jac$calcJac)
  expect_false(jac$calcSens)
  sens <- suppressMessages(rxode2(jac, calcSens = TRUE))
  expect_false(sens$calcJac)
  expect_true(sens$calcSens)
  full <- suppressMessages(rxode2(jac, calcSens = TRUE, calcJac = TRUE))
  expect_false(sens$calcJac)
  expect_true(sens$calcSens)
  rxDelete(jac)
  rxDelete(sens)
  rxDelete(norm)
  rxDelete(full)
})

test_that("Jacobian and sensitivity specified.", {
  sens <- suppressMessages(rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
", calcSens = TRUE))

  norm <- suppressMessages(rxode2(sens, calcSens = FALSE))
  expect_false(norm$calcJac)
  expect_false(norm$calcSens)

  jac <- suppressMessages(rxode2(sens, calcJac = TRUE))
  expect_true(jac$calcJac)
  expect_false(jac$calcSens)
  expect_false(sens$calcJac)
  expect_true(sens$calcSens)
})

test_that("Conditional Sensitivities", {

  transit.if <- rxode2({
    ## Table 3 from Savic 2007
    cl <- 17.2 # (L/hr)
    vc <- 45.1 # L
    ka1 <- 0.38 # 1/hr
    ka2 <- 0.2 # 1/hr
    mtt <- 0.37 # hr
    bio <- 1
    n <- 20.1
    k <- cl / vc
    if (pop1 == 1) {
      ka <- ka1
    } else {
      ka <- ka2
    }
    d/dt(depot) <- transit(n, mtt, bio) - ka * depot
    d/dt(cen) <- ka * depot - k * cen
  })

  expect_false(transit.if$calcJac)
  expect_false(transit.if$calcSens)

  jac <- suppressMessages(rxode2(transit.if, calcJac = TRUE))
  expect_true(jac$calcJac)
  expect_false(jac$calcSens)

  sens <- suppressMessages(rxode2(transit.if, calcSens = TRUE))
  expect_false(sens$calcJac)
  expect_true(sens$calcSens)

  full <- suppressMessages(rxode2(transit.if, calcSens = TRUE, calcJac = TRUE))
  expect_true(full$calcJac)
  expect_true(full$calcSens)

})

test_that("Transit Sensitivities", {

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
ktr = (n+1)/mtt
## note that lgammafn is the same as lgamma in R.
d/dt(depot) = exp(log(bio*podo(depot))+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-lgammafn(n+1))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  mod <- suppressMessages(rxode2(mod, calcSens = TRUE))

  et <- eventTable()
  et$add.sampling(seq(0, 10, length.out = 200))
  et$add.dosing(20, start.time = 0, evid=7)

  transit <- suppressWarnings({
    rxSolve(mod, et)
  })

  ## Used the log(0) protection since the depot_mtt sensitivity
  ## includes log(t*...) and t=0
  ##
  ## These results are now system dependent since it uses
  ## log(sqrt(DOUBLE_EPS)) and DOUBLE_EPS varies by platform, so
  ## just make sure the results are not NA.
  expect_true(all(!is.na(transit[["_sens_depot_mtt"]])))
  expect_equal(transit$depot.mtt, transit[["_sens_depot_mtt"]])
  expect_equal(transit$depot_mtt, transit[["_sens_depot_mtt"]])
  expect_equal(transit[["depot_mtt"]], transit[["_sens_depot_mtt"]])
  expect_equal(transit[["depot.mtt"]], transit[["_sens_depot_mtt"]])

  mod <- rxode2({
    ## Table 3 from Savic 2007
    cl <- 17.2 # (L/hr)
    vc <- 45.1 # L
    tvka <- 0.38 # 1/hr
    tvmtt <- 0.37 # hr
    ka <- tvka * exp(eta_ka)
    mtt <- tvmtt * exp(eta_mtt)
    bio <- 1
    n <- 20.1
    k <- cl / vc
    ktr <- (n + 1) / mtt
    ## note that lgammafn is the same as lgamma in R.
    d/dt(depot) <- exp(log(bio * podo(depot)) + log(ktr) + n * log(ktr * tad(depot)) - ktr * tad(depot) -
                         lgammafn(n + 1)) - ka * depot
    d/dt(cen) <- ka * depot - k * cen
  })

  transit <- suppressMessages(rxode2(mod, calcSens = c("eta_ka", "eta_mtt")))
  expect_true(all(!is.na(transit[["_sens_depot_mtt"]])))

  ## tmp <- rxode2(mod, calcSens=list(eta=c("eta_ka", "eta_mtt"), theta=c("cl", "vc")));
})
