rxTest({
  withr::local_options(list(rxode2.eventSens = "fd"))

  test_that("dose-history names as an lhs do not break analytic Jacobian (calcJac)", {
    ## A bare dose-history name (tad, dosenum, tlast, ...) used as a value expands
    ## to its functional form, but as an assignment *target* it must stay a plain
    ## symbol.  Otherwise the lhs is rewritten into the function body, producing an
    ## invalid assignment like `(t-tlast()) = t-tlast()` that fails calcJac.  FOCEi's
    ## finite-difference model appends exactly `tad = tad()` / `dosenum = dosenum()`,
    ## and delay()/DDE models force the Jacobian-requiring dense solver, so this hit
    ## every DDE fit (rxode2/nlmixr2#dde).
    expect_s3_class(
      suppressMessages(rxode2("d/dt(cen) <- -k*cen\ncen(0) <- 10\nk <- 0.2\ntad <- tad()",
                              calcJac = TRUE)),
      "rxode2"
    )
    expect_s3_class(
      suppressMessages(rxode2("d/dt(cen) <- -k*cen\ncen(0) <- 10\nk <- 0.2\ndosenum <- dosenum()",
                              calcJac = TRUE)),
      "rxode2"
    )
    ## the practical trigger: a delay() model with the FOCEi FD-model bookkeeping lines
    expect_s3_class(
      suppressMessages(rxode2(paste0(
        "d/dt(cen) <- -k*cen + kin*delay(cen, tau)\ncen(0) <- 10\n",
        "k <- 0.2\nkin <- 0.5\ntau <- 1\ntad <- tad()\ndosenum <- dosenum()"),
        calcJac = TRUE)),
      "rxode2"
    )
    ## a bare dose-history name used as a value still expands (no regression)
    .m <- suppressMessages(rxode2("d/dt(cen) <- -k*cen\ncen(0) <- 10\nk <- 0.2\ny <- tad + 1"))
    expect_true(grepl("y=tad+1", rxNorm(.m), fixed = TRUE))
  })

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
    et$add.dosing(20, start.time = 0, evid = 7)

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

  test_that("df(state)/dy(THETA[n]) and dy(ETA[n]) do not segfault", {
    # The grammar accepts a bracketed THETA[n]/ETA[n] in dy(), but handleDy()
    # never registered the synthetic _THETA_n_ / _ETA_n_ symbol, leaving tb.ix
    # = -2; that -2 was stored in tb.dy[] and assertCorrectDfDy() then read
    # tb.ss.line[-2] -- an out-of-bounds access that crashed R.  nlm/FOCEi hit
    # this by re-parsing their generated calcJac model (parameters expressed as
    # THETA[n]) in the residual/table step.  It must error or parse, never crash.
    # The operative property is simply "did not crash": rxModelVars() returns
    # normally, whether it parses the model or raises a clean R error.
    .safe <- function(m) {
      .r <- tryCatch(suppressMessages(rxModelVars(m)), error = function(e) "clean-error")
      !is.null(.r)
    }
    expect_true(.safe("d/dt(x1)=-x1\nx1(0)=1\ny=x1\ndf(x1)/dy(THETA[1])=0\n"))
    expect_true(.safe("d/dt(x1)=-x1\nx1(0)=1\ny=x1\ndf(x1)/dy(ETA[1])=0\n"))
    # a full re-parsed calcJac DDE model (delay in a product of delayed states,
    # parameters as THETA[n], the FOCEi tad=/dosenum= bookkeeping) must not crash
    expect_true(.safe(paste0(
      "cmt(x1);\ncmt(x2);\nka=2495\nke=2.52\nV=2.79\n",
      "d/dt(x1)=-ka*x1\nd/dt(x2)=ka*x1-ke*x2\n",
      "d/dt(cen)=exp(THETA[1])*delay(x1, exp(THETA[2]))*delay(x2, exp(THETA[2]))\n",
      "cen(0)=0\ndf(cen)/dy(x1)=0\ndf(cen)/dy(THETA[1])=0\ndf(cen)/dy(THETA[2])=0\n",
      "df(cen)/dy(THETA[3])=0\ny=cen\ntad=t-tlast()\n"
    )))
  })

  test_that("dfdy names report ETA[n] and THETA[n], not the internal names", {
    # THETA and ETA both reach populateDfdy() under their internal spellings
    # (_THETA_1_, _ETA_1_) and are translated back for display.  The ETA half
    # of that translation used to be discarded, so an ETA derivative reported
    # the internal name while a THETA derivative next to it did not.
    mod <- rxode2({
      d / dt(A) <- -(CL / V) * A + THETA[1] * ETA[1]
      df(A) / dy(A) <- -(CL / V)
      df(A) / dy(THETA[1]) <- ETA[1]
      df(A) / dy(ETA[1]) <- THETA[1]
    })

    expect_equal(
      rxDfdy(mod),
      c("df(A)/dy(A)", "df(A)/dy(THETA[1])", "df(A)/dy(ETA[1])")
    )
    expect_false(any(grepl("_ETA_", rxDfdy(mod), fixed = TRUE)))
    expect_false(any(grepl("_THETA_", rxDfdy(mod), fixed = TRUE)))
  })

  test_that("a Jacobian on a dotted state name compiles", {
    m <- rxode2("cl <- exp(tcl + eta.cl)\nd/dt(central) = -cl/v*central",
                calcSens = "eta.cl", calcJac = TRUE)
    s <- rxSolve(m, et(amt = 100) |> et(1:3), c(tcl = 0, eta.cl = 0.1, v = 10))
    expect_equal(
      s[["rx__sens_central_BY_eta.cl__"]],
      -100 * exp(0.1) / 10 * (1:3) * exp(-exp(0.1) / 10 * (1:3)),
      tolerance = 1e-5
    )
  })

  test_that("lsoda reads the analytic Jacobian in column-major order", {
    # calc_jac is row major; a transposed Jacobian made lsoda hit MXSTEP here.
    .rob <- "d/dt(a) = -k1*a + k2*b*cc\nd/dt(b) = k1*a - k2*b*cc - k3*b*b\nd/dt(cc) = k3*b*b"
    .cs <- c("k1", "k2", "k3")
    .p <- c(k1 = 0.04, k2 = 1e4, k3 = 3e7)
    .ini <- c(a = 1, b = 0, cc = 0)
    .ev <- et(c(0.1, 1, 10, 100))
    .ref <- rxSolve(
      rxode2(.rob, calcSens = .cs, eventSens = "fd"),
      .ev,
      params = .p,
      inits = .ini,
      method = "lsoda",
      atol = 1e-10,
      rtol = 1e-10
    )
    .mj <- rxode2(.rob, calcSens = .cs, calcJac = TRUE, eventSens = "fd")
    for (.m in c("lsoda", "bdf")) {
      .s <- rxSolve(.mj, .ev, params = .p, inits = .ini, method = .m, atol = 1e-10, rtol = 1e-10)
      expect_equal(.s$rx__sens_a_BY_k1__, .ref$rx__sens_a_BY_k1__, tolerance = 1e-6, label = .m)
      expect_equal(.s$rx__sens_b_BY_k3__, .ref$rx__sens_b_BY_k3__, tolerance = 1e-6, label = .m)
    }
  })
})
