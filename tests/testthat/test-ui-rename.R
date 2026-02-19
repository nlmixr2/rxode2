rxTest({
  test_that("rename for ui makes sense", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
    }

    expect_error(rxRename(ocmt, d/dt(cp1) ~ cp))
    expect_error(rxRename(ocmt, cp1 ~ cp))
    expect_error(rxRename(ocmt, cp1=3))
    expect_error(rxRename(ocmt, cp=cp1))
    expect_error(rxRename(ocmt, cp2=cp1))

    f <- rxRename(ocmt, cpParent=cp, parentDepot=depot, parentCentral=center, parentKa=ka,
                  parentTka=tka, parent.sd=add.sd)

    expect_true("parent.sd" %in% f$iniDf$name)
    expect_true("parentTka" %in% f$iniDf$name)
    expect_true("parentKa" %in% f$mv0$lhs)
    expect_true("parentCentral" %in% f$mv0$state)
    expect_true("parentDepot" %in% f$mv0$state)
    expect_true("cpParent" %in% f$mv0$lhs)
    expect_true("cpParent" %in% f$predDf$var)
    expect_true("cpParent" %in% f$predDf$cond)

    f <- dplyr::rename(ocmt, cp.parent=cp)
    expect_true("cp.parent" %in% f$mv0$lhs)
    expect_true("cp.parent" %in% f$predDf$var)
    expect_true("cp.parent" %in% f$predDf$cond)

    f2 <- dplyr::rename(f, depot.parent=depot)
    expect_true("depot.parent" %in% f2$mv0$state)

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd) | parent
      })
    }

    f <- rxRename(ocmt, metabolite=parent)

    expect_true("metabolite" %in% f$predDf$cond)

    # now test that functions remain intact

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
        lag <- 1
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        lag(depot) <- lag
        cp = center / v
        cp ~ add(add.sd) | parent
      })
    }

    f <- dplyr::rename(ocmt, alag=lag)

    expect_equal(f$lstExpr[[6]], quote(lag(depot) <- alag))

    # now test that d/dt(item) remains intact


    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
        d <- 1
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        lag(depot) <- d
        cp = center / v
        cp ~ add(add.sd) | parent
      })
    }

    f <- dplyr::rename(ocmt, alag=d)
    expect_equal(f$lstExpr[[6]], quote(lag(depot) <- alag))

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        depot(0) <- 1
        rate(depot) <- 1
        dur(depot) <- 1
        alag(depot) <- 1
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
    }

    tmp <- rxRename(rxode2(ocmt), dcmt=depot)

    expect_equal(tmp$lstExpr[[5]], quote(dcmt(0) <- 1))

    expect_equal(tmp$lstExpr[[6]], quote(rate(dcmt) <- 1))
    expect_equal(tmp$lstExpr[[7]], quote(dur(dcmt) <- 1))
    expect_equal(tmp$lstExpr[[8]], quote(alag(dcmt) <- 1))

  })

  test_that("rename with sigma and thetaMat", {

    f <- function() {
      description <- "BOLUS_2CPT_CLV1QV2 SINGLE DOSE FOCEI (120 Ind/2280 Obs) runODE032"
      dfObs <- 2280
      dfSub <- 120
      sigma <- lotri({
        eps1 ~ 1
      })
      thetaMat <- lotri({
        theta1 + theta2 + theta3 + theta4 + RSV + eps1 + eta1 +
          omega.2.1 + eta2 + omega.3.1 + omega.3.2 + eta3 +
          omega.4.1 + omega.4.2 + omega.4.3 + eta4 ~
            c(0.000887681,
              -0.00010551, 0.000871409, 0.000184416, -0.000106195,
              0.00299336, -0.000120234, -5.06663e-05, 0.000165252,
              0.00121347, 5.2783e-08, -1.56562e-05, 5.99331e-06,
              -2.53991e-05, 9.94218e-06, 0, 0, 0, 0, 0, 0, -4.71273e-05,
              4.69667e-05, -3.64271e-05, 2.54796e-05, -8.16885e-06,
              0, 0.000169296, 0, 0, 0, 0, 0, 0, 0, 0, -7.37156e-05,
              2.56634e-05, -8.08349e-05, 1.37e-05, -4.36564e-06,
              0, 8.75181e-06, 0, 0.00015125, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.63383e-05,
              -8.19002e-05, 0.000548985, 0.000168356, 1.59122e-06,
              0, 3.48714e-05, 0, 4.31593e-07, 0, 0, 0.000959029,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, -9.49661e-06, 0.000110108,
              -0.000306537, -9.12897e-05, 3.1877e-06, 0, 1.36628e-05,
              0, -1.95096e-05, 0, 0, -0.00012977, 0, 0, 0, 0.00051019)
      })
      ini({
        theta1 <- 1.37034036528946
        label("log Cl")
        theta2 <- 4.19814911033061
        label("log Vc")
        theta3 <- 1.38003493562413
        label("log Q")
        theta4 <- 3.87657341967489
        label("log Vp")
        RSV <- c(0, 0.196446108190896, 1)
        label("RSV")
        eta1 ~ 0.101251418415006
        eta2 ~ 0.0993872449483344
        eta3 ~ 0.101302674763154
        eta4 ~ 0.0730497519364148
      })
      model({
        cmt(CENTRAL)
        cmt(PERI)
        cl <- exp(theta1 + eta1)
        v <- exp(theta2 + eta2)
        q <- exp(theta3 + eta3)
        v2 <- exp(theta4 + eta4)
        v1 <- v
        scale1 <- v
        k21 <- q/v2
        k12 <- q/v
        d/dt(CENTRAL) <- k21 * PERI - k12 * CENTRAL - cl * CENTRAL/v1
        d/dt(PERI) <- -k21 * PERI + k12 * CENTRAL
        f <- CENTRAL/scale1
        ipred <- f
        rescv <- RSV
        w <- ipred * rescv
        ires <- DV - ipred
        iwres <- ires/w
        y <- ipred + w * eps1
      })
    }

    f <- f()

    f2 <- rxRename(f, eta.cl=eta1, eta.v=eta2, eta.q=eta3, eta.v2=eta4, eps=eps1)

    expect_equal(sort(intersect(dimnames(f2$omega)[[1]],
                                c("eta.cl", "eta.v", "eta.q", "eta.v2"))),
                 c("eta.cl", "eta.q", "eta.v", "eta.v2"))

    expect_equal(sort(intersect(dimnames(f2$thetaMat)[[1]],
                                c("eta.cl", "eta.v", "eta.q", "eta.v2", "eps"))),
                 c("eps", "eta.cl", "eta.q", "eta.v", "eta.v2"))

    expect_equal(dimnames(f2$sigma)[[1]], "eps")

    f <- rxUiDecompress(f)
    f$sigma <- f$meta$sigma
    f$thetaMat <- f$meta$thetaMat
    rm("sigma", envir=f$meta)
    rm("thetaMat", envir=f$meta)
    f <- rxUiCompress(f)

    f3 <- rxRename(f, eta.cl=eta1, eta.v=eta2, eta.q=eta3, eta.v2=eta4, eps=eps1)

    expect_equal(sort(intersect(dimnames(f3$omega)[[1]],
                                c("eta.cl", "eta.v", "eta.q", "eta.v2"))),
                 c("eta.cl", "eta.q", "eta.v", "eta.v2"))

    expect_equal(sort(intersect(dimnames(f3$thetaMat)[[1]],
                                c("eta.cl", "eta.v", "eta.q", "eta.v2", "eps"))),
                 c("eps", "eta.cl", "eta.q", "eta.v", "eta.v2"))

    expect_equal(dimnames(f3$sigma)[[1]], "eps")

  })

  test_that("rename doesn't change parent ui", {

    f <- function() {
      description <- "BOLUS_2CPT_CLV1QV2 SINGLE DOSE FOCEI (120 Ind/2280 Obs) runODE032"
      dfObs <- 2280
      dfSub <- 120
      sigma <- lotri({
        eps1 ~ 1
      })
      thetaMat <- lotri({
        theta1 ~ c(theta1 = 0.000887681)
        theta2 ~ c(theta1 = -0.00010551, theta2 = 0.000871409)
        theta3 ~ c(theta1 = 0.000184416, theta2 = -0.000106195,
                   theta3 = 0.00299336)
        theta4 ~ c(theta1 = -0.000120234, theta2 = -5.06663e-05,
                   theta3 = 0.000165252, theta4 = 0.00121347)
        RSV ~ c(theta1 = 5.2783e-08, theta2 = -1.56562e-05, theta3 = 5.99331e-06,
                theta4 = -2.53991e-05, RSV = 9.94218e-06)
        eps1 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                 RSV = 0, eps1 = 0)
        eta1 ~ c(theta1 = -4.71273e-05, theta2 = 4.69667e-05,
                 theta3 = -3.64271e-05, theta4 = 2.54796e-05, RSV = -8.16885e-06,
                 eps1 = 0, eta1 = 0.000169296)
        omega.2.1 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                      RSV = 0, eps1 = 0, eta1 = 0, omega.2.1 = 0)
        eta2 ~ c(theta1 = -7.37156e-05, theta2 = 2.56634e-05,
                 theta3 = -8.08349e-05, theta4 = 1.37e-05, RSV = -4.36564e-06,
                 eps1 = 0, eta1 = 8.75181e-06, omega.2.1 = 0, eta2 = 0.00015125)
        omega.3.1 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                      RSV = 0, eps1 = 0, eta1 = 0, omega.2.1 = 0, eta2 = 0,
                      omega.3.1 = 0)
        omega.3.2 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                      RSV = 0, eps1 = 0, eta1 = 0, omega.2.1 = 0, eta2 = 0,
                      omega.3.1 = 0, omega.3.2 = 0)
        eta3 ~ c(theta1 = 6.63383e-05, theta2 = -8.19002e-05,
                 theta3 = 0.000548985, theta4 = 0.000168356, RSV = 1.59122e-06,
                 eps1 = 0, eta1 = 3.48714e-05, omega.2.1 = 0, eta2 = 4.31593e-07,
                 omega.3.1 = 0, omega.3.2 = 0, eta3 = 0.000959029)
        omega.4.1 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                      RSV = 0, eps1 = 0, eta1 = 0, omega.2.1 = 0, eta2 = 0,
                      omega.3.1 = 0, omega.3.2 = 0, eta3 = 0, omega.4.1 = 0)
        omega.4.2 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                      RSV = 0, eps1 = 0, eta1 = 0, omega.2.1 = 0, eta2 = 0,
                      omega.3.1 = 0, omega.3.2 = 0, eta3 = 0, omega.4.1 = 0,
                      omega.4.2 = 0)
        omega.4.3 ~ c(theta1 = 0, theta2 = 0, theta3 = 0, theta4 = 0,
                      RSV = 0, eps1 = 0, eta1 = 0, omega.2.1 = 0, eta2 = 0,
                      omega.3.1 = 0, omega.3.2 = 0, eta3 = 0, omega.4.1 = 0,
                      omega.4.2 = 0, omega.4.3 = 0)
        eta4 ~ c(theta1 = -9.49661e-06, theta2 = 0.000110108,
                 theta3 = -0.000306537, theta4 = -9.12897e-05, RSV = 3.1877e-06,
                 eps1 = 0, eta1 = 1.36628e-05, omega.2.1 = 0, eta2 = -1.95096e-05,
                 omega.3.1 = 0, omega.3.2 = 0, eta3 = -0.00012977,
                 omega.4.1 = 0, omega.4.2 = 0, omega.4.3 = 0, eta4 = 0.00051019)
      })
      validation <- c("IPRED relative difference compared to Nonmem IPRED: 0%; 95% percentile: (0%,0%); rtol=6.43e-06",
                      "IPRED absolute difference compared to Nonmem IPRED: 95% percentile: (2.19e-05, 0.0418); atol=0.00167",
                      "IWRES relative difference compared to Nonmem IWRES: 0%; 95% percentile: (0%,0.01%); rtol=8.99e-06",
                      "IWRES absolute difference compared to Nonmem IWRES: 95% percentile: (1.82e-07, 4.63e-05); atol=3.65e-06",
                      "PRED relative difference compared to Nonmem PRED: 0%; 95% percentile: (0%,0%); rtol=6.41e-06",
                      "PRED absolute difference compared to Nonmem PRED: 95% percentile: (1.41e-07,0.00382) atol=6.41e-06")
      ini({
        theta1 <- 1.37034036528946
        label("log Cl")
        theta2 <- 4.19814911033061
        label("log Vc")
        theta3 <- 1.38003493562413
        label("log Q")
        theta4 <- 3.87657341967489
        label("log Vp")
        RSV <- c(0, 0.196446108190896, 1)
        label("RSV")
        eta1 ~ 0.101251418415006
        eta2 ~ 0.0993872449483344
        eta3 ~ 0.101302674763154
        eta4 ~ 0.0730497519364148
      })
      model({
        cmt(CENTRAL)
        cmt(PERI)
        cl <- exp(theta1 + eta1)
        v <- exp(theta2 + eta2)
        q <- exp(theta3 + eta3)
        v2 <- exp(theta4 + eta4)
        v1 <- v
        scale1 <- v
        k21 <- q/v2
        k12 <- q/v
        d/dt(CENTRAL) <- k21 * PERI - k12 * CENTRAL - cl * CENTRAL/v1
        d/dt(PERI) <- -k21 * PERI + k12 * CENTRAL
        f <- CENTRAL/scale1
        ipred <- f
        rescv <- RSV
        w <- ipred * rescv
        ires <- DV - ipred
        iwres <- ires/w
        y <- ipred + w * eps1
      })
    }

    f <- f()

    f2 <- f %>% rxRename(err1=eps1)
    expect_equal(dimnames(f2$sigma)[[1]], "err1")
    expect_equal(dimnames(f$sigma)[[1]], "eps1")
    expect_true(any(dimnames(f2$thetaMat)[[1]] == "err1"))
    expect_false(any(dimnames(f2$thetaMat)[[1]] == "eps1"))
    expect_false(any(dimnames(f$thetaMat)[[1]] == "err1"))
    expect_true(any(dimnames(f$thetaMat)[[1]] == "eps1"))

    f3 <- f %>% rxRename(Vp=theta4)
    expect_true(any(dimnames(f3$thetaMat)[[1]] == "Vp"))
    expect_false(any(dimnames(f3$thetaMat)[[1]] == "theta4"))
    expect_false(any(dimnames(f$thetaMat)[[1]] == "Vp"))
    expect_true(any(dimnames(f$thetaMat)[[1]] == "theta4"))

  })


})
