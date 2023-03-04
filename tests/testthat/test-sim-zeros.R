rxTest({
  test_that("simulate zeros tests", {
    
    # from nonmem2R
    f <- function() {
      description <- "PK"
      ini({
        t.CL <- c(0, 27.7322)
        label("1 CL L/h parent")
        t.V1 <- c(0, 112.773)
        label("2 V1 L parent")
        t.V2 <- c(0, 98.2478)
        label("3 V2 L parent")
        t.Q1 <- c(0, 20.8085)
        label("4 Q1 parent")
        theta5 <- c(0, 0.168248)
        label("5  ERROR parent")
        t.MTT <- c(0, 0.938162)
        label("6 MTT")
        F1 <- fix(1)
        label("7 F1")
        t.CLM <- c(0, 15.6045)
        label("8 CLM metabolite")
        t.VM1 <- c(0, 9.69614)
        label("9 VM1 L metabolite")
        t.VM2 <- c(0, 44.5183)
        label("10 VM2 L metabolite")
        t.Q2 <- c(0, 5.55961)
        label("11 Q2 metabolite")
        theta12 <- c(0, 0.145536)
        label("12 ERROR metabolite")
        theta13 <- c(0, 1.65686)
        label("13 Add ERROR parent")
        theta14 <- c(0, 1.07706)
        label("14 Add ERROR metabolite")
        NTR <- c(1, 1.95835, 12)
        label("15 NTR")
        eta1 ~ 0.0704585
        e.Q1 ~ 0.0386847
        eta3 ~ 0.0430261
        e.V2 ~ fix(0)
        e.KTR ~ 0.146879
        e.NTR ~ fix(0)
        Bioavailability ~ 0.0506769
      })
      model({
        cmt(cent)
        cmt(centmet)
        cmt(peri)
        cmt(perimet)
        if (newind <= 1) {
          tdos <- -1000
          pd <- 0
        }
        if (amt > 0) {
          tdos <- t
          pd <- amt
        }
        tad2 <- t - tdos
        e1 <- exp(eta1)
        e2 <- exp(e.Q1)
        e3 <- exp(eta3)
        e4 <- exp(e.V2)
        e5 <- exp(e.KTR)
        e6 <- exp(e.NTR)
        e7 <- exp(Bioavailability)
        tvcl <- t.CL
        cl <- tvcl * e1
        tvv1 <- t.V1
        v1 <- tvv1
        tvv2 <- t.V2
        v2 <- tvv2
        tvq1 <- t.Q1
        q1 <- tvq1 * e2
        tvclm <- t.CLM
        clm <- tvclm * e3
        tvvm1 <- t.VM1
        vm1 <- tvvm1
        tvvm2 <- t.VM2
        vm2 <- tvvm2 * e4
        tvq2 <- t.Q2
        q2 <- tvq2
        mtt <- t.MTT * e5
        nt <- 1 + NTR * e6
        ktr <- nt/mtt
        nn <- nt - 1
        if (nn >= 0.558) {
          l <- log(2.5066) + (nn + 0.5) * log(nn) - nn + log(1 + 
                                                               1/(12 * nn))
        } else {
          l <- -0.1171157 + 0.569 * ((0.558 - nn)^2) - 0.1122 * 
            (0.558 - nn)
        }
        k13 <- q1/v1
        k31 <- q1/v2
        TVFM <- 0.22
        fm <- TVFM
        k10 <- cl * (1 - fm)/v1
        k12 <- fm * cl/v1
        k24 <- q2/vm1
        k42 <- q2/vm2
        k20 <- clm/vm1
        scale1 <- v1
        scale2 <- vm1
        rxf.rxddta1. <- 0
        f(cent) <- rxf.rxddta1.
        bio <- F1 * e7
        X <- 1e-07
        if (t >= tdos) {
          d/dt(cent) <- bio * pd * ktr * exp(nn * log(ktr * 
                                                        (t - tdos) + X) - ktr * (t - tdos) - l) - k10 * 
                                         cent - k12 * cent - k13 * cent + k31 * peri
        } else {
          d/dt(cent) <- -k12 * cent - k13 * cent + k31 * peri
        }
        d/dt(centmet) <- k12 * cent - k20 * centmet - k24 * centmet + 
          k42 * perimet
        d/dt(peri) <- k13 * cent - k31 * peri
        d/dt(perimet) <- k24 * centmet - k42 * perimet
        f <- cent/scale1
        strt <- FLAG
        eps1 <- rxnorm()
        if (FLAG <= 1) {
          ipred <- cent/scale1
          w <- sqrt((theta5 * ipred)^2 + theta13^2)
          y <- ipred + w * eps1
        } else {
          ipred <- centmet/scale2
          w <- sqrt((theta12 * ipred)^2 + theta14^2)
          y <- ipred + w * eps1
        }
        cp <- cent/scale1
        cm <- centmet/scale2
        del <- 0
        if (w == 0) 
          del <- 1
        ires <- DV - ipred
        iwres <- ires/(w + del)
      })
    }
    f <- f()

    e <- et(amt=100) %>% et(seq(0,20))
    e$FLAG <- 1

    expect_error(rxSolve(f, e), NA)

    m <- f$simulationModel
    
    expect_error(rxSolve(m, params=f$theta, events=e, omega=f$omega), NA)

    # ok now try just the control

    .ctl <- rxControl(omega=lotri::lotri(eta1~c(0.0)))
    expect_equal(.ctl$omega, NULL)
    expect_equal(.ctl$.zeros, "eta1")

    .ctl <- rxControl(omega=lotri::lotri(eta1+eta2~c(0.0, 0.0, 1)))
    expect_equal(.ctl$omega, lotri::lotri(eta2~1.0))
    expect_equal(.ctl$.zeros, "eta1")

    .ctl <- rxControl(omega=lotri::lotri(eta1+eta2~c(0.0, 0.0, 1)),
                      omegaLower=c(-1, -1), omegaUpper=c(1,1))
    expect_equal(.ctl$omega, lotri::lotri(eta2~1.0))
    expect_equal(.ctl$.zeros, "eta1")
    expect_equal(.ctl$omegaLower, c(eta1=-1, eta2=-1))
    expect_equal(.ctl$omegaUpper, c(eta1=1, eta2=1))

    # sigma
    .ctl <- rxControl(sigma=lotri::lotri(eps1~c(0.0)))
    expect_equal(.ctl$sigma, NULL)
    expect_equal(.ctl$.zeros, "eps1")

    .ctl <- rxControl(sigma=lotri::lotri(eps1+eps2~c(0.0, 0.0, 1)))
    expect_equal(.ctl$sigma, lotri::lotri(eps2~1.0))
    expect_equal(.ctl$.zeros, "eps1")

    .ctl <- rxControl(sigma=lotri::lotri(eps1+eps2~c(0.0, 0.0, 1)),
                      sigmaLower=c(-1, -1), sigmaUpper=c(1,1))
    expect_equal(.ctl$sigma, lotri::lotri(eps2~1.0))
    expect_equal(.ctl$.zeros, "eps1")
    expect_equal(.ctl$sigmaLower, c(eps1=-1, eps2=-1))
    expect_equal(.ctl$sigmaUpper, c(eps1=1, eps2=1))
    
  })
})
