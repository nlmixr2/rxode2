rxTest({
  test_that("rxFix", {

    One.comp.transit.allo <- function() {
      ini({
        # Where initial conditions/variables are specified
        lktr <- log(1.15)  #log k transit (/h)
        lcl  <- log(0.15)  #log Cl (L/hr)
        lv   <- log(7)     #log V (L)
        ALLC <- fix(0.75)  #allometric exponent cl
        ALLV <- fix(1.00)  #allometric exponent v
        prop.err <- 0.15   #proportional error (SD/mean)
        add.err <- 0.6     #additive error (mg/L)
        eta.ktr ~ 0.5
        eta.cl ~ 0.1
        eta.v ~ 0.1
      })
      model({
        #Allometric scaling on weight
        cl <- exp(lcl + eta.cl + ALLC * logWT70)
        v  <- exp(lv + eta.v + ALLV * logWT70)
        ktr <- exp(lktr + eta.ktr)
        # RxODE-style differential equations are supported
        d/dt(depot)   = -ktr * depot
        d/dt(central) =  ktr * trans - (cl/v) * central
        d/dt(trans)   =  ktr * depot - ktr * trans
        ## Concentration is calculated
        cp = central/v
        # And is assumed to follow proportional and additive error
        cp ~ prop(prop.err) + add(add.err)
      })
    }

    tmp <- rxFixPop(One.comp.transit.allo)

    expect_equal(tmp$theta,
                 c(lktr = 0.139761942375159, lcl = -1.89711998488588, lv = 1.94591014905531,
                   prop.err = 0.15, add.err = 0.6))

    tmp2 <- rxFixPop(tmp)

    expect_equal(tmp2$theta,
                 c(lktr = 0.139761942375159, lcl = -1.89711998488588, lv = 1.94591014905531,
                   prop.err = 0.15, add.err = 0.6))

    tmp3 <- rxFixPop(tmp2, returnNull = TRUE)

    expect_true(is.null(tmp3))

    nlmixr_threecmt_mm_no_add_wtcl_pdtg_kout_delay2 <- function() {
      ini({
        tf_sc <- log(999)
        tf_infilt <- log(999)
        tka_sc <- log(999)
        tka_infilt <- log(999)
        tcl_low <- log(999)
        tcl_high <- log(999)
        tcl_c50 <- log(3000)
        e_wt_cl <- fixed(999)
        tv <- log(999)
        tq1 <- log(999)
        tvp1 <- log(10)
        tq2 <- log(999)
        tvp2 <- log(20)
        eta_cl~999
        eta_v~999
        prop_err <- 999
        tg_bl <- log(999)
        eta_tg_bl~999
        tg_kel <- log(999)
        tg_ec50 <- log(5000)
        tg_emax_kel <- log(2)
        ktr_tg <- log(999)
        prop_err_tg <- 999
      })
      model({
        # PK setup
        f_sc <- exp(tf_sc)
        f_infilt <- exp(tf_infilt)
        ka_sc <- exp(tka_sc)
        ka_infilt <- exp(tka_infilt)
        cl_low <- exp(tcl_low + eta_cl)*(WEIGHT_BL/85)^e_wt_cl
        cl_high <- exp(tcl_high + eta_cl)*(WEIGHT_BL/85)^e_wt_cl
        cl_c50 <- exp(tcl_c50)
        v <- exp(tv + eta_v)
        q1 <- exp(tq1)
        vp1 <- exp(tvp1)
        q2 <- exp(tq2)
        vp2 <- exp(tvp2)
        # PK micro-parameters
        ke_low <- cl_low/v
        ke_high <- cl_high/v
        kc_p1 <- q1/v
        kp1_c <- q1/vp1
        kc_p2 <- q2/v
        kp2_c <- q2/vp2
        # TG setup
        tgbl <- exp(tg_bl + eta_tg_bl)
        kin_tg <- tgbl*exp(tg_kel)
        ktr_TG <- exp(ktr_tg)
        TG(0) <- tgbl
        # differential equations
        cp <- CENTRAL/v*1e3 # 1e3 is for unit conversion
        ke <- ke_low + (ke_high - ke_low)*cp/(cp + cl_c50)
        kout_tg <- exp(tg_kel) + exp(tg_emax_kel)*TG_TR/(TG_TR + exp(tg_ec50))
        d/dt(IVINFILT) =             - ka_infilt * IVINFILT
        d/dt(SC)       = -ka_sc * SC
        d/dt(CENTRAL)  =  ka_sc * SC + ka_infilt * IVINFILT - ke*CENTRAL - kc_p1*CENTRAL + kp1_c*P1 - kc_p2*CENTRAL + kp2_c*P2
        d/dt(P1) =                                                         kc_p1*CENTRAL - kp1_c*P1
        d/dt(P2) =                                                                                    kc_p2*CENTRAL - kp2_c*P2
        f(SC) <- f_sc
        f(IVINFILT) <- f_infilt
        # TG transit model
        d/dt(TG_TR) = ktr_tg*cp - ktr_tg*TG_TR
        d/dt(TG) = kin_tg - kout_tg*TG
        # Residual error models
        cp ~ prop(prop_err)
        TG ~ prop(prop_err_tg)
      })
    }

    tmp <- rxFixPop(nlmixr_threecmt_mm_no_add_wtcl_pdtg_kout_delay2)

    expect_equal(tmp$theta,
                 c(tf_sc = 6.90675477864855, tf_infilt = 6.90675477864855, tka_sc = 6.90675477864855,
                   tka_infilt = 6.90675477864855, tcl_low = 6.90675477864855, tcl_high = 6.90675477864855,
                   tcl_c50 = 8.00636756765025, tv = 6.90675477864855, tq1 = 6.90675477864855,
                   tvp1 = 2.30258509299405, tq2 = 6.90675477864855, tvp2 = 2.99573227355399,
                   prop_err = 999, tg_bl = 6.90675477864855, tg_kel = 6.90675477864855,
                   tg_ec50 = 8.51719319141624, tg_emax_kel = 0.693147180559945,
                   ktr_tg = 6.90675477864855, prop_err_tg = 999))


  })
})
