rxTest({
  test_that("issue nlmixr#501", {

    nlmixr_threecmt_mm_no_add_wtcl_pdtg_kout_delay <- function() {
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

        # differential equations
        cp <- CENTRAL/v*1e3 # 1e3 is for unit conversion
        ke <- ke_low + (ke_high - ke_low)*cp/(cp + cl_c50)
        d/dt(IVINFILT) =             - ka_infilt * IVINFILT
        d/dt(SC)       = -ka_sc * SC
        d/dt(CENTRAL)  =  ka_sc * SC + ka_infilt * IVINFILT - ke*CENTRAL - kc_p1*CENTRAL + kp1_c*P1 - kc_p2*CENTRAL + kp2_c*P2
        d/dt(P1) =                                                         kc_p1*CENTRAL - kp1_c*P1
        d/dt(P2) =                                                                                    kc_p2*CENTRAL - kp2_c*P2

        f(SC) <- f_sc
        f(IVINFILT) <- f_infilt

        # TG Emax model
        tgbl <- exp(tg_bl + eta_tg_bl)
        kin_tg <- tgbl*exp(tg_kel)
        TG(0) <- tgbl
        ktr_TG <- exp(ktr_tg)
        d/dt(TG_TR) = ktr_tg*cp - ktr_tg*TG_TR
        kout_tg <- exp(tg_kel) + exp(tg_emax_kel)*TG_TR/(TG_TR + exp(tg_ec50))
        d/dt(TG) = kin_tg - kout_tg*TG

        # Residual error models
        cp ~ prop(prop_err)
        TG ~ prop(prop_err_tg)
      })
    }

    expect_error(rxode2(nlmixr_threecmt_mm_no_add_wtcl_pdtg_kout_delay), NA)

    one.compartment <- function() {
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45    # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
      keep = "WT"
      drop = "depot"
    }

    expect_warning(rxode2(one.compartment))


    one.compartment <- function() {
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45    # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45    # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
      keep = "WT"
      drop = "depot"
    }

    expect_error(rxode2(one.compartment), "ini")


    one.compartment <- function() {
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        add.sd <- 4
        cp ~ add(add.sd)
      })
      keep = "WT"
      drop = "depot"
    }

    expect_error(rxode2(one.compartment), NA)


    one.compartment <- function() {
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45    # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      keep = "WT"
      drop = "depot"
    }

    expect_error(rxode2(one.compartment), "model")


    one.compartment <- function() {
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45    # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
      keep = "WT"
      drop = "depot"
    }

    expect_error(rxode2(one.compartment), "model")

  })


  test_that("Duplicate parameters raise errors", {

    uif <- function() {
      ini({
        lCL <- 1.37
        lV <- 4.19
        lCLD <- 1.37
        lVT <- 3.87
        prop.err <- 1
        eta.Cl ~ 0.1
        eta.V ~ 0.1
        ## Duplicate CLs
        eta.Cl ~ 0.1
        eta.VT ~ 0.1
      })
      model({
        CL <- exp(lCL + eta.Cl)
        V <- exp(lV + eta.V)
        CLD <- exp(lCLD + eta.Cl)
        VT <- exp(lVT + eta.VT)
        linCmt() ~ prop(prop.err)
      })
    }

    expect_error(rxode2(uif), rex::rex("duplicated parameter(s): 'eta.Cl'"))
  })

  test_that("Un-estimated paramteres raise errors", {

    uif.ode <- function() {
      ini({
        lCL <- 1.37
        lV <- 4.19
        lCLD <- 1.37
        lVT <- 3.87
        ## Prop error isn't estimated
        prop.err <- 1
        add.err <- 0.1
        eta.Cl + eta.V ~ c(
          0.1,
          0.01, 0.01
        )
      })
      model({
        CL <- exp(lCL + eta.Cl)
        V <- exp(lV + eta.V)
        CLD <- exp(lCLD)
        VT <- exp(lVT)
        K10 <- CL / V
        K12 <- CLD / V
        K21 <- CLD / VT
        d / dt(centr) <- K21 * periph - K12 * centr - K10 * centr
        d / dt(periph) <- -K21 * periph + K12 * centr
        cp <- centr / V
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif.ode), rex::rex("the following parameter(s) were in the ini block but not in the model block: prop.err"))

    uif <- function() {
      ini({
        tka <- exp(0.5)
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        ## Should be eta.cl
        eta.v ~ 0.2
        add.err ~ 0.1
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif), rex::rex("endpoint parameter(s) missing, duplicated, or defined with '~'"))


    uif <- function() {
      ini({
        tka <- exp(0.5)
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        ## Should be eta.cl
        eta.v ~ 0.2
        add.err <- 0.1
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif), rex::rex("the following parameter(s) were in the ini block but not in the model block: eta.v"))

  })

  test_that("Residuals are population parameters", {

    uif <- function() {
      ini({
        tka <- exp(0.5)
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        eta.cl ~ 0.2
        add.err ~ 0.1
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif), rex::rex("endpoint parameter(s) missing, duplicated, or defined with '~'"))

  })

  test_that("Parameters need to be named", {

    uif <- function() {
      ini({
        tka <- exp(0.5)
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        eta.cl ~ 0.2
        ## Should be assign since it is a THETa, should I support it....?
        0.1
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }
    ## , rex::rex("The following THETAs are unnamed: THETA[4]")

    expect_error(
      expect_message(
        rxode2(uif),
        regexp="bad matrix specification"
      ),
      regexp="lotri syntax errors above"
    )

    uif <- function() {
      ini({
        tka <- exp(0.5)
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        ~0.2
        ## Should be assign since it is a THETa, should I support it....?
        add.err <- 0.1
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    ## rex::rex("The following ETAs are unnamed: ETA[2]")
    expect_error(
      expect_message(
        rxode2(uif),
        rex::rex("matrix expression should be 'name ~ c(lower-tri)'")
      ),
      regexp="lotri syntax errors above"
    )

  })

  test_that("Parameters cannot be missing or Infinite", {

    uif <- function() {
      ini({
        tka <- 1 / 0
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        eta.cl ~ 0.2
        add.err <- 4
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif), rex::rex("infinite/NA initial parameters: tka"))

    uif <- function() {
      ini({
        tka <- NA
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        eta.cl ~ 0.2
        add.err <- 4
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    expect_error(
      expect_message(
        rxode2(uif),
        regexp=rex::rex("estimate syntax unsupported: tka <- NA")
      ),
      regexp="lotri syntax errors above"
    )

    uif <- function() {
      ini({
        tka <- 3
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        eta.cl ~ 0.2
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif), rex::rex("endpoint 'cp' needs the following parameters estimated or modeled"))

    uif <- function() {
      ini({
        tka <- 3
        tcl <- exp(-3.2)
        tv <- exp(1)
        eta.ka ~ 0.1
        eta.cl ~ 0.2
      })
      model({
        ka <- tka + eta.ka
        cl <- tcl + eta.cl
        v <- tv
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        add.err <- ka + cl
        cp ~ add(add.err)
      })
    }

    expect_error(rxode2(uif), NA)

  })


  test_that("modeled endpoints", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        eta.v ~ 0.01 # log V
        ## the label("Label name") works with all models
        add.sd <- 0.7
        add.sd2 <- 0.7
        tprop <- 0.5
        prop.eta ~ 0.01
      })
      model({
        ka <- tka
        cl <- tcl
        v <- eta.v
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        prop.sd <- exp(tprop + prop.eta)
        cp ~ add(add.sd)
        cp2 ~ add(add.sd2)
      })
    }


    expect_error(ocmt())

    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        eta.v ~ 0.01 # log V
        ## the label("Label name") works with all models
        add.sd <- 0.7
        add.sd2 <- 0.7
        tprop <- 0.5
        prop.eta ~ 0.01
        df <- 3
      })
      model({
        ka <- tka
        cl <- tcl
        v <- eta.v
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        prop.sd <- exp(tprop + prop.eta)
        cp ~ add(add.sd)
        cp2 ~ add(add.sd2) + dt(df)
      })
    }

  })


  test_that("if/else in endpoints should error", {
    myModel_OS <- function(){           # TTE model for OS. Weibull baseline hazard.
      ini({

        tLAM <- log(0.001)               # scale parameter
        tSHP <- log(2)                   # shape parameter
        tBSLD0 <- log(0.1)               # parameter for SLD (sum of longest diameter) at enrolment
        tBTSR <- log(0.1)                # parameter for TSR(t) (tumour size ratio)
        tBNWLS <- log(0.1)               # parameter for NewLesion(t)
        tBECOG <- log(0.1)               # parameter for ECOG at enrolment
        eta.LAM ~ 0.01                   # inter-individual variability in scale parameter
        add.err <- 0

      })
      model({

        LAM = exp(tLAM + eta.LAM)
        SHP = exp(tSHP)
        BSLD0 = exp(tBSLD0)
        BTSR = exp(tBTSR)
        BNWLS = exp(tBNWLS)
        BECOG = exp(tBECOG)

        #Time constant covariates
        TVSLD0 = 70                      # average SLD at enrolment
        NSLD0 = SLD0/TVSLD0              # normalised SLD at enrolment
        IECOG=ECOG                       # ECOG at enrolment

        # Model for dSLD(t)
        A(0) = IBASE*1000                # SLD baseline
        MMBAS = IBASE*1000               # Parameter of the SLD(t) model were estimated previously (IPP approach)
        d/dt(A) = KG/1000 * A - (KD0/1000 * AUC0 + + KD1/100 * AUC1) * A
        TUM = A
        TSR = (TUM-MMBAS)/MMBAS

        if(time==0){                     #TSR(t) for t<= week 12 and TSR(week12) for t> week 12
          WTS = 0
          TM12 = 0
        }
        if(time<=84){
          WTS = TSR
          TM12 = WTS
        }
        else {
          WTS = WTS
        }

        # survival model
        DEL = 1E-6
        d/dt(A2) = LAM*SHP*(LAM*(T+DEL))**(SHP-1) *exp(BSLD0*NSLD0+BTSR*WTS+BNWLS*NWLS+BECOG*IECOG)

        # Death hazard
        CHZ = A2
        SUR=exp(-CHZ)
        DELX = 1E-6
        xTUM = A
        xTSR = (xTUM-MMBAS)/MMBAS
        if(time==0){
          XWTS=0
          XTM12=0
        }

        if(time<=84){
          XWTS = XTSR
          XTM12 = XWTS
        }
        else {
          XWTS=XWTS
        }

        HAZN = LAM*SHP*(LAM*(TIME+DELx))**(SHP-1)*exp(BSLD0*NSLD0+BTSR*XWTS+BNWLS*NWLS+BECOG*IECOG)
        # prediction
        if(FLAG == 9 & EVID == 0 & OSCENS == 1){
          IPRED=SUR                      # probability of survival (censored event)
          Y = IPRED                      # Y is probability for TTE data
          Y ~ add(add.err)
        }
        if(FLAG == 9 & EVID == 0 & OSCENS == 0){
          IPRED=SUR*HAZN                # probability of event (death) at time=TIME
          Y = IPRED                     # Y is probability for TTE data
        }
        Y ~ add(add.err)
      })
    }

    expect_error(rxode2(myModel_OS))

    expect_error(myModel_OS())


  })
})
