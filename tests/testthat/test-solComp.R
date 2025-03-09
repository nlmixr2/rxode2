test_that("test the matrices of the linear compartment solutions .solComp2", {

  v <- list(L = c(4.07546291005291, 0.0245370899470903),
            C1 = structure(c(0.759200006770939,-0.740571447916969,
                             0.240799993229061, 0.740571447916969),
                           dim = c(2L, 2L)),
            C2 = structure(c(-0.246857149305656, 0.240799993229061,
                             0.246857149305656, 0.759200006770939),
                           dim = c(2L, 2L)))

  expect_equal(.solComp2(k10=0.1, k12=3, k21=1), v)

  v <- list(L = c(5.8977832399092, 0.0122878774411843, 0.689928882649618),
            C1 = structure(c(0.86252789433926, -0.528317313419071, -0.319585969277925,
                             0.120784508040635, 0.366861472939276, 0.495310665672737,
                             0.016687597620105, 0.161455840479795, -0.175724696394812),
                           dim = c(3L, 3L)),
            C2 = structure(c(-0.17610577113969, 0.107868659665074, 0.0652511460029109,
                             0.122287157646425, 0.371425504011093, 0.501472700759773,
                             0.053818613493265, 0.520705836323833, -0.566723846762684),
                           dim = c(3L, 3L)),
            C3 = structure(c(-0.0798964923194813, 0.0489383595021832, 0.0296034459956658,
                             0.123827666418184, 0.37610452556983, 0.507789987948271,
                             -0.043931174098703, -0.425042885072013, 0.462606566056063),
                           dim = c(3L,  3L)))

  expect_equal(.solComp3(k10=0.1, k12=3, k21=1, k13=2, k31=0.5),
               v)

})

if (requireNamespace("pmxTools", quietly = TRUE)) {

  f <- function(dt, CL=25, V=20, DOSE=100) {
    p1 <- CL
    v1 <- V
    p2 <- 0
    p3 <- 0
    p4 <- 0
    p5 <- 0
    ka <- 0
    alastNV <- DOSE
    rateNV <- 0
    oral0 <- 0
    trans <- 1
    ncmt <- 1
    deriv <- FALSE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_1cmt_linear_bolus(CL=CL, V=V, t=dt, dose=DOSE), l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the one compartment linear compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })


  f <- function(dt, CL=25, V=20, KA=2, DOSE=100) {
    p1 <- CL
    v1 <- V
    p2 <- 0
    p3 <- 0
    p4 <- 0
    p5 <- 0
    ka <- KA
    alastNV <- c(DOSE, 0)
    rateNV <- c(0, 0)
    oral0 <- 1
    trans <- 1
    ncmt <- 1
    deriv <- FALSE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_1cmt_linear_oral_1(CL=CL, V=V, ka=KA, t=dt, dose=DOSE),
      l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the oral 1-cmt linear compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })


  f <- function(t, CL=25, V=20, DOSE=100, tinf=1) {
    p1 <- CL
    v1 <- V
    p2 <- 0
    p3 <- 0
    p4 <- 0
    p5 <- 0
    ka <- 0
    deriv <- FALSE
    if (t < tinf) {
      alastNV <- 0
      rateNV <- DOSE/tinf
      dt <- t
    } else {
      alastNV <- pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=tinf, dose=DOSE, tinf=tinf)*V # amt not conc
      rateNV <- 0
      dt <- t - tinf
    }
    oral0 <- 0
    trans <- 1
    ncmt <- 1
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
    l <- l$val
    c(s=pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t, dose=DOSE, tinf=tinf),
      l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the iv infusion 1-cmt linear compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })

  f <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- 0
    p5 <- 0
    ka <- 0
    alastNV <- c(DOSE, 0)
    rateNV <- c(0, 0)
    oral0 <- 0
    trans <- 1
    ncmt <- 2
    deriv <- FALSE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_2cmt_linear_bolus(CL=CL, V=V, V2=V2, Q=Q,
                                            t=dt, dose=DOSE), l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the two compartment linear compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })

  f <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100, tinf=1) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- 0
    p5 <- 0
    ka <- 0
    alastNV <- c(0, 0)
    rateNV <- DOSE/tinf
    oral0 <- 0
    trans <- 1
    ncmt <- 2
    extra <- 0
    deriv <- FALSE
    if (dt <= tinf) {
    } else {
      l <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
      dt <- dt - tinf
      extra <- tinf
      rateNV <- 0
      alastNV <- l$Alast
    }
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_2cmt_linear_infusion(CL=CL, V=V, V2=V2, Q=Q,
                                               t=dt+extra, dose=DOSE, tinf=tinf),
      l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the two compartment iv infusion linear compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })

  # 3 compartment tests
  f <- function(dt,
                V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                DOSE=100) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- Q2
    p5 <- V3
    ka <- 0
    alastNV <- c(DOSE, 0, 0)
    rateNV <- c(0, 0)
    oral0 <- 0
    trans <- 1
    ncmt <- 3
    deriv <- FALSE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_3cmt_linear_bolus(CL=CL, V=V, V2=V2, Q=Q,
                                            V3=V3, Q3=Q2,
                                            t=dt, dose=DOSE), l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the two compartment linear compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })

  # 3 compartment tests, oral
  f <- function(dt,
                V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                ka=2,
                DOSE=100) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- Q2
    p5 <- V3
    ka <- ka
    alastNV <- c(DOSE, 0, 0, 0)
    rateNV <- c(0, 0)
    oral0 <- 1
    trans <- 1
    ncmt <- 3
    deriv <- FALSE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_3cmt_linear_oral_1(CL=CL, V=V, V2=V2, Q=Q,
                                             V3=V3, Q3=Q2,
                                             ka=ka,
                                             t=dt, dose=DOSE), l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the three compartment linear oral compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })


  f <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                DOSE=100, tinf=1) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- Q2
    p5 <- V3
    ka <- 0
    alastNV <- c(0, 0, 0)
    rateNV <- DOSE/tinf
    oral0 <- 0
    trans <- 1
    ncmt <- 3
    extra <- 0
    deriv <- FALSE
    if (dt <= tinf) {
    } else {
      l <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
      dt <- dt - tinf
      extra <- tinf
      rateNV <- 0
      alastNV <- l$Alast
    }
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)$val
    c(s=pmxTools::calc_sd_3cmt_linear_infusion(CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q3=Q2,
                                               t=dt+extra, dose=DOSE, tinf=tinf),
      l=l)
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           test_that(paste0("test the three compartment linear infusion compartment solution at ", d), {
             v <- f(d)
             expect_equal(stats::setNames(v["s"], NULL),
                          stats::setNames(v["l"], NULL))
           })
         })


  ####################################################
  ## AD linCmt()
  ####################################################

  f0 <- function(dt, CL=25, V=20, DOSE=100,
                alastNV=c(DOSE, 0, 0)) {
    p1 <- CL
    v1 <- V
    p2 <- 0
    p3 <- 0
    p4 <- 0
    p5 <- 0
    ka <- 0
    rateNV <- 0
    oral0 <- 0
    trans <- 1
    ncmt <- 1
    deriv <- TRUE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
    l
    #c(s=pmxTools::calc_sd_1cmt_linear_bolus(CL=CL, V=V, t=dt, dose=DOSE), l=l$val)
  }

  f <-  function(dt, CL=25, V=20, DOSE=100) {
    test_that(paste0("one compartment bolus t=", dt, ";CL=", CL,
                     "; V=", V, "; DOSE=", DOSE), {
      t0 <- dt/2
      f1 <- f0(t0, CL=CL, V=V, DOSE=DOSE)
      expect_equal(pmxTools::calc_sd_1cmt_linear_bolus(CL=CL, V=V, t=t0, dose=DOSE),
                   f1$val)
      #
      f2 <- f0(t0, CL=CL, V=V, DOSE=DOSE, alastNV=f1$Alast)
      #
      f3 <- f0(t0*2, CL=CL, V=V, DOSE=DOSE)
      #
      expect_equal(pmxTools::calc_sd_1cmt_linear_bolus(CL=CL, V=V, t=dt, dose=DOSE),
                   f3$val)
      # Value is in concentration
      expect_equal(f2$val, f3$val)
      # Unadjusted Jacobian
      expect_equal(f2$J, f3$J)
      # Jacobian adjusted to concentration
      expect_equal(f2$Jg, f3$Jg)
      # Alast and gradients are in amounts
      expect_equal(f2$Alast, f3$Alast)
    })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d, CL=25, V=20, DOSE=100)
         })

  f0 <- function(dt, CL=25, V=20, KA=2, DOSE=100,
                 alastNV=c(DOSE, 0, 0, 0, 0, 0)) {
    p1 <- CL
    v1 <- V
    p2 <- 0
    p3 <- 0
    p4 <- 0
    p5 <- 0
    ka <- KA
    alastNV <- alastNV
    rateNV <- c(0, 0)
    oral0 <- 1
    trans <- 1
    ncmt <- 1
    deriv <- TRUE
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1,
               p2, p3,
               p4, p5,
               ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
    l
  }

  f <-  function(dt, CL=25, V=20, KA=2, DOSE=100) {
    test_that(paste0("one compartment oral t=", dt, ";CL=", CL,
                     "; V=", V, "; KA=", KA, "; DOSE=", DOSE), {
                       t0 <- dt/2
                       f1 <- f0(t0, CL=CL, V=V, KA=KA, DOSE=DOSE)
                       expect_equal(pmxTools::calc_sd_1cmt_linear_oral_1(CL=CL, V=V, ka=KA, t=t0, dose=DOSE),
                                    f1$val)
                       #
                       f2 <- f0(t0, CL=CL, V=V, KA=KA, DOSE=DOSE, alastNV=f1$Alast)
                       #
                       f3 <- f0(t0*2, CL=CL, V=V, KA=KA, DOSE=DOSE)
                       #
                       expect_equal(pmxTools::calc_sd_1cmt_linear_oral_1(CL=CL, V=V, ka=KA, t=dt, dose=DOSE),
                                    f3$val)
                       # Value is in concentration
                       expect_equal(f2$val, f3$val)
                       # Unadjusted Jacobian
                       expect_equal(f2$J, f3$J)
                       # Jacobian adjusted to concentration
                       expect_equal(f2$Jg, f3$Jg)
                       # Alast and gradients are in amounts
                       expect_equal(f2$Alast, f3$Alast)
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d, CL=25, V=20, KA=2, DOSE=100)
         })

  ## One compartment IV infusion

  f0i <- function(t, CL=25, V=20, DOSE=100, tinf=1,
                 alastNV=rep(0, 3), rateNV=DOSE/tinf) {
    p1 <- CL
    v1 <- V
    p2 <- 0
    p3 <- 0
    p4 <- 0
    p5 <- 0
    ka <- 0
    deriv <- TRUE
    oral0 <- 0
    trans <- 1
    ncmt <- 1
    if (all(rateNV == 0)) {
      # Now a simple eliminiation; Handles the case where the infusion has already past
      dt <- t
    } else if (t < tinf) {
      # infusion not complete
      dt <- t
    } else {
      #Infusion completes during the time
      v <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
      if (t == tinf) return(v)
      # Infusion is now complete
      alastNV <- v$Alast
      rateNV <- 0
      dt <- t - tinf
    }
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, CL=25, V=20, DOSE=100, tinf=1) {
    test_that(paste0("one compartment infusion t=", dt, ";CL=", CL,
                     "; V=", V, "; tinf=", tinf, "; DOSE=", DOSE), {
                       t0 <- dt/2
                       # See if more care is needed for IV comparison
                       if (dt < tinf) {
                         f1 <- f0i(t0, CL=CL, V=V, DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0, dose=DOSE, tinf=tinf),
                                      f1$val)
                         #
                         f2 <- f0i(t0, CL=CL, V=V, DOSE=DOSE, tinf=tinf, alastNV=f1$Alast)
                         #
                         f3 <- f0i(t0*2, CL=CL, V=V, DOSE=DOSE)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0*2, dose=DOSE, tinf=tinf),
                                      f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       } else if (t0 > tinf) {
                         rateNV <- DOSE/tinf
                         f1 <- f0i(t0, CL=CL, V=V, DOSE=DOSE, tinf=tinf, rateNV=rateNV)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0, dose=DOSE, tinf=tinf),
                                      f1$val)
                         #
                         f2 <- f0i(t0, CL=CL, V=V, DOSE=DOSE, tinf=tinf, rateNV=0,
                                  alastNV=f1$Alast)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0*2, dose=DOSE, tinf=tinf),
                                      f2$val)
                         f3 <- f0i(t0*2, CL=CL, V=V, DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0*2, dose=DOSE, tinf=tinf),
                                      f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       } else {
                         rateNV <- DOSE/tinf
                         f1 <- f0i(t0, CL=CL, V=V, DOSE=DOSE, tinf=tinf, rateNV=rateNV)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0, dose=DOSE, tinf=tinf),
                                      f1$val)
                         f3 <- f0i(t0*2, CL=CL, V=V, DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0*2, dose=DOSE, tinf=tinf),
                                      f3$val)
                         tinf2 <- tinf-t0
                         f2 <- f0i(t0, CL=CL, V=V, DOSE=DOSE, tinf=tinf2, rateNV=rateNV,
                                  alastNV=f1$Alast)
                         expect_equal(pmxTools::calc_sd_1cmt_linear_infusion(CL=CL, V=V, t=t0*2, dose=DOSE, tinf=tinf),
                                      f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       }
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d, CL=25, V=20, tinf=1, DOSE=100)
         })

  ## Two compartment bolus
  f0 <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100,
                 alastNV=c(DOSE, rep(0, 9))) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- 0
    p5 <- 0
    ka <- 0
    alastNV <- alastNV
    rateNV <- c(0, 0)
    oral0 <- 0
    trans <- 1
    ncmt <- 2
    deriv <- TRUE
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100) {
    test_that(paste0("two compartment bolus t=", dt, ";CL=", CL,
                     "; V=", V, "; Q=", Q, "; V2=", V2, "; DOSE=", DOSE), {
                       t0 <- dt/2
                       f1 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, DOSE=DOSE)
                       expect_equal(pmxTools::calc_sd_2cmt_linear_bolus(CL=CL, V=V,
                                                                        V2=V2, Q=Q,
                                                                        t=t0, dose=DOSE),
                                    f1$val)
                       #
                       f2 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2,
                                DOSE=DOSE, alastNV=f1$Alast)
                       #
                       f3 <- f0(t0*2, CL=CL, V=V, Q=Q, V2=V2,
                                DOSE=DOSE)
                       #
                       expect_equal(
                         pmxTools::calc_sd_2cmt_linear_bolus(CL=CL, V=V, V2=V2, Q=Q,
                                                             t=dt, dose=DOSE),
                                    f3$val)
                       # Value is in concentration
                       expect_equal(f2$val, f3$val)
                       # Unadjusted Jacobian
                       expect_equal(f2$J, f3$J)
                       # Jacobian adjusted to concentration
                       expect_equal(f2$Jg, f3$Jg)
                       # Alast and gradients are in amounts
                       expect_equal(f2$Alast, f3$Alast)
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d,V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100)
         })

  ## Two compartment oral
  f0 <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100, ka=2,
                 alastNV=c(DOSE, rep(0, 13))) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- 0
    p5 <- 0
    ka <- ka
    alastNV <- alastNV
    rateNV <- c(0, 0)
    oral0 <- 1
    trans <- 1
    ncmt <- 2
    deriv <- TRUE
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, V = 40, CL = 18, V2 = 297, Q = 10, ka=2, DOSE=100) {
    test_that(paste0("two compartment oral t=", dt, ";CL=", CL,
                     "; V=", V, "; Q=", Q, "; V2=", V2, "; ka=", ka, "DOSE=", DOSE), {
                       t0 <- dt/2
                       f1 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, ka=ka, DOSE=DOSE)
                       expect_equal(pmxTools::calc_sd_2cmt_linear_oral_1(
                         CL=CL, V=V,
                         V2=V2, Q=Q, ka=ka,
                         t=t0, dose=DOSE),
                         f1$val)
                       #
                       f2 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, ka=ka,
                                DOSE=DOSE, alastNV=f1$Alast)
                       #
                       f3 <- f0(t0*2, CL=CL, V=V, Q=Q, V2=V2, ka=ka,
                                DOSE=DOSE)
                       #
                       expect_equal(
                         pmxTools::calc_sd_2cmt_linear_oral_1(
                           CL=CL, V=V, V2=V2, Q=Q, ka=ka,
                           t=dt, dose=DOSE),
                         f3$val)
                       # Value is in concentration
                       expect_equal(f2$val, f3$val)
                       # Unadjusted Jacobian
                       expect_equal(f2$J, f3$J)
                       # Jacobian adjusted to concentration
                       expect_equal(f2$Jg, f3$Jg)
                       # Alast and gradients are in amounts
                       expect_equal(f2$Alast, f3$Alast)
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d,V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100)
         })

  ## Two compartment IV infusion

  f0i <- function(t, CL=25, V=20, V2 = 297, Q = 10, DOSE=100, tinf=1,
                  alastNV=rep(0, 10), rateNV=DOSE/tinf) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- 0
    p5 <- 0
    ka <- 0
    deriv <- TRUE
    oral0 <- 0
    trans <- 1
    ncmt <- 2
    if (all(rateNV == 0)) {
      # Now a simple eliminiation; Handles the case where the infusion has already past
      dt <- t
    } else if (t < tinf) {
      # infusion not complete
      dt <- t
    } else {
      #Infusion completes during the time
      v <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
      if (t == tinf) return(v)
      # Infusion is now complete
      alastNV <- v$Alast
      rateNV <- 0
      dt <- t - tinf
    }
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, CL=25, V=20, V2 = 297, Q = 10, DOSE=100, tinf=1) {
    test_that(paste0("two compartment infusion t=", dt, ";CL=", CL,
                     "; V2 = ", V2, "; Q = ", Q,
                     "; V=", V, "; tinf=", tinf, "; DOSE=", DOSE), {
                       t0 <- dt/2
                       # See if more care is needed for IV comparison
                       if (dt < tinf) {
                         f1 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q, DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2,
                           t=t0, dose=DOSE, tinf=tinf),
                           f1$val)
                         #
                         f2 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q,
                                   DOSE=DOSE, tinf=tinf, alastNV=f1$Alast)
                         #
                         f3 <- f0i(t0*2, CL=CL, V=V, Q=Q, V2=V2, DOSE=DOSE)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2,
                           t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       } else if (t0 > tinf) {
                         rateNV <- DOSE/tinf
                         f1 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q,
                                   DOSE=DOSE, tinf=tinf, rateNV=rateNV)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, t=t0, dose=DOSE, tinf=tinf),
                           f1$val)
                         #
                         f2 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q,
                                   DOSE=DOSE, tinf=tinf, rateNV=0,
                                   alastNV=f1$Alast)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, t=t0*2, dose=DOSE, tinf=tinf),
                                      f2$val)
                         f3 <- f0i(t0*2, CL=CL, V=V, Q=Q, V2=V2,
                                   DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, t=t0*2, dose=DOSE, tinf=tinf),
                                      f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       } else {
                         rateNV <- DOSE/tinf
                         f1 <- f0i(t0, CL=CL, V=V, Q=Q, V2=V2,
                                   DOSE=DOSE, tinf=tinf, rateNV=rateNV)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, t=t0, dose=DOSE, tinf=tinf),
                           f1$val)
                         f3 <- f0i(t0*2, CL=CL, V=V, Q=Q, V2=V2,
                                   DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         tinf2 <- tinf-t0
                         f2 <- f0i(t0, CL=CL, V=V, Q=Q, V2=V2,
                                   DOSE=DOSE, tinf=tinf2, rateNV=rateNV,
                                   alastNV=f1$Alast)
                         expect_equal(pmxTools::calc_sd_2cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       }
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d, CL=25, V=20, tinf=1, DOSE=100)
         })

  ## Three compartment model bolus
  f0 <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                 DOSE=100,
                 alastNV=c(DOSE, rep(0, 20))) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- Q2
    p5 <- V3
    ka <- 0
    alastNV <- alastNV
    rateNV <- c(0, 0)
    oral0 <- 0
    trans <- 1
    ncmt <- 3
    deriv <- TRUE
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, DOSE=100) {
    test_that(paste0("two compartment bolus t=", dt, ";CL=", CL,
                     "; V=", V, "; Q=", Q, "; V2=", V2, "; Q2=", Q2,
                     "; V3=", V3,
                     "; DOSE=", DOSE), {
                       t0 <- dt/2
                       f1 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3, DOSE=DOSE)
                       expect_equal(pmxTools::calc_sd_3cmt_linear_bolus(
                         CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q3=Q2,
                         t=t0, dose=DOSE),
                         f1$val)
                       #
                       f2 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3,
                                DOSE=DOSE, alastNV=f1$Alast)
                       #
                       f3 <- f0(t0*2, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3,
                                DOSE=DOSE)
                       #
                       expect_equal(
                         pmxTools::calc_sd_3cmt_linear_bolus(
                           CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q3=Q2,
                           t=dt, dose=DOSE),
                         f3$val)
                       # Value is in concentration
                       expect_equal(f2$val, f3$val)
                       # Unadjusted Jacobian
                       expect_equal(f2$J, f3$J)
                       # Jacobian adjusted to concentration
                       expect_equal(f2$Jg, f3$Jg)
                       # Alast and gradients are in amounts
                       expect_equal(f2$Alast, f3$Alast)
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d,V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100)
         })


  ## Three compartment model oral
  f0 <- function(dt, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                 ka=2,
                 DOSE=100,
                 alastNV=c(DOSE, rep(0, 25))) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- Q2
    p5 <- V3
    ka <- ka
    alastNV <- alastNV
    rateNV <- c(0, 0)
    oral0 <- TRUE
    trans <- 1
    ncmt <- 3
    deriv <- TRUE
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                 ka=2, DOSE=100) {
    test_that(paste0("three compartment oral t=", dt, ";CL=", CL,
                     "; V=", V, "; Q=", Q, "; V2=", V2, "; ka=", ka,
                     "; Q2=", Q2, "; V3=", V3, "; ka=", ka,
                     "; DOSE=", DOSE), {
                       t0 <- dt/2
                       f1 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3, ka=ka, DOSE=DOSE)
                       expect_equal(pmxTools::calc_sd_3cmt_linear_oral_1(
                         CL=CL, V=V,
                         V2=V2, Q=Q, Q3=Q2, V3=V3, ka=ka,
                         t=t0, dose=DOSE),
                         f1$val)
                       #
                       f2 <- f0(t0, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3, ka=ka,
                                DOSE=DOSE, alastNV=f1$Alast)
                       #
                       f3 <- f0(t0*2, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3, ka=ka,
                                DOSE=DOSE)
                       #
                       expect_equal(
                         pmxTools::calc_sd_3cmt_linear_oral_1(
                           CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q3=Q2, ka=ka,
                           t=dt, dose=DOSE),
                         f3$val)
                       # Value is in concentration
                       expect_equal(f2$val, f3$val)
                       # Unadjusted Jacobian
                       expect_equal(f2$J, f3$J)
                       # Jacobian adjusted to concentration
                       expect_equal(f2$Jg, f3$Jg)
                       # Alast and gradients are in amounts
                       expect_equal(f2$Alast, f3$Alast)
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d,V = 40, CL = 18, V2 = 297, Q = 10, DOSE=100)
         })


  ## Three compartment IV infusion
  f0i <- function(t, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                  DOSE=100, tinf=1,
                  alastNV=rep(0, 21),
                  rateNV=DOSE/tinf) {
    p1 <- CL
    v1 <- V
    p2 <- Q
    p3 <- V2
    p4 <- Q2
    p5 <- V3
    ka <- 0
    deriv <- TRUE
    oral0 <- 0
    trans <- 1
    ncmt <- 3
    if (all(rateNV == 0)) {
      # Now a simple eliminiation; Handles the case where the infusion has already past
      dt <- t
    } else if (t < tinf) {
      # infusion not complete
      dt <- t
    } else {
      #Infusion completes during the time
      v <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
      if (t == tinf) return(v)
      # Infusion is now complete
      alastNV <- v$Alast
      rateNV <- 0
      dt <- t - tinf
    }
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv)
  }

  f <-  function(dt, V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400,
                 DOSE=100, tinf=1) {
    test_that(paste0("three compartment infusion t=", dt, ";CL=", CL,
                     "; V2 = ", V2, "; Q = ", Q,
                     "; V3 = ", V3, "; Q2 = ", Q2,
                     "; V=", V, "; tinf=", tinf, "; DOSE=", DOSE), {
                       t0 <- dt/2
                       # See if more care is needed for IV comparison
                       if (dt < tinf) {
                         f1 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q2=Q2,
                                   DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0, dose=DOSE, tinf=tinf),
                           f1$val)
                         #
                         f2 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q2=Q2,
                                   DOSE=DOSE, tinf=tinf, alastNV=f1$Alast)
                         #
                         f3 <- f0i(t0*2, CL=CL, V=V, Q=Q, V2=V2, V3=V3, Q2=Q2,
                                   DOSE=DOSE)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       } else if (t0 > tinf) {
                         rateNV <- DOSE/tinf
                         f1 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q2=Q2,
                                   DOSE=DOSE, tinf=tinf, rateNV=rateNV)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0, dose=DOSE, tinf=tinf),
                           f1$val)
                         f2 <- f0i(t0, CL=CL, V=V, V2=V2, Q=Q, V3=V3, Q2=Q2,
                                   DOSE=DOSE, tinf=tinf, rateNV=0,
                                   alastNV=f1$Alast)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0*2, dose=DOSE, tinf=tinf),
                           f2$val)
                         f3 <- f0i(t0*2, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3,
                                   DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       } else {
                         rateNV <- DOSE/tinf
                         f1 <- f0i(t0, CL=CL, V=V, Q=Q, V2=V2, V3=V3, Q2=Q2,
                                   DOSE=DOSE, tinf=tinf, rateNV=rateNV)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0, dose=DOSE, tinf=tinf),
                           f1$val)
                         f3 <- f0i(t0*2, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3,
                                   DOSE=DOSE, tinf=tinf)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         tinf2 <- tinf-t0
                         f2 <- f0i(t0, CL=CL, V=V, Q=Q, V2=V2, Q2=Q2, V3=V3,
                                   DOSE=DOSE, tinf=tinf2, rateNV=rateNV,
                                   alastNV=f1$Alast)
                         expect_equal(pmxTools::calc_sd_3cmt_linear_infusion(
                           CL=CL, V=V, Q=Q, V2=V2, Q3=Q2, V3=V3,
                           t=t0*2, dose=DOSE, tinf=tinf),
                           f3$val)
                         # Unadjusted Jacobian
                         expect_equal(f2$J, f3$J)
                         # Jacobian adjusted to concentration
                         expect_equal(f2$Jg, f3$Jg)
                         # Alast and gradients are in amounts
                         expect_equal(f2$Alast, f3$Alast)
                         # Value is in concentration
                         expect_equal(f2$val, f3$val)
                       }
                     })
  }

  lapply(seq(.1, 10, by=0.1),
         function(d) {
           f(d, CL=25, V=20, tinf=1, DOSE=100)
         })


 }
