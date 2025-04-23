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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
      l <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
      dt <- dt - tinf
      extra <- tinf
      rateNV <- 0
      alastNV <- l$Alast
    }
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
      l <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
      dt <- dt - tinf
      extra <- tinf
      rateNV <- 0
      alastNV <- l$Alast
    }
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
               ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
      v <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
      if (t == tinf) return(v)
      # Infusion is now complete
      alastNV <- v$Alast
      rateNV <- 0
      dt <- t - tinf
    }
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
      v <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
      if (t == tinf) return(v)
      # Infusion is now complete
      alastNV <- v$Alast
      rateNV <- 0
      dt <- t - tinf
    }
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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
      v <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
      if (t == tinf) return(v)
      # Infusion is now complete
      alastNV <- v$Alast
      rateNV <- 0
      dt <- t - tinf
    }
    .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans, deriv, 0L, 0, 0, 0, 0)
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

testVal <- function(rx, val) {
  state <- rx$state
  state <- setNames(seq_along(state), state)

  expect_equal(.getCmtNum(rx),
               state[val])
}

testInf <- function(rx, val) {
  state <- rx$state
  state <- setNames(seq_along(state), state)
  .num <- .getCmtNum(rx)
  expect_equal(setNames(cmtSupportsInfusion_(.num, rxModelVars(rx)),
                        names(.getCmtNum(rx))),
               val)
}

testOff <- function(rx, val) {
  state <- rx$state
  state <- setNames(seq_along(state), state)
  .num <- .getCmtNum(rx)
  expect_equal(setNames(cmtSupportsOff_(.num, rxModelVars(rx)),
                        names(.num)),
               val)
}

test_that("rxode2 parsing of linCmt() 1 compartment with ka", {

  rx <- function() {
    ini({
      popCl <- 1
      popV <- 20
      bsvCl ~ 0.1
      bsvV ~ 0.1
      popKeo <- 1.4
      bsvKeo ~ 0.1
      popKa <- 1
    })
    model({
      cl ~ popCl * exp(bsvCl)
      v ~ popV * exp(bsvV)
      ka ~ popKa
      keo ~ popKeo * exp(bsvKeo)
      popLagCentral <- 0
      popRateCentral <- 0
      popDurCentral <- 0
      bsvLagCentral <- 0
      bsvRateCentral <- 0
      bsvDurCentral <- 0
      alag(central) <- popLagCentral * exp(bsvLagCentral)
      rate(central) <- popRateCentral * exp(bsvRateCentral)
      dur(central) <- popDurCentral * exp(bsvDurCentral)
      cp <- linCmt()
      d/dt(ce) = keo*(cp-ce)
      effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    })
  }


  rx <- rxode2({
    popCl <- 1
    popV <- 20
    bsvCl <-0
    bsvV <- 0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    popKa <- 1
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    ka ~ popKa
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
  })

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 0L, numLin = 2L, depotLin = 1L))

  testVal(rx, c("depot", "central", "ce"))

  testInf(rx, c(depot=TRUE, central=TRUE, ce=TRUE))

  testOff(rx, c(depot=FALSE, central=FALSE, ce=TRUE))

  expect_equal(rx$state, c("ce", "depot", "central"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtA[(]", rxModelVars(rx)$model["normModel"]))

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popKa <- 1
    bsvCl <-0
    bsvV <- 0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    ka ~ popKa
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    b=central
    c=peripheral1
  }, linCmtSens="linCmtB")

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 4L, numLin = 2L, depotLin = 1L))

  testVal(rx, c("depot", "central", "ce",
                "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                "rx__sens_central_BY_ka", "rx__sens_depot_BY_ka"))

  testInf(rx, c(depot=TRUE, central=TRUE, ce=TRUE,
                rx__sens_central_BY_p1=FALSE, rx__sens_central_BY_v1=FALSE,
                rx__sens_central_BY_ka=FALSE, rx__sens_depot_BY_ka=FALSE))

  testOff(rx, c(depot=FALSE, central=FALSE, ce=TRUE,
                rx__sens_central_BY_p1=FALSE, rx__sens_central_BY_v1=FALSE,
                rx__sens_central_BY_ka=FALSE, rx__sens_depot_BY_ka=FALSE))



  expect_equal(rx$state, c("ce",
                           "depot", "central",
                           "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                           "rx__sens_central_BY_ka",
                           "rx__sens_depot_BY_ka"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtB[(]", rxModelVars(rx)$model["normModel"]))
})


test_that("rxode2 parsing of linCmt() 1 compartment without ka", {

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    bsvCl <-0
    bsvV <- 0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
  })

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 0L, numLin = 1L, depotLin = 0L))

  testVal(rx, c("central", "ce"))

  testInf(rx, c(central=TRUE, ce=TRUE))

  testOff(rx, c(central=FALSE, ce=TRUE))

  expect_equal(rx$state, c("ce", "central"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtA[(]", rxModelVars(rx)$model["normModel"]))

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    bsvCl <-0
    bsvV <- 0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    b=central
    c=peripheral1
  }, linCmtSens="linCmtB")

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 2L, numLin = 1L, depotLin = 0L))

  testVal(rx, c("central","ce",
                "rx__sens_central_BY_p1", "rx__sens_central_BY_v1"))

  testInf(rx, c(central=TRUE, ce=TRUE,
                rx__sens_central_BY_p1=FALSE, rx__sens_central_BY_v1=FALSE))

  testOff(rx, c(central=FALSE, ce=TRUE,
                rx__sens_central_BY_p1=FALSE, rx__sens_central_BY_v1=FALSE))


  expect_equal(rx$state, c("ce",
                           "central",
                           "rx__sens_central_BY_p1", "rx__sens_central_BY_v1"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtB[(]", rxModelVars(rx)$model["normModel"]))

})



test_that("rxode2 parsing of linCmt() 2 compartment without ka", {

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popVp <- 10
    popQ <- 2
    bsvCl <-0
    bsvV <- 0
    bsvVp <- 0
    bsvQ <-0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
  })

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 0L, numLin = 2L, depotLin = 0L))

  testVal(rx, c("central", "ce", "peripheral1"))

  testInf(rx, c("central"=TRUE, "ce"=TRUE, "peripheral1"=FALSE))

  testOff(rx, c("central"=FALSE, "ce"=TRUE, "peripheral1"=FALSE))

  expect_equal(rx$state, c("ce", "central", "peripheral1"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtA[(]", rxModelVars(rx)$model["normModel"]))

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popVp <- 10
    popQ <- 2
    bsvCl <-0
    bsvV <- 0
    bsvVp <- 0
    bsvQ <-0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    b=central
    c=peripheral1
  }, linCmtSens="linCmtB")

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 8L, numLin = 2L, depotLin = 0L))

  testVal(rx, c("central", "ce", "peripheral1",
                "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
                #
                "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
                "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3"))


  testInf(rx, c("central"=TRUE, "ce"=TRUE, "peripheral1"=FALSE,
                "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
                "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
                #
                "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
                "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE))

  testOff(rx, c("central"=FALSE, "ce"=TRUE, "peripheral1"=FALSE,
                "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
                "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
                #
                "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
                "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE))


  expect_equal(rx$state, c("ce", "central", "peripheral1",
                           "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                           "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
                           #
                           "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
                           "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtB[(]", rxModelVars(rx)$model["normModel"]))
})


test_that("rxode2 parsing of linCmt() 2 compartment with ka", {

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popKa <- 1
    popVp <- 10
    popQ <- 2
    bsvCl <-0
    bsvV <- 0
    bsvKa <-0
    bsvVp <- 0
    bsvQ <-0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    ka ~ popKa * exp(bsvKa)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    keo ~ popKeo * exp(bsvKeo)
    popLagDepot <- 0
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagDepot <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(depot) <- popLagDepot * exp(bsvLagDepot)
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
  })

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 0L, numLin = 3L, depotLin = 1L))

  testVal(rx, c("depot", "central", "ce", "peripheral1"))

  testInf(rx, c("depot"=TRUE, "central"=TRUE, "ce"=TRUE, "peripheral1"=FALSE))

  testOff(rx, c("depot"=FALSE, "central"=FALSE, "ce"=TRUE, "peripheral1"=FALSE))

  expect_equal(rx$state, c("ce", "depot", "central", "peripheral1"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtA[(]", rxModelVars(rx)$model["normModel"]))

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popKa <- 1
    popVp <- 10
    popQ <- 2
    bsvCl <-0
    bsvV <- 0
    bsvKa <-0
    bsvVp <- 0
    bsvQ <-0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    ka ~ popKa * exp(bsvKa)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    keo ~ popKeo * exp(bsvKeo)
    popLagDepot <- 0
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagDepot <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(depot) <- popLagDepot * exp(bsvLagDepot)
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    a=depot
    b=central
    c=peripheral1
  }, linCmtSens="linCmtB")

  testVal(rx, c("depot", "central", "ce", "peripheral1",
                "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
                "rx__sens_central_BY_ka",
                #
                "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
                "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3",
                "rx__sens_peripheral1_BY_ka",
                #
                "rx__sens_depot_BY_ka"))

  testInf(rx, c("depot"=TRUE, "central"=TRUE, "ce"=TRUE, "peripheral1"=FALSE,
                "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
                "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
                "rx__sens_central_BY_ka"=FALSE,
                #
                "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
                "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE,
                "rx__sens_peripheral1_BY_ka"=FALSE,
                #
                "rx__sens_depot_BY_ka"=FALSE))

  testOff(rx, c("depot"=FALSE, "central"=FALSE, "ce"=TRUE, "peripheral1"=FALSE,
                "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
                "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
                "rx__sens_central_BY_ka"=FALSE,
                #
                "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
                "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE,
                "rx__sens_peripheral1_BY_ka"=FALSE,
                #
                "rx__sens_depot_BY_ka"=FALSE))


  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 11L, numLin = 3L, depotLin = 1L))

  expect_equal(rx$state, c("ce", "depot", "central", "peripheral1",
                 "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                 "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
                 "rx__sens_central_BY_ka",
                 #
                 "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
                 "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3",
                 "rx__sens_peripheral1_BY_ka",
                 #
                 "rx__sens_depot_BY_ka"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtB[(]", rxModelVars(rx)$model["normModel"]))
})


test_that("rxode2 parsing of linCmt() 3 compartment without ka", {

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popVp <- 10
    popQ <- 2
    popVp2 <- 1
    popQ2 <- 2
    bsvCl <-0
    bsvV <- 0
    bsvVp <- 0
    bsvQ <-0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    vp2 ~ popVp2
    q2 ~ popQ2
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
  })

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 0L, numLin = 3L, depotLin = 0L))

  testVal(rx, c("central", "ce", "peripheral1", "peripheral2"))

  testInf(rx, c("central"=TRUE, "ce"=TRUE, "peripheral1"=FALSE, "peripheral2"=FALSE))

  testOff(rx, c("central"=FALSE, "ce"=TRUE, "peripheral1"=FALSE, "peripheral2"=FALSE))

  expect_equal(rx$state, c("ce", "central", "peripheral1", "peripheral2"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtA[(]", rxModelVars(rx)$model["normModel"]))

  rx <- rxode2({
    popCl <- 1
    popV <- 20
    popVp <- 10
    popQ <- 2
    bsvCl <-0
    bsvV <- 0
    bsvVp <- 0
    bsvQ <-0
    popVp2 <- 3
    popQ2 <- 4
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    vp2 ~ popVp2
    q2 ~ popQ2
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    b=central
    c=peripheral1
  }, linCmtSens="linCmtB")

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 18L, numLin = 3L, depotLin = 0L))

  testVal(rx,c("central", "ce", "peripheral1", "peripheral2",
               "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
               "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
               "rx__sens_central_BY_p4", "rx__sens_central_BY_p5",
               #
               "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
               "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3",
               "rx__sens_peripheral1_BY_p4", "rx__sens_peripheral1_BY_p5",
               #
               "rx__sens_peripheral2_BY_p1", "rx__sens_peripheral2_BY_v1",
               "rx__sens_peripheral2_BY_p2", "rx__sens_peripheral2_BY_p3",
               "rx__sens_peripheral2_BY_p4", "rx__sens_peripheral2_BY_p5"))

  testInf(rx,c("central"=TRUE, "ce"=TRUE, "peripheral1"=FALSE, "peripheral2"=FALSE,
               "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
               "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
               "rx__sens_central_BY_p4"=FALSE, "rx__sens_central_BY_p5"=FALSE,
               #
               "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
               "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE,
               "rx__sens_peripheral1_BY_p4"=FALSE, "rx__sens_peripheral1_BY_p5"=FALSE,
               #
               "rx__sens_peripheral2_BY_p1"=FALSE, "rx__sens_peripheral2_BY_v1"=FALSE,
               "rx__sens_peripheral2_BY_p2"=FALSE, "rx__sens_peripheral2_BY_p3"=FALSE,
               "rx__sens_peripheral2_BY_p4"=FALSE, "rx__sens_peripheral2_BY_p5"=FALSE))

  testOff(rx,c("central"=FALSE, "ce"=TRUE, "peripheral1"=FALSE, "peripheral2"=FALSE,
               "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
               "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
               "rx__sens_central_BY_p4"=FALSE, "rx__sens_central_BY_p5"=FALSE,
               #
               "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
               "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE,
               "rx__sens_peripheral1_BY_p4"=FALSE, "rx__sens_peripheral1_BY_p5"=FALSE,
               #
               "rx__sens_peripheral2_BY_p1"=FALSE, "rx__sens_peripheral2_BY_v1"=FALSE,
               "rx__sens_peripheral2_BY_p2"=FALSE, "rx__sens_peripheral2_BY_p3"=FALSE,
               "rx__sens_peripheral2_BY_p4"=FALSE, "rx__sens_peripheral2_BY_p5"=FALSE))


  expect_equal(rx$state, c("ce", "central", "peripheral1", "peripheral2",
                 "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                 "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
                 "rx__sens_central_BY_p4", "rx__sens_central_BY_p5",
                 #
                 "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
                 "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3",
                 "rx__sens_peripheral1_BY_p4", "rx__sens_peripheral1_BY_p5",
                 #
                 "rx__sens_peripheral2_BY_p1", "rx__sens_peripheral2_BY_v1",
                 "rx__sens_peripheral2_BY_p2", "rx__sens_peripheral2_BY_p3",
                 "rx__sens_peripheral2_BY_p4", "rx__sens_peripheral2_BY_p5"
                 ))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtB[(]", rxModelVars(rx)$model["normModel"]))

})


test_that("rxode2 parsing of linCmt() 3 compartment with ka", {

  rx <- rxode2({
    popCl <- 1
    popKa <- 4
    popV <- 20
    popVp <- 10
    popQ <- 2
    popVp2 <- 1
    popQ2 <- 2
    bsvCl <-0
    bsvV <- 0
    bsvVp <- 0
    bsvQ <-0
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    ka ~ popKa
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    vp2 ~ popVp2
    q2 ~ popQ2
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
  })

  etTrans(et(amt=3,cmt=1) %>% et(1:10), rx)

  etTrans(et(amt=3) %>% et(1:10), rx)

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 0L, numLin = 4L, depotLin = 1L))

  testVal(rx,
          c("depot", "central", "ce", "peripheral1", "peripheral2"))

  testInf(rx,
          c("depot"=TRUE, "central"=TRUE, "ce"=TRUE,
            "peripheral1"=FALSE, "peripheral2"=FALSE))

  testOff(rx,
          c("depot"=FALSE, "central"=FALSE, "ce"=TRUE,
            "peripheral1"=FALSE, "peripheral2"=FALSE))


  expect_equal(rx$state, c("ce", "depot", "central", "peripheral1", "peripheral2"))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtA[(]", rxModelVars(rx)$model["normModel"]))

  rx <- rxode2({
    popCl <- 1
    popKa <- 3
    popV <- 20
    popVp <- 10
    popQ <- 2
    bsvCl <-0
    bsvV <- 0
    bsvVp <- 0
    bsvQ <-0
    popVp2 <- 3
    popQ2 <- 4
    popKeo <- 1.4
    bsvKeo <- 0
    popE0 <- 0
    popEmax <- 1
    popEC50 <- 5
    popGamma <- 1
    bsvE0 <- 0
    bsvEmax <- 0
    bsvEC50 <- 0
    ##
    ka ~ popKa
    cl ~ popCl * exp(bsvCl)
    v ~ popV * exp(bsvV)
    q ~ popQ * exp(bsvQ)
    vp ~ popVp * exp(bsvVp)
    vp2 ~ popVp2
    q2 ~ popQ2
    keo ~ popKeo * exp(bsvKeo)
    popLagCentral <- 0
    popRateCentral <- 0
    popDurCentral <- 0
    bsvLagCentral <- 0
    bsvRateCentral <- 0
    bsvDurCentral <- 0
    alag(central) <- popLagCentral * exp(bsvLagCentral)
    rate(central) <- popRateCentral * exp(bsvRateCentral)
    dur(central) <- popDurCentral * exp(bsvDurCentral)
    cp <- linCmt()
    d/dt(ce) = keo*(cp-ce)
    effect = E0 - Emax*(Ce^gamma)/((Ce^gamma)+(Ec50^gamma));
    b=central
    c=peripheral1
  }, linCmtSens="linCmtB")

  expect_equal(getLinInfo_(rxModelVars(rx)),
               c(numLinSens = 22L, numLin = 4L, depotLin = 1L))

  testVal(rx,
          c("depot", "central", "ce", "peripheral1", "peripheral2",
            "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
            "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
            "rx__sens_central_BY_p4", "rx__sens_central_BY_p5",
            "rx__sens_central_BY_ka",
            #
            "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
            "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3",
            "rx__sens_peripheral1_BY_p4", "rx__sens_peripheral1_BY_p5",
            "rx__sens_peripheral1_BY_ka",
            #
            "rx__sens_peripheral2_BY_p1", "rx__sens_peripheral2_BY_v1",
            "rx__sens_peripheral2_BY_p2", "rx__sens_peripheral2_BY_p3",
            "rx__sens_peripheral2_BY_p4", "rx__sens_peripheral2_BY_p5",
            "rx__sens_peripheral2_BY_ka",
            #
            "rx__sens_depot_BY_ka"))

  testInf(rx,
          c("depot"=TRUE, "central"=TRUE,
            "ce"=TRUE, "peripheral1"=FALSE, "peripheral2"=FALSE,
            "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
            "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
            "rx__sens_central_BY_p4"=FALSE, "rx__sens_central_BY_p5"=FALSE,
            "rx__sens_central_BY_ka"=FALSE,
            #
            "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
            "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE,
            "rx__sens_peripheral1_BY_p4"=FALSE, "rx__sens_peripheral1_BY_p5"=FALSE,
            "rx__sens_peripheral1_BY_ka"=FALSE,
            #
            "rx__sens_peripheral2_BY_p1"=FALSE, "rx__sens_peripheral2_BY_v1"=FALSE,
            "rx__sens_peripheral2_BY_p2"=FALSE, "rx__sens_peripheral2_BY_p3"=FALSE,
            "rx__sens_peripheral2_BY_p4"=FALSE, "rx__sens_peripheral2_BY_p5"=FALSE,
            "rx__sens_peripheral2_BY_ka"=FALSE,
            #
            "rx__sens_depot_BY_ka"=FALSE))

  testOff(rx,
          c("depot"=FALSE, "central"=FALSE,
            "ce"=TRUE, "peripheral1"=FALSE, "peripheral2"=FALSE,
            "rx__sens_central_BY_p1"=FALSE, "rx__sens_central_BY_v1"=FALSE,
            "rx__sens_central_BY_p2"=FALSE, "rx__sens_central_BY_p3"=FALSE,
            "rx__sens_central_BY_p4"=FALSE, "rx__sens_central_BY_p5"=FALSE,
            "rx__sens_central_BY_ka"=FALSE,
            #
            "rx__sens_peripheral1_BY_p1"=FALSE, "rx__sens_peripheral1_BY_v1"=FALSE,
            "rx__sens_peripheral1_BY_p2"=FALSE, "rx__sens_peripheral1_BY_p3"=FALSE,
            "rx__sens_peripheral1_BY_p4"=FALSE, "rx__sens_peripheral1_BY_p5"=FALSE,
            "rx__sens_peripheral1_BY_ka"=FALSE,
            #
            "rx__sens_peripheral2_BY_p1"=FALSE, "rx__sens_peripheral2_BY_v1"=FALSE,
            "rx__sens_peripheral2_BY_p2"=FALSE, "rx__sens_peripheral2_BY_p3"=FALSE,
            "rx__sens_peripheral2_BY_p4"=FALSE, "rx__sens_peripheral2_BY_p5"=FALSE,
            "rx__sens_peripheral2_BY_ka"=FALSE,
            #
            "rx__sens_depot_BY_ka"=FALSE))



  expect_equal(rx$state, c("ce", "depot", "central", "peripheral1", "peripheral2",
                 "rx__sens_central_BY_p1", "rx__sens_central_BY_v1",
                 "rx__sens_central_BY_p2", "rx__sens_central_BY_p3",
                 "rx__sens_central_BY_p4", "rx__sens_central_BY_p5",
                 "rx__sens_central_BY_ka",
                 #
                 "rx__sens_peripheral1_BY_p1", "rx__sens_peripheral1_BY_v1",
                 "rx__sens_peripheral1_BY_p2", "rx__sens_peripheral1_BY_p3",
                 "rx__sens_peripheral1_BY_p4", "rx__sens_peripheral1_BY_p5",
                 "rx__sens_peripheral1_BY_ka",
                 #
                 "rx__sens_peripheral2_BY_p1", "rx__sens_peripheral2_BY_v1",
                 "rx__sens_peripheral2_BY_p2", "rx__sens_peripheral2_BY_p3",
                 "rx__sens_peripheral2_BY_p4", "rx__sens_peripheral2_BY_p5",
                 "rx__sens_peripheral2_BY_ka",
                 #
                 "rx__sens_depot_BY_ka"
                 ))

  expect_equal(rx$stateExtra, character(0))

  expect_true(grepl("linCmtB[(]", rxModelVars(rx)$model["normModel"]))

})


if (requireNamespace("pmxTools", quietly = TRUE)) {

  test_that("rxode2 solving of 1 compartment steady state bolus", {

    f <- function(CL=6, V=25, DOSE=600, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- 0
      p3 <- 0
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- c(0, 0, 0)
      } else {
        alastNV <- 0
      }
      rateNV <- 0
      oral0 <- 0
      trans <- 1L
      ncmt <- 1
      type <- 3L # ss bolus
      amt <- DOSE
      tinf <- 1
      dt <- 0
      cmt <- 0L
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans, deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(pmxTools::calc_ss_1cmt_linear_bolus(t=0, CL=6, V=25, dose=600, tau=24),
                 f()$val)

    v <- f(deriv=TRUE)

    expect_equal(v$val,pmxTools::calc_ss_1cmt_linear_bolus(t=0, CL=6, V=25, dose=600, tau=24))

    expect_equal(dim(v$J), 1:2)
    expect_length(v$Jg, 2)
    expect_length(v$Alast, 3)
  })

  test_that("rxode2 solving of 1 compartment steady state infusion", {

    f <- function(CL=2, V=25, DOSE=600, tinf=1, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- 0
      p3 <- 0
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- c(0, 0, 0)
      } else {
        alastNV <- 0
      }
      rateNV <- DOSE/tinf
      oral0 <- 0
      trans <- 1L
      ncmt <- 1
      type <- 2L # ss infusion
      amt <- DOSE
      dt <- 0
      cmt <- 0L
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(pmxTools::calc_ss_1cmt_linear_infusion(tad=0, CL=2, V=25, dose=600, tinf=1, tau=24),
                 f()$val)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 pmxTools::calc_ss_1cmt_linear_infusion(tad=0, CL=2, V=25, dose=600, tinf=1, tau=24))

    expect_equal(dim(v$J), 1:2)
    expect_length(v$Jg, 2)
    expect_length(v$Alast, 3)
  })

  test_that("rxode2 solving of 1 compartment steady state 1st order dosing", {

    f <- function(CL=2, V=25, DOSE=600, ka=0.25, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- 0
      p3 <- 0
      p4 <- 0
      p5 <- 0
      ka <- ka
      if (deriv) {
        alastNV <- c(0, 0, 0, 0, 0, 0)
      } else {
        alastNV <- c(0, 0)
      }
      rateNV <- c(0, 0)
      oral0 <- 1L
      trans <- 1L
      ncmt <- 1
      type <- 3L # bolus infusion
      amt <- DOSE
      dt <- 0
      cmt <- 0L
      tinf <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(f()$val,
                 pmxTools::calc_ss_1cmt_linear_oral_1(tad=0, CL=2, V=25, dose=600, ka=0.25, tau=24))


    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 pmxTools::calc_ss_1cmt_linear_oral_1(tad=0, CL=2, V=25, dose=600, ka=0.25, tau=24))

    expect_equal(dim(v$J), 2:3)
    expect_length(v$Jg, 3)
    expect_length(v$Alast, 6)
  })

  test_that("rxode2 solving of 1 compartment steady state for constant infusion", {
    ## Now test the solutions not covered by pmxTools by cross testing
    ## against rxode2 lsoda
    rx <- rxode2({
      CL <- 6
      V <- 25
      kel <- CL/V
      d/dt(central) = -kel*central
      cp <- central/V
    })

    e <- et(amt=0, rate=600, ss=1) %>%
      et(time=seq(1:25))

    f <- function(CL=6, V=25, DOSE=600, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- 0
      p3 <- 0
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- c(0, 0, 0)
      } else {
        alastNV <- 0
      }
      rateNV <- 600
      oral0 <- 0
      trans <- 1L
      ncmt <- 1
      type <- 1L # ss infinite infusion
      amt <- 0
      tinf <- 0
      dt <- 0
      cmt <- 0L
      tau <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(f()$val,
                 rxSolve(rx, e, addDosing =TRUE)$cp[1])

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 rxSolve(rx, e, addDosing =TRUE)$cp[1])

    expect_equal(dim(v$J), 1:2)
    expect_length(v$Jg, 2)
    expect_length(v$Alast, 3)
  })

  test_that("rxode2 solving of 1 compartment steady state for constant depot infusion", {

    rx <- rxode2({
      CL <- 6
      V <- 25
      ka <- 0.25
      kel <- CL/V
      d/dt(depot) = -ka*depot
      d/dt(central) = -kel*central + ka*depot
      cp <- central/V
    })

    e <- et(amt=0, rate=600, ss=1) %>%
      et(time=seq(1:25))

    f <- function(CL=6, V=25, DOSE=600, ka=0.25, rate=c(600, 0), deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- 0
      p3 <- 0
      p4 <- 0
      p5 <- 0
      ka <- ka
      if (deriv) {
        alastNV <- c(0, 0, 0, 0, 0, 0)
      } else {
        alastNV <- c(0, 0)
      }
      rateNV <- rate
      oral0 <- 1L
      trans <- 1L
      ncmt <- 1
      type <- 1L # ss infinite infusion
      amt <- 0
      tinf <- 0
      dt <- 0
      cmt <- 0L
      tau <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f()$val,
                 s$cp[1])

    expect_equal(f()$Alast,
                 c(s$depot[1], s$central[1]))

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1])

    expect_equal(v$Alast[1:2],
                 c(s$depot[1], s$central[1]))

    expect_equal(dim(v$J), 2:3)

    expect_length(v$Jg, 3)
    expect_length(v$Alast, 6)

    # Now infusion in the 2 compartment models to central

    e <- et(amt=0, rate=600, ss=1, cmt=2) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f(rate=c(0, 600))$val,
                 s$cp[1])

    expect_equal(f(rate=c(0, 600))$Alast,
                 c(s$depot[1], s$central[1]))

    v <- f(rate=c(0, 600), deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1])

    expect_equal(v$Alast[1:2],
                 c(s$depot[1], s$central[1]))

    expect_equal(dim(v$J), 2:3)

    expect_length(v$Jg, 3)
    expect_length(v$Alast, 6)

  })

  test_that("rxode2 solving of 1 compartment steady state for constant depot infusion", {

    f <- function(CL=2, V=25, DOSE=600, tinf=1, tau=6, deriv=FALSE, cmt=0L,
                  ka=0.25) {
      p1 <- CL
      v1 <- V
      p2 <- 0
      p3 <- 0
      p4 <- 0
      p5 <- 0
      ka <- ka
      if (deriv) {
        alastNV <- c(0, 0, 0, 0, 0, 0)
      } else {
        alastNV <- c(0, 0)
      }
      if (cmt == 0L) {
        rateNV <- c(DOSE/tinf, 0)
      } else {
        rateNV <- c(0, DOSE/tinf)
      }
      oral0 <- 1L
      trans <- 1L
      ncmt <- 1
      type <- 2L # ss infusion
      amt <- DOSE
      dt <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    rx <- rxode2({
      CL <- 2
      V <- 25
      ka <- 0.25
      kel <- CL/V
      d/dt(depot) = -ka*depot
      d/dt(central) = -kel*central + ka*depot
      cp <- central/V
    })

    e <- et(amt=600, rate=600, ss=1, ii=6, cmt=depot) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f()$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f()$Alast,
                 c(s$depot[1], s$central[1]),
                 tolerance=1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:2],
                 c(s$depot[1], s$central[1]),
                 tolerance = 1e-3)

    expect_equal(dim(v$J), 2:3)

    expect_length(v$Jg, 3)
    expect_length(v$Alast, 6)

    e <- et(amt=600, rate=600, ss=1, ii=6, cmt=central) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f(cmt=1L)$val,
                 s$cp[1], tolerance = 1e-3)

        expect_equal(f(cmt=1L)$Alast,
                     c(s$depot[1], s$central[1]),
                     tolerance = 1e-3)

        v <- f(cmt=1L, deriv=TRUE)

        expect_equal(v$val,
                     s$cp[1], tolerance = 1e-3)

        expect_equal(v$Alast[1:2],
                     c(s$depot[1], s$central[1]), tolerance = 1e-3)

        expect_equal(dim(v$J), 2:3)

        expect_length(v$Jg, 3)
        expect_length(v$Alast, 6)

  })

  ####################################################################
  ## Test the 2 compartment steady state solutions
  ####################################################################

  test_that("rxode2 solving of 2 compartment steady state bolus", {

    f <- function(CL=7.5, V=20, V2 = 30, Q = 0.5, DOSE=10, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- rep(0, 10)
      } else {
        alastNV <- c(0, 0)
      }
      rateNV <- 0
      oral0 <- 0
      trans <- 1L
      ncmt <- 2
      type <- 3L # ss bolus
      amt <- DOSE
      tinf <- 1
      dt <- 0
      cmt <- 0L
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans, deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(pmxTools::calc_ss_2cmt_linear_bolus(0, CL = 7.5, V1 = 20, V2 = 30, Q = 0.5,
                                                     dose = 10, tau=24),
                 f()$val)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 pmxTools::calc_ss_2cmt_linear_bolus(0, CL = 7.5, V1 = 20, V2 = 30, Q = 0.5,
                                                     dose = 10, tau=24))

    expect_equal(dim(v$J), c(2L, 4L))

    expect_length(v$Jg, 4)

    expect_length(v$Alast, 10)

  })


  test_that("rxode2 solving of 2 compartment steady state infusion", {

    f <- function(CL=7.5, V=20, V2=30, Q=0.5, DOSE=10, tinf=1, tau=12, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- rep(0, 10)
      } else {
        alastNV <- c(0, 0)
      }
      rateNV <- DOSE/tinf
      oral0 <- 0
      trans <- 1L
      ncmt <- 2
      type <- 2L # ss infusion
      amt <- DOSE
      dt <- 0
      cmt <- 0L
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(pmxTools::calc_ss_2cmt_linear_infusion(tad = 0, CL = 7.5, V1 = 20, V2 = 30, Q = 0.5,
                                                        dose = 10, tinf = 1, tau = 12),
                 f()$val)

    v <- f(deriv=TRUE)

    expect_equal(pmxTools::calc_ss_2cmt_linear_infusion(tad = 0, CL = 7.5, V1 = 20, V2 = 30, Q = 0.5,
                                                        dose = 10, tinf = 1, tau = 12),
                 v$val)

    expect_equal(dim(v$J), c(2L, 4L))
    expect_length(v$Jg, 4)
    expect_length(v$Alast, 10)

  })

  test_that("rxode2 solving of 2 compartment steady state 1st order dosing", {

    f <- function(CL=7.5, V=20, V2=30, Q=0.5, DOSE=1000, ka=1, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- ka
      if (deriv) {
        alastNV <- rep(0, 14)
      } else {
        alastNV <- c(0, 0, 0)
      }
      rateNV <- c(0, 0)
      oral0 <- 1L
      trans <- 1L
      ncmt <- 2
      type <- 3L # bolus infusion
      amt <- DOSE
      dt <- 0
      cmt <- 0L
      tinf <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(f()$val,
                 pmxTools::calc_ss_2cmt_linear_oral_1(tad=0, CL=7.5, V1=20, V2=30, Q=0.5, dose=1000, ka=1, tau=24))


    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 pmxTools::calc_ss_2cmt_linear_oral_1(tad=0, CL=7.5, V1=20, V2=30, Q=0.5, dose=1000, ka=1, tau=24))

    expect_equal(dim(v$J), c(3L, 5L))
    expect_length(v$Jg, 5)
    expect_length(v$Alast, 14)

  })

  test_that("rxode2 solving of 2 compartment steady state for constant infusion", {
    ## Now test the solutions not covered by pmxTools by cross testing
    ## against rxode2 lsoda

    rx <- rxode2({
      CL <- 6
      V <- 25
      Q <- 0.5
      V2 <- 30
      k12 <- Q/V
      k21 <- Q/V2
      kel <- CL/V
      d/dt(central) = -kel*central + k21*peripheral - k12*central
      d/dt(peripheral) = k12*central - k21*peripheral
      cp <- central/V
    })

    e <- et(amt=0, rate=600, ss=1) %>%
      et(time=seq(1:25))

    f <- function(CL=6, V=25, Q=0.5, V2=30, DOSE=600, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- rep(0, 10)
      } else {
        alastNV <- c(0, 0)
      }
      rateNV <- 600
      oral0 <- 0
      trans <- 1L
      ncmt <- 2
      type <- 1L # ss infinite infusion
      amt <- 0
      tinf <- 0
      dt <- 0
      cmt <- 0L
      tau <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(f()$val,
                 rxSolve(rx, e, addDosing =TRUE)$cp[1],
                 tolerance = 1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 rxSolve(rx, e, addDosing =TRUE)$cp[1],
                 tolerance = 1e-3)

    expect_equal(dim(v$J), c(2L, 4L))
    expect_length(v$Jg, 4)
    expect_length(v$Alast, 10)

  })

  test_that("rxode2 solving of 2 compartment steady state for constant depot infusion", {

    rx <- rxode2({
      CL <- 6
      V <- 25
      Q <- 0.5
      V2 <- 30
      k12 <- Q/V
      k21 <- Q/V2
      kel <- CL/V
      ka <- 0.25
      d/dt(depot) = -ka*depot
      d/dt(central) = -kel*central + k21*peripheral - k12*central + ka*depot
      d/dt(peripheral) = k12*central - k21*peripheral
      cp <- central/V
    })


    e <- et(amt=0, rate=600, ss=1) %>%
      et(time=seq(1:25))

    f <- function(CL=6, V=25, Q=0.5, V2=30, DOSE=600, ka=0.25, rate=c(600, 0), deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- ka
      if (deriv) {
        alastNV <- rep(0, 14)
      } else {
        alastNV <- rep(0, 3)
      }
      rateNV <- rate
      oral0 <- 1L
      trans <- 1L
      ncmt <- 2
      type <- 1L # ss infinite infusion
      amt <- 0
      tinf <- 0
      dt <- 0
      cmt <- 0L
      tau <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f()$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f()$Alast[1:3],
                 c(s$depot[1], s$central[1], s$peripheral[1]),
                 tolerance=1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:3],
                 c(s$depot[1], s$central[1], s$peripheral[1]),
                 tolerance=1e-3)

    expect_equal(dim(v$J), c(3L, 5L))

    expect_length(v$Jg, 5)
    expect_length(v$Alast, 14)

    # Now infusion in the 2 compartment models to central

    e <- et(amt=0, rate=600, ss=1, cmt=2) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f(rate=c(0, 600))$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f(rate=c(0, 600))$Alast[1:3],
                 c(s$depot[1], s$central[1],s$peripheral[1]),
                 tolerance=1e-3)

    v <- f(rate=c(0, 600), deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:3],
                 c(s$depot[1], s$central[1], s$peripheral[1]),
                 tolerance=1e-3)

    expect_equal(dim(v$J), c(3L, 5L))

    expect_length(v$Jg, 5)
    expect_length(v$Alast, 14)

  })

  test_that("rxode2 solving of 2 compartment steady state for constant depot infusion", {

    f <- function(CL=2, V=25, Q=0.5, V2=30, DOSE=600, tinf=1, tau=6, deriv=FALSE, cmt=0L,
                  ka=0.25) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- ka
      if (deriv) {
        alastNV <- rep(0, 14)
      } else {
        alastNV <- rep(0, 3)
      }
      if (cmt == 0L) {
        rateNV <- c(DOSE/tinf, 0)
      } else {
        rateNV <- c(0, DOSE/tinf)
      }
      oral0 <- 1L
      trans <- 1L
      ncmt <- 2
      type <- 2L # ss infusion
      amt <- DOSE
      dt <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    rx <- rxode2({
      CL <- 2
      V <- 25
      Q <- 0.5
      V2 <- 30
      k12 <- Q/V
      k21 <- Q/V2
      kel <- CL/V
      ka <- 0.25
      d/dt(depot) = -ka*depot
      d/dt(central) = -kel*central + k21*peripheral - k12*central + ka*depot
      d/dt(peripheral) = k12*central - k21*peripheral
      cp <- central/V
    })


    e <- et(amt=600, rate=600, ss=1, ii=6, cmt=depot) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f()$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f()$Alast,
                 c(s$depot[1], s$central[1], s$peripheral[1]),
                 tolerance=1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:3],
                 c(s$depot[1], s$central[1], s$peripheral[1]),
                 tolerance = 1e-3)

    expect_equal(dim(v$J), c(3L, 5L))

    expect_length(v$Jg, 5)
    expect_length(v$Alast, 14)

    e <- et(amt=600, rate=600, ss=1, ii=6, cmt=central) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f(cmt=1L)$val,
                 s$cp[1], tolerance = 1e-3)

    expect_equal(f(cmt=1L)$Alast,
                 c(s$depot[1], s$central[1], s$peripheral[1]),
                 tolerance = 1e-3)

    v <- f(cmt=1L, deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance = 1e-3)

    expect_equal(v$Alast[1:2],
                 c(s$depot[1], s$central[1]), tolerance = 1e-3)

    expect_equal(dim(v$J), c(3L, 5L))

    expect_length(v$Jg, 5L)
    expect_length(v$Alast, 14)

  })

  ####################################################################
  ## Test the 3 compartment steady state solutions
  ####################################################################

  test_that("rxode2 solving of 3 compartment steady state bolus", {

    f <- function(CL=3.5, V1=20, V2 = 500, V3=200, Q2=0.5, Q3 = 0.05, DOSE=100, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V1
      p2 <- Q2
      p3 <- V2
      p4 <- Q3
      p5 <- V3
      ka <- 0
      if (deriv) {
        alastNV <- rep(0, 21)
      } else {
        alastNV <- c(0, 0, 0)
      }
      rateNV <- 0
      oral0 <- 0
      trans <- 1L
      ncmt <- 3
      type <- 3L # ss bolus
      amt <- DOSE
      tinf <- 1
      dt <- 0
      cmt <- 0L
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans, deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(pmxTools::calc_ss_3cmt_linear_bolus(0, CL=3.5, V1=20, V2 = 500, V3=200, Q2=0.5, Q3 = 0.05, dose=100, tau=24),
                 f()$val)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 pmxTools::calc_ss_3cmt_linear_bolus(0, CL=3.5, V1=20, V2 = 500, V3=200, Q2=0.5, Q3 = 0.05, dose=100, tau=24))

    expect_equal(dim(v$J), c(3L, 6L))
    expect_length(v$Jg, 6)
    expect_length(v$Alast, 21)

  })


  test_that("rxode2 solving of 3 compartment steady state infusion", {

    f <- function(CL=7.5, V=20, V2=30, Q=0.5, DOSE=10, tinf=1, tau=12, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q
      p3 <- V2
      p4 <- 0
      p5 <- 0
      ka <- 0
      if (deriv) {
        alastNV <- rep(0, 10)
      } else {
        alastNV <- c(0, 0)
      }
      rateNV <- DOSE/tinf
      oral0 <- 0
      trans <- 1L
      ncmt <- 2
      type <- 2L # ss infusion
      amt <- DOSE
      dt <- 0
      cmt <- 0L
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(pmxTools::calc_ss_2cmt_linear_infusion(tad = 0, CL = 7.5, V1 = 20, V2 = 30, Q = 0.5,
                                                        dose = 10, tinf = 1, tau = 12),
                 f()$val)

    v <- f(deriv=TRUE)

    expect_equal(pmxTools::calc_ss_2cmt_linear_infusion(tad = 0, CL = 7.5, V1 = 20, V2 = 30, Q = 0.5,
                                                        dose = 10, tinf = 1, tau = 12),
                 v$val)

    expect_equal(dim(v$J), c(2L, 4L))
    expect_length(v$Jg, 4)
    expect_length(v$Alast, 10)

  })

  test_that("rxode2 solving of 3 compartment steady state 1st order dosing", {

    f <- function(CL=7.5, V1=20, V2=30, V3=200, Q2=0.5, Q3=0.05, ka=1, DOSE=100, tau=24, deriv=FALSE) {
      p1 <- CL
      v1 <- V1
      p2 <- Q2
      p3 <- V2
      p4 <- Q3
      p5 <- V3
      ka <- ka
      if (deriv) {
        alastNV <- rep(0, 26)
      } else {
        alastNV <- c(0, 0, 0, 0)
      }
      rateNV <- c(0, 0)
      oral0 <- 1L
      trans <- 1L
      ncmt <- 3
      type <- 3L # bolus infusion
      amt <- DOSE
      dt <- 0
      cmt <- 0L
      tinf <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(f()$val,
                 pmxTools::calc_ss_3cmt_linear_oral_1(tad = 0, CL = 7.5, V1 = 20,
                                                      V2 = 30, V3 = 200, Q2 = 0.5,
                                                      Q3 = 0.05, ka = 1, dose = 100,
                                                      tau = 24))


    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 pmxTools::calc_ss_3cmt_linear_oral_1(tad = 0, CL = 7.5, V1 = 20,
                                                      V2 = 30, V3 = 200, Q2 = 0.5,
                                                      Q3 = 0.05, ka = 1, dose = 100,
                                                      tau = 24))

    expect_equal(dim(v$J), c(4L, 7L))
    expect_length(v$Jg, 7)
    expect_length(v$Alast, 26)

  })

  test_that("rxode2 solving of 3 compartment steady state for constant infusion", {

    ## Now test the solutions not covered by pmxTools by cross testing
    ## against rxode2 lsoda

    rx <- rxode2({
      CL <- 6
      V <- 25
      V2 <- 30
      V3 <- 200
      Q2 <- 0.5
      Q3 <- 0.05
      k12 <- Q2/V
      k21 <- Q2/V2
      k13 <- Q3/V
      k31 <- Q3/V3
      kel <- CL/V
      d/dt(central) = -kel*central + k21*peripheral - k12*central +
        k31*peripheral2 - k13*central
      d/dt(peripheral) = k12*central - k21*peripheral
      d/dt(peripheral2) = k13*central - k31*peripheral2
      cp <- central/V
    })

    e <- et(amt=0, rate=600, ss=1) %>%
      et(time=seq(1:25))

    f <- function(CL= 6, V = 25, V2 = 30, V3 = 200,
                  Q2= 0.5,
                  Q3= 0.05,
                  DOSE=600, deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q2
      p3 <- V2
      p4 <- Q3
      p5 <- V3
      ka <- 0
      if (deriv) {
        alastNV <- rep(0, 21)
      } else {
        alastNV <- c(0, 0, 0)
      }
      rateNV <- 600
      oral0 <- 0
      trans <- 1L
      ncmt <- 3
      type <- 1L # ss infinite infusion
      amt <- 0
      tinf <- 0
      dt <- 0
      cmt <- 0L
      tau <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    expect_equal(f()$val,
                 rxSolve(rx, e, addDosing =TRUE)$cp[1],
                 tolerance = 1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 rxSolve(rx, e, addDosing =TRUE)$cp[1],
                 tolerance = 1e-3)

    expect_equal(dim(v$J), c(3L, 6L))
    expect_length(v$Jg, 6)
    expect_length(v$Alast, 21)

  })

  test_that("rxode2 solving of 3 compartment steady state for constant depot infusion", {

    rx <- rxode2({
      CL <- 6
      V  <- 25
      Q2 <- 0.5
      Q3 <- 0.05
      V2 <- 30
      V3 <- 300
      k12 <- Q2/V
      k21 <- Q2/V2
      k13 <- Q3/V
      k31 <- Q3/V3
      kel <- CL/V
      ka <- 0.25
      d/dt(depot) = -ka*depot
      d/dt(central) = -kel*central + ka*depot +
        k21*peripheral  - k12*central +
        k31*peripheral1 - k13*central
      d/dt(peripheral) = k12*central - k21*peripheral
      d/dt(peripheral1) = k13*central - k31*peripheral1
      cp <- central/V
    })

    e <- et(amt=0, rate=600, ss=1) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    f <- function(CL=6, V=25, Q2=0.5, Q3=0.05, V2=30, V3=300, DOSE=600,
                  ka=0.25, rate=c(600, 0), deriv=FALSE) {
      p1 <- CL
      v1 <- V
      p2 <- Q2
      p3 <- V2
      p4 <- Q3
      p5 <- V3
      ka <- ka
      if (deriv) {
        alastNV <- rep(0, 26)
      } else {
        alastNV <- rep(0, 4)
      }
      rateNV <- rate
      oral0 <- 1L
      trans <- 1L
      ncmt <- 3
      type <- 1L # ss infinite infusion
      amt <- 0
      tinf <- 0
      dt <- 0
      cmt <- 0L
      tau <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f()$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f()$Alast[1:4],
                 c(s$depot[1], s$central[1], s$peripheral[1], s$peripheral1[1]),
                 tolerance=1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:4],
                 c(s$depot[1], s$central[1], s$peripheral[1],
                   s$peripheral1[1]), tolerance=1e-3)

    expect_equal(dim(v$J), c(4L, 7L))

    expect_length(v$Jg, 7)
    expect_length(v$Alast, 26)

    # Now infusion in the 3 compartment models to central

    e <- et(amt=0, rate=600, ss=1, cmt=2) %>%
      et(time=seq(1:25))

    s <- rxSolve(rx, e, addDosing =TRUE)

    expect_equal(f(rate=c(0, 600))$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f(rate=c(0, 600))$Alast[1:3],
                 c(s$depot[1], s$central[1],s$peripheral[1]),
                 tolerance=1e-3)

    v <- f(rate=c(0, 600), deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:4],
                 c(s$depot[1], s$central[1], s$peripheral[1], s$peripheral1[1]),
                 tolerance=1e-3)

    expect_equal(dim(v$J), c(4L, 7L))

    expect_length(v$Jg, 7)
    expect_length(v$Alast, 26)

  })

  test_that("rxode2 solving of 3 compartment steady state for constant depot infusion", {

    f <- function(CL=6, V=25, Q2=0.5, Q3=0.05, V2=30, V3=300, DOSE=600, tinf=1, tau=6, deriv=FALSE, cmt=0L,
                  ka=0.25) {
      p1 <- CL
      v1 <- V
      p2 <- Q2
      p3 <- V2
      p4 <- Q3
      p5 <- V3
      ka <- ka
      if (deriv) {
        alastNV <- rep(0, 26)
      } else {
        alastNV <- rep(0, 4)
      }
      if (cmt == 0L) {
        rateNV <- c(DOSE/tinf, 0)
      } else {
        rateNV <- c(0, DOSE/tinf)
      }
      oral0 <- 1L
      trans <- 1L
      ncmt <- 3
      type <- 2L # ss infusion
      amt <- DOSE
      dt <- 0
      .Call(`_rxode2_linCmtModelDouble`, 0.1,
            p1, v1, p2, p3, p4, p5, ka,
            alastNV, rateNV, ncmt, oral0, trans,
            deriv, type, tau, tinf, amt, cmt)
    }

    rx <- rxode2({
      CL <- 6
      V  <- 25
      Q2 <- 0.5
      Q3 <- 0.05
      V2 <- 30
      V3 <- 300
      k12 <- Q2/V
      k21 <- Q2/V2
      k13 <- Q3/V
      k31 <- Q3/V3
      kel <- CL/V
      ka <- 0.25
      d/dt(depot) = -ka*depot
      d/dt(central) = -kel*central + ka*depot +
        k21*peripheral  - k12*central +
        k31*peripheral1 - k13*central
      d/dt(peripheral) = k12*central - k21*peripheral
      d/dt(peripheral1) = k13*central - k31*peripheral1
      cp <- central/V
    })



    e <- et(amt=600, rate=600, ss=1, ii=6, cmt=depot) %>%
      et(time=0.5)

    s <- rxSolve(rx, e, addDosing =TRUE, maxsteps=700000)

    expect_equal(f()$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(f()$Alast,
                 c(s$depot[1], s$central[1], s$peripheral[1], s$peripheral1[1]),
                 tolerance=1e-3)

    v <- f(deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance=1e-3)

    expect_equal(v$Alast[1:4],
                 c(s$depot[1], s$central[1], s$peripheral[1], s$peripheral1[1]),
                 tolerance = 1e-3)

    expect_equal(dim(v$J), c(4L, 7L))

    expect_length(v$Jg, 7)
    expect_length(v$Alast, 26)

    e <- et(amt=600, rate=600, ss=1, ii=6, cmt=central) %>%
      et(time=0.5)

    s <- rxSolve(rx, e, addDosing =TRUE, , maxsteps=700000)

    expect_equal(f(cmt=1L)$val,
                 s$cp[1], tolerance = 1e-3)

    expect_equal(f(cmt=1L)$Alast,
                 c(s$depot[1], s$central[1], s$peripheral[1], s$peripheral1[1]),
                 tolerance = 1e-3)

    v <- f(cmt=1L, deriv=TRUE)

    expect_equal(v$val,
                 s$cp[1], tolerance = 1e-3)

    expect_equal(v$Alast[1:4],
                 c(s$depot[1], s$central[1], s$peripheral[1], s$peripheral1[1]),
                 tolerance = 1e-3)

    expect_equal(dim(v$J), c(4L, 7L))

    expect_length(v$Jg, 7L)
    expect_length(v$Alast, 26)

  })


}
