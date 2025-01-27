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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    if (dt <= tinf) {
    } else {
      l <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)
      dt <- dt - tinf
      extra <- tinf
      rateNV <- 0
      alastNV <- l$Alast
    }
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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
    if (dt <= tinf) {
    } else {
      l <- .Call(`_rxode2_linCmtModelDouble`, tinf, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)
      dt <- dt - tinf
      extra <- tinf
      rateNV <- 0
      alastNV <- l$Alast
    }
    l <- .Call(`_rxode2_linCmtModelDouble`, dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0, trans)$val
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

 }
