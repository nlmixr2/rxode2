rxTest({

  ## Reading materialized linCmt() compartment amounts (central / peripheral1 /
  ## peripheral2 / depot) in model equations *under an error model*.  An
  ## endpoint injects an observation compartment that used to shift the linCmt
  ## compartments' solved-state indices, so an in-equation reference read an
  ## unwritten slot (== 0).  The __DDT declaration-index fix makes these resolve
  ## to the solved amount.  Each case is checked against the equivalent explicit
  ## ODE solved with useLinCmt = FALSE (the analytic and numeric solutions must
  ## agree), and the referenced columns must not be identically zero (the bug).

  .tol <- 1e-4
  .chk <- function(lin, ode, ev, cols) {
    .rL <- suppressMessages(rxSolve(lin, ev))
    .rO <- suppressMessages(rxSolve(ode, ev, useLinCmt = FALSE))
    for (.c in cols) {
      expect_true(.c %in% names(.rL), info = paste0(.c, " present in linCmt solve"))
      expect_false(all(.rL[[.c]] == 0),
                   label = paste0(.c, " must not be identically zero (the bug)"))
      expect_equal(.rL[[.c]], .rO[[.c]], tolerance = .tol,
                   label = paste0(.c, " (linCmt) vs ", .c, " (ODE)"))
    }
  }
  .evIV <- et(amt = 100, cmt = "central") |> et(seq(0, 48, by = 2))
  .evPO <- et(amt = 100, cmt = "depot")   |> et(seq(0, 48, by = 2))

  test_that("2-cmt IV: peripheral1 amount and concentration resolve under an error model", {
    .lin <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50); pSd<-0.1 })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        Cc  <- linCmt()
        Ap1 <- peripheral1        # solved peripheral amount
        Cp1 <- peripheral1 / vp   # peripheral concentration
        Cc ~ prop(pSd)
      })
    }))
    .ode <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50) })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        kel<-cl/vc; k12<-q/vc; k21<-q/vp
        d/dt(central)     <- -kel*central - k12*central + k21*peripheral1
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        Cc<-central/vc; Ap1<-peripheral1; Cp1<-peripheral1/vp
      })
    }))
    .chk(.lin, .ode, .evIV, c("Cc", "Ap1", "Cp1"))
  })

  test_that("2-cmt IV: central amount resolves under an error model", {
    .lin <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50); pSd<-0.1 })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        Cc <- linCmt()
        Ac <- central             # solved central amount
        Cc ~ prop(pSd)
      })
    }))
    .ode <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50) })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        kel<-cl/vc; k12<-q/vc; k21<-q/vp
        d/dt(central)     <- -kel*central - k12*central + k21*peripheral1
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        Cc<-central/vc; Ac<-central
      })
    }))
    .chk(.lin, .ode, .evIV, c("Cc", "Ac"))
  })

  test_that("3-cmt IV: peripheral1 and peripheral2 resolve under an error model", {
    .lin <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50); lq2<-log(1); lvp2<-log(100); pSd<-0.1 })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp); q2<-exp(lq2); vp2<-exp(lvp2)
        Cc  <- linCmt()
        Cp1 <- peripheral1 / vp
        Cp2 <- peripheral2 / vp2
        Cc ~ prop(pSd)
      })
    }))
    .ode <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50); lq2<-log(1); lvp2<-log(100) })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp); q2<-exp(lq2); vp2<-exp(lvp2)
        kel<-cl/vc; k12<-q/vc; k21<-q/vp; k13<-q2/vc; k31<-q2/vp2
        d/dt(central)     <- -kel*central - k12*central + k21*peripheral1 - k13*central + k31*peripheral2
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        d/dt(peripheral2) <-  k13*central - k31*peripheral2
        Cc<-central/vc; Cp1<-peripheral1/vp; Cp2<-peripheral2/vp2
      })
    }))
    .chk(.lin, .ode, .evIV, c("Cc", "Cp1", "Cp2"))
  })

  test_that("2-cmt oral: depot amount and peripheral1 resolve under an error model", {
    .lin <- suppressMessages(rxode2(function() {
      ini({ lka<-log(0.8); lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50); pSd<-0.1 })
      model({
        ka<-exp(lka); cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        Cc   <- linCmt()
        Adep <- depot             # depot amount
        Cp1  <- peripheral1 / vp
        Cc ~ prop(pSd)
      })
    }))
    .ode <- suppressMessages(rxode2(function() {
      ini({ lka<-log(0.8); lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50) })
      model({
        ka<-exp(lka); cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        kel<-cl/vc; k12<-q/vc; k21<-q/vp
        d/dt(depot)       <- -ka*depot
        d/dt(central)     <-  ka*depot - kel*central - k12*central + k21*peripheral1
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        Cc<-central/vc; Adep<-depot; Cp1<-peripheral1/vp
      })
    }))
    .chk(.lin, .ode, .evPO, c("Cc", "Adep", "Cp1"))
  })

  test_that("3-cmt oral: depot, central, peripheral1 and peripheral2 all resolve together", {
    .lin <- suppressMessages(rxode2(function() {
      ini({ lka<-log(0.8); lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50)
            lq2<-log(1); lvp2<-log(100); pSd<-0.1 })
      model({
        ka<-exp(lka); cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp); q2<-exp(lq2); vp2<-exp(lvp2)
        Cc   <- linCmt()
        Adep <- depot
        Ac   <- central
        Cp1  <- peripheral1 / vp
        Cp2  <- peripheral2 / vp2
        Cc ~ prop(pSd)
      })
    }))
    .ode <- suppressMessages(rxode2(function() {
      ini({ lka<-log(0.8); lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50)
            lq2<-log(1); lvp2<-log(100) })
      model({
        ka<-exp(lka); cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp); q2<-exp(lq2); vp2<-exp(lvp2)
        kel<-cl/vc; k12<-q/vc; k21<-q/vp; k13<-q2/vc; k31<-q2/vp2
        d/dt(depot)       <- -ka*depot
        d/dt(central)     <-  ka*depot - kel*central - k12*central + k21*peripheral1 - k13*central + k31*peripheral2
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        d/dt(peripheral2) <-  k13*central - k31*peripheral2
        Cc<-central/vc; Adep<-depot; Ac<-central; Cp1<-peripheral1/vp; Cp2<-peripheral2/vp2
      })
    }))
    .chk(.lin, .ode, .evPO, c("Cc", "Adep", "Ac", "Cp1", "Cp2"))
  })

  test_that("the same references work without an error model (control)", {
    ## Always worked (no observation compartment injected); guards against the
    ## fix regressing the non-error path.
    .lin <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50) })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        Cc<-linCmt(); Cp1<-peripheral1/vp
      })
    }))
    .ode <- suppressMessages(rxode2(function() {
      ini({ lcl<-log(5); lvc<-log(30); lq<-log(2); lvp<-log(50) })
      model({
        cl<-exp(lcl); vc<-exp(lvc); q<-exp(lq); vp<-exp(lvp)
        kel<-cl/vc; k12<-q/vc; k21<-q/vp
        d/dt(central)     <- -kel*central - k12*central + k21*peripheral1
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        Cc<-central/vc; Cp1<-peripheral1/vp
      })
    }))
    .chk(.lin, .ode, .evIV, c("Cc", "Cp1"))
  })

})
