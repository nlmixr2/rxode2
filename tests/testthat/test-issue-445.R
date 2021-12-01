rxodeTest({

  test_that("rate and duration data mismatches", {

    TV_CLr <- 6.54 # L/h, (CLr/F)
    TV_CLnr <- 2.39 # L/h, (CLnr/F)
    TV_Vc <- 95.1 # L, (V/F)
    TV_alag <- 0.145 # h,
    TV_D <- 0.512 # h,
    TV_Q <- 2.1 # L/h, (Q/F)
    TV_Vp <- 23.3 # L, (Vp/F)

    # zero-order absorption with lag time 2-compartment

    mod <- rxode2({
      CL <- CLr + CLnr
      C2 <- central / Vc * 1000
      d / dt(central) <- -CL / Vc * central - Q / Vc * central + Q / Vp * periph
      d / dt(periph) <- Q / Vc * central - Q / Vp * periph
      alag(central) <- alag
      dur(central) <- D
    })

    ev <- et(amt = 2, cmt = "central", ii = 24, addl = 4) %>%
      et(seq(0, 120, 0.1))

    ### check whether infusion duration was considered in simulation

    theta2 <- c(D = TV_D * 10, CLr = TV_CLr, CLnr = TV_CLnr, Vc = TV_Vc, Vp = TV_Vp, Q = TV_Q, alag = TV_alag)
    #sim2 <- expect_warning(rxSolve(mod, theta2, ev), "dur()")


    ev2 <- et(amt = 2, cmt = "central", ii = 24, addl = 4, rate = -2) %>%
      et(seq(0, 120, 0.1))

    theta2 <- c(D = TV_D * 10, CLr = TV_CLr, CLnr = TV_CLnr, Vc = TV_Vc, Vp = TV_Vp, Q = TV_Q, alag = TV_alag)
    sim2 <- expect_warning(rxSolve(mod, theta2, ev2), NA)

    mod <- rxode2({
      CL <- CLr + CLnr
      C2 <- central / Vc * 1000
      d / dt(central) <- -CL / Vc * central - Q / Vc * central + Q / Vp * periph
      d / dt(periph) <- Q / Vc * central - Q / Vp * periph
      alag(central) <- alag
      rate(central) <- D
    })

    theta2 <- c(D = TV_D * 10, CLr = TV_CLr, CLnr = TV_CLnr, Vc = TV_Vc, Vp = TV_Vp, Q = TV_Q, alag = TV_alag)
    #sim2 <- expect_warning(rxSolve(mod, theta2, ev), "rate()")

    ev3 <- et(amt = 2, cmt = "central", ii = 24, addl = 4, rate = -1) %>%
      et(seq(0, 120, 0.1))

    theta2 <- c(D = TV_D * 10, CLr = TV_CLr, CLnr = TV_CLnr, Vc = TV_Vc, Vp = TV_Vp, Q = TV_Q, alag = TV_alag)
    sim2 <- expect_warning(rxSolve(mod, theta2, ev3), NA)

  })
})
