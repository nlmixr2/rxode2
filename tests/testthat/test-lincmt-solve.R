rxode2Test(
  {
    tol <- 5e-6 ## Current difference for all equations
    type <- 2

    types <- 1:4

    for (type in types) {
      .txt <- switch(type,
        "linear",
        "sensitivity",
        "linear [no save]",
        "advanSens"
      )
      sens <- switch(type,
        "linCmtA",
        "linCmtB",
        "linCmtC",
        "linCmtB"
      )
      sensType <- switch(type,
        "autodiff",
        "autodiff",
        "autodiff",
        "advan"
      )

      etSsB <- et() %>%
        et(amt = 3) %>%
        et(time = 4, amt = 3, ss = 1, ii = 24) %>%
        et(amt = 3, ss = 2, ii = 24, time = 8) %>%
        et(seq(0, 24, length.out = 200))

      etSsI <- et() %>%
        et(amt = 3, rate = 1.5) %>%
        et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24) %>%
        et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24) %>%
        et(seq(0, 24, length.out = 200))

      etSsR <- et(amt = 0, ss = 1, rate = 10000 / 8)

      ode.1c <- rxode2(
        {
          C2 <- center / V
          d / dt(center) <- -CL * C2
        },
        linCmtSens = sens
      )

      sol.1c <- rxode2(
        {
          C2 <- linCmt(CL, V)
        },
        linCmtSens = sens
      )

      context(sprintf("Test steady state solutions 1 cmt (%s)", .txt))

      o1 <- rxSolve(ode.1c, params = c(V = 20, CL = 25), events = etSsB)
      s1 <- rxSolve(sol.1c, params = c(V = 20, CL = 25), events = etSsB, sensType = sensType)
      test_that("one compartment bolus steady state", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      o1 <- rxSolve(ode.1c, params = c(V = 20, CL = 25), events = etSsI)
      s1 <- rxSolve(sol.1c, params = c(V = 20, CL = 25), events = etSsI, sensType = sensType)
      test_that("one compartment infusion tau steady state", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      o1 <- rxSolve(ode.1c, params = c(V = 20, CL = 25), events = etSsR)
      s1 <- rxSolve(sol.1c, params = c(V = 20, CL = 25), events = etSsR, sensType = sensType)
      test_that("one compartment infusion steady state", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      context(sprintf("Test steady state solutions 2 cmt (%s)", .txt))

      ode.2c <- rxode2(
        {
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
        },
        linCmtSens = sens
      )

      sol.2c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q1)
        },
        linCmtSens = sens
      )

      o2 <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = etSsB)
      s2 <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q1 = 10), events = etSsB, sensType = sensType)
      test_that("two compartment bolus steady state", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      o2 <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = etSsI)
      s2 <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q1 = 10), events = etSsI, sensType = sensType)
      test_that("two compartment infusion steady state, tau", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      o2 <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = etSsR)
      s2 <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q1 = 10), events = etSsR, sensType = sensType)
      test_that("two compartment infusion steady state", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      context(sprintf("Test steady state solutions 3 cmt (%s)", .txt))

      ode.3c <- rxode2(
        {
          C2 <- centr / V
          C3 <- peri / V2
          C4 <- peri2 / V3
          d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
          d / dt(peri) <- Q * C2 - Q * C3
          d / dt(peri2) <- Q2 * C2 - Q2 * C4
        },
        linCmtSens = sens
      )

      sol.3c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
        },
        linCmtSens = sens
      )

      o3 <- ode.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSsB)
      s3 <- sol.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSsB, sensType = sensType)
      test_that("three compartment bolus steady state", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      o3 <- ode.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSsI)
      s3 <- sol.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSsI, sensType = sensType)
      test_that("three compartment bolus steady state", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      o3 <- ode.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSsR)
      s3 <- sol.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSsR, sensType = sensType)
      test_that("three compartment bolus steady state", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      context(sprintf("Test steady state solutions 1 cmt ka (%s)", .txt))

      ode.1c.ka <- rxode2(
        {
          C2 <- center / V
          d / dt(depot) <- -KA * depot
          d / dt(center) <- KA * depot - CL * C2
        },
        linCmtSens = sens
      )

      sol.1c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, KA)
        },
        linCmtSens = sens
      )

      o1 <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsB)
      s1 <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsB, sensType = sensType)
      test_that("one compartment bolus steady state to depot compartment", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      o1 <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsI)
      s1 <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsI, sensType = sensType)
      test_that("one compartment infusion steady state to depot compartment, tau", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      o1 <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsR)
      s1 <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsR, sensType = sensType)
      test_that("one compartment infusion steady state to depot compartment", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      etSsB2 <- et() %>%
        et(amt = 3, cmt = 2) %>%
        et(time = 4, amt = 3, ss = 1, ii = 24, cmt = 2) %>%
        et(amt = 3, ss = 2, ii = 24, time = 8, cmt = 2) %>%
        et(seq(0, 24, length.out = 200))

      etSsI2 <- et() %>%
        et(amt = 3, rate = 1.5, cmt = 2) %>%
        et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 2) %>%
        et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 2) %>%
        et(seq(0, 24, length.out = 200))

      etSsR2 <- et(amt = 0, ss = 1, rate = 10000 / 8, cmt = 2)

      o1 <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsB2)
      s1 <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsB2, sensType = sensType)
      test_that("one compartment bolus steady state to central compartment", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      o1 <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsI2)
      s1 <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsI2, sensType = sensType)
      test_that("one compartment infusion steady state to central compartment, tau", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      o1 <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsR2)
      s1 <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = etSsR2, sensType = sensType)
      test_that("one compartment infusion steady state to central compartment", {
        expect_equal(o1$C2, s1$C2, tol = tol)
      })

      context(sprintf("Test steady state solutions 2 cmt ka (%s)", .txt))

      ode.2c.ka <- rxode2(
        {
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(depot) <- -KA * depot
          d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
        },
        linCmtSens = sens
      )

      sol.2c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, KA)
        },
        linCmtSens = sens
      )

      o2 <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsB)
      s2 <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsB, sensType = sensType)
      test_that("two compartment bolus steady state to depot compartment", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      o2 <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsI)
      s2 <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsI, sensType = sensType)
      test_that("two compartment infusion steady state to depot compartment, tau", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })


      o2 <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsR)
      s2 <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsR, sensType = sensType)
      test_that("two compartment infusion steady state to depot compartment, tau", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      o2 <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsB2)
      s2 <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsB2, sensType = sensType)
      test_that("two compartment bolus steady state to central compartment", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      o2 <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsI2)
      s2 <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsI2, sensType = sensType)
      test_that("two compartment infusion steady state to central compartment, tau", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })


      o2 <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsR2)
      s2 <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = etSsR2, sensType = sensType)
      test_that("two compartment infusion steady state to central compartment", {
        expect_equal(o2$C2, s2$C2, tol = tol)
      })

      context(sprintf("Test steady state solutions 3 cmt ka (%s)", .txt))

      ode.3c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
      })

      sol.3c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3, KA)
        },
        linCmtSens = sens
      )

      o3 <- ode.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsB)
      s3 <- sol.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsB, sensType = sensType)
      test_that("three compartment bolus steady state to depot compartment", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      o3 <- ode.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsI)
      s3 <- sol.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsI, sensType = sensType)
      test_that("three compartment infusion steady state to depot compartment, tau", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      o3 <- ode.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsR)
      s3 <- sol.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsR, sensType = sensType)
      test_that("three compartment infusion steady state to depot compartment", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      ## B2
      o3 <- ode.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsB2)
      s3 <- sol.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsB2, sensType = sensType)
      test_that("three compartment bolus steady state to central compartment", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      o3 <- ode.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsI2)
      s3 <- sol.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsI2, sensType = sensType)
      test_that("three compartment infusion steady state to central compartment, tau", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      o3 <- ode.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsR2)
      s3 <- sol.3c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSsR2, sensType = sensType)
      test_that("three compartment infusion steady state to central compartment", {
        expect_equal(o3$C2, s3$C2, tol = tol)
      })

      context(sprintf("Test the solved equations 1 cmt (%s)", .txt))

      et <- eventTable() %>%
        add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      ode.1c <- rxode2({
        C2 <- center / V
        d / dt(center) <- -CL * C2
      })

      test_that("ode model gives extraCmt=0", {
        expect_equal(rxModelVars(ode.1c)$extraCmt, 0L)
      })

      goodP <- function(model, cmt = 1L, ka = 0L) {
        test_that(sprintf("model '%s' parses to cmt=%d, ka=%d", as.character(substitute(model)), cmt, ka), {
          .flags <- rxModelVars(model)$flags
          expect_equal(setNames(.flags["ncmt"], NULL), cmt)
          expect_equal(setNames(.flags["ka"], NULL), ka)
        })
      }

      ## Solved systems can check the variables in the rxode2 statement
      ## to figure out what type of solved system is being requested
      ode.1cs <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.1cs)

      context(sprintf("Test the solved equations 2 cmt (%s)", .txt))

      ode.2cK <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          K <- CLx / V
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.2cK)

      ode.2cA1 <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          alpha <- CLx / V
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.2cA1)

      ode.2cA2 <- rxode2(
        {
          A <- 1 / theta[1]
          CLx <- theta[2]
          alpha <- CLx * A
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.2cA2)

      ## Instead of specifying parameters in the solved system, you can
      ## specify them in the linCmt variable.
      ode.1cs2 <- rxode2(
        {
          C2 <- linCmt(CL, V)
        },
        linCmtSens = sens
      )

      goodP(ode.1cs2)

      test_that("linear compartment model gives extraCmt=1", {
        expect_equal(rxModelVars(ode.1cs2)$extraCmt, 1L)
      })

      ## The solved systems can be mixed with ODE solving routines (to
      ## speed them up a bit...?)

      o.1c <- ode.1c %>% solve(params = c(V = 20, CL = 25), events = et, sensType = sensType)

      s.1c <- ode.1cs2 %>% solve(params = c(V = 20, CL = 25), events = et, sensType = sensType)

      s.2c <- ode.1cs %>% solve(theta = c(20, 25), events = et, sensType = sensType)

      s.2cK <- ode.2cK %>% solve(theta = c(20, 25), events = et, sensType = sensType)
      s.2cA1 <- ode.2cA1 %>% solve(theta = c(20, 25), events = et, sensType = sensType)
      s.2cA2 <- ode.2cA2 %>% solve(theta = c(20, 25), events = et, sensType = sensType)

      test_that("Gives the correct parameters for THETAs", {
        expect_equal(
          s.2c$params,
          structure(list("THETA[1]" = 20, "THETA[2]" = 25),
            class = "data.frame",
            row.names = c(NA, -1L)
          )
        )
      })

      test_that("1 compartment solved models and ODEs same.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2cK$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2cA1$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2cA2$C2, tolerance = tol)
      })

      ## Test steady state doses.
      etSs <- et() %>%
        et(amt = 3) %>%
        et(time = 4, amt = 3, ss = 1, ii = 24) %>%
        et(amt = 3, ss = 2, ii = 24, time = 8) %>%
        et(seq(0, 24, length.out = 200))

      o.1c <- ode.1c %>% solve(params = c(V = 20, CL = 1), events = etSs, sensType = sensType)

      s.1c <- ode.1cs2 %>% solve(params = c(V = 20, CL = 1), events = etSs, sensType = sensType)

      s.2c <- ode.1cs %>% solve(theta = c(20, 1), events = etSs, sensType = sensType)

      test_that("1 compartment steady-state solved models and ODEs same.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2c$C2, tolerance = tol)
      })

      ode.1c.ka <- rxode2({
        C2 <- center / V
        d / dt(depot) <- -KA * depot
        d / dt(center) <- KA * depot - CL * C2
      })

      sol.1c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, KA)
        },
        linCmtSens = sens
      )

      goodP(sol.1c.ka, ka = 1L)

      ode.2cK <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          Ka <- theta[3]
          K <- CLx / V
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.2cK, ka = 1L)

      ode.2cA1 <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          Ka <- theta[3]
          alpha <- CLx / V
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.2cA1, ka = 1L)

      ode.2cA2 <- rxode2(
        {
          A <- 1 / theta[1]
          CLx <- theta[2]
          Ka <- theta[3]
          alpha <- CLx * A
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.2cA2, ka = 1L)

      test_that("linear oral model gives extraCmt=2", {
        expect_equal(rxModelVars(sol.1c.ka)$extraCmt, 2L)
      })

      o.1c <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = et, sensType = sensType)

      s.1c <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = et, sensType = sensType)

      s.2cK <- ode.2cK %>% solve(theta = c(20, 25, KA = 2), events = et, sensType = sensType)
      s.2cA1 <- ode.2cA1 %>% solve(theta = c(20, 25, KA = 2), events = et, sensType = sensType)
      s.2cA2 <- ode.2cA2 %>% solve(theta = c(20, 25, KA = 2), events = et, sensType = sensType)

      test_that("1 compartment oral solved models and ODEs same.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2cK$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2cA1$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2cA2$C2, tolerance = tol)
      })

      ## Note the strange-looking dip at 4 hours.  This is because ss=1 resets the system first.
      o.1c <- ode.1c.ka %>% solve(params = c(V = 20, CL = 2, KA = 2), events = etSs, sensType = sensType)

      s.1c <- sol.1c.ka %>% solve(params = c(V = 20, CL = 2, KA = 2), events = etSs, sensType = sensType)

      test_that("1 compartment oral solved models steady state ODEs same.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
      })

      ode.2c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
      })

      sol.2c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q1)
        },
        linCmtSens = sens
      )

      goodP(sol.2c, cmt = 2L)

      sol.2cK <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          K <- CLx / V
          K12 <- Qx / V
          K21 <- Qx / V2x
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cK, cmt = 2L)

      ## A1 in terms of A, alpha, B, beta

      sol.2cA1 <- rxode2(
        {
          Vx <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Kx <- CLx / Vx
          K12x <- Qx / Vx
          K21x <- Qx / V2x
          beta <- 0.5 * (K12x + K21x + Kx -
            sqrt((K12x +
              K21x + Kx) * (K12x + K21x + Kx) - 4 * K21x *
              Kx))
          alpha <- K21x * Kx / beta
          A <- (alpha - K21x) / (alpha - beta) / Vx
          B <- (beta - K21x) / (beta - alpha) / Vx
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cA1, cmt = 2L)

      ## A2 V, alpha, beta, k21
      sol.2cA2 <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Kx <- CLx / V
          K12x <- Qx / V
          K21 <- Qx / V2x
          beta <- 0.5 * (K12x + K21 + Kx -
            sqrt((K12x +
              K21 + Kx) * (K12x + K21 + Kx) - 4 * K21 *
              Kx))
          alpha <- K21 * Kx / beta
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cA2, cmt = 2L)

      ## A3 alpha, beta, aob
      sol.2cA3 <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Kx <- CLx / V
          K12x <- Qx / V
          K21x <- Qx / V2x
          beta <- 0.5 * (K12x + K21x + Kx -
            sqrt((K12x +
              K21x + Kx) * (K12x + K21x + Kx) - 4 * K21x *
              Kx))
          alpha <- K21x * Kx / beta
          Ax <- (alpha - K21x) / (alpha - beta) / V
          Bx <- (beta - K21x) / (beta - alpha) / V
          aob <- Ax / Bx
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cA3, cmt = 2L)

      o.2c <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)

      s.2c <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q1 = 10), events = et, sensType = sensType)

      s.2cK <- sol.2cK %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)

      s.2cA1 <- sol.2cA1 %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)

      s.2cA2 <- sol.2cA2 %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)
      s.2cA3 <- sol.2cA3 %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)

      test_that("2 compartment solved models and ODEs same.", {
        expect_equal(s.2cK$C2, s.2c$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cK$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cA1$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cA2$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cA3$C2, tolerance = tol)
      })

      ode.2c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
      })

      sol.2c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, KA)
        },
        linCmtSens = sens
      )

      goodP(sol.2c.ka, cmt = 2, ka = 1)

      sol.2cK <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          ka <- theta[5]
          K <- CLx / V
          K12 <- Qx / V
          K21 <- Qx / V2x
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cK, cmt = 2, ka = 1)

      ## A1 in terms of A, alpha, B, beta

      sol.2cA1 <- rxode2(
        {
          Vx <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          ka <- theta[5]
          Kx <- CLx / Vx
          K12x <- Qx / Vx
          K21x <- Qx / V2x
          beta <- 0.5 * (K12x + K21x + Kx -
            sqrt((K12x +
              K21x + Kx) * (K12x + K21x + Kx) - 4 * K21x *
              Kx))
          alpha <- K21x * Kx / beta
          A <- (alpha - K21x) / (alpha - beta) / Vx
          B <- (beta - K21x) / (beta - alpha) / Vx
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cA1, cmt = 2, ka = 1)

      ## A2 V, alpha, beta, k21
      sol.2cA2 <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          ka <- theta[5]
          Kx <- CLx / V
          K12x <- Qx / V
          K21 <- Qx / V2x
          beta <- 0.5 * (K12x + K21 + Kx -
            sqrt((K12x +
              K21 + Kx) * (K12x + K21 + Kx) - 4 * K21 *
              Kx))
          alpha <- K21 * Kx / beta
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cA2, cmt = 2, ka = 1)

      ## A3 alpha, beta, aob
      sol.2cA3 <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          ka <- theta[5]
          Kx <- CLx / V
          K12x <- Qx / V
          K21x <- Qx / V2x
          beta <- 0.5 * (K12x + K21x + Kx -
            sqrt((K12x +
              K21x + Kx) * (K12x + K21x + Kx) - 4 * K21x *
              Kx))
          alpha <- K21x * Kx / beta
          Ax <- (alpha - K21x) / (alpha - beta) / V
          Bx <- (beta - K21x) / (beta - alpha) / V
          aob <- Ax / Bx
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cA3, cmt = 2, ka = 1)

      sol.2cSS <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          V2x <- theta[3]
          Q <- theta[4]
          Ka <- theta[5]
          Vss <- V + V2x
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cSS, cmt = 2, ka = 1)

      sol.2cT <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          VT <- theta[3]
          Q <- theta[4]
          Ka <- theta[5]
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.2cT, cmt = 2, ka = 1)

      o.2c <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = et, sensType = sensType)
      s.2c <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = et, sensType = sensType)
      s.2cK <- sol.2cK %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, ka = 0.3), events = et, sensType = sensType)
      s.2cA1 <- sol.2cA1 %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, ka = 0.3), events = et, sensType = sensType)
      s.2cA2 <- sol.2cA2 %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, ka = 0.3), events = et, sensType = sensType)
      s.2cA3 <- sol.2cA3 %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, ka = 0.3), events = et, sensType = sensType)
      s.2cSS <- sol.2cSS %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, ka = 0.3), events = et, sensType = sensType)
      s.2cT <- sol.2cT %>% solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, ka = 0.3), events = et, sensType = sensType)

      test_that("2 compartment oral solved models and ODEs same.", {
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cK$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cA1$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cA2$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cA3$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cSS$C2, tolerance = tol)
        expect_equal(o.2c$C2, s.2cT$C2, tolerance = tol)
      })

      o.2c <- ode.2c.ka %>% solve(params = c(V = 40, CL = 1, V2 = 297, Q = 10, KA = 0.3), events = etSs, sensType = sensType)

      s.2c <- sol.2c.ka %>% solve(params = c(V = 40, CL = 1, V2 = 297, Q = 10, KA = 0.3), events = etSs, sensType = sensType)

      test_that("2 compartment oral steady-state solved models and ODEs same.", {
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
      })

      context(sprintf("Test the solved equations 3 cmt (%s)", .txt))

      ode.3c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
      })

      sol.3c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
        },
        linCmtSens = sens
      )

      goodP(sol.3c, 3)

      sol.3cK <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Q2x <- theta[5]
          V3x <- theta[6]
          K <- CLx / V
          K12 <- Qx / V
          K21 <- Qx / V2x
          k13 <- Q2x / V
          k31 <- Q2x / V3x
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.3cK, 3)

      sol.3cA1 <- rxode2(
        {
          Vx <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Q2x <- theta[5]
          V3x <- theta[6]
          Kx <- CLx / Vx
          K12x <- Qx / Vx
          K21x <- Qx / V2x
          K13x <- Q2x / Vx
          K31x <- Q2x / V3x
          a0 <- Kx * K21x * K31x
          a1 <- Kx * K31x + K21x * K31x + K21x * K13x +
            Kx * K21x + K31x * K12x
          a2 <- Kx + K12x + K13x + K21x + K31x
          p <- a1 - a2 * a2 / 3
          qq <- 2 * a2 * a2 * a2 / 27 - a1 * a2 / 3 + a0
          r1 <- sqrt(-p * p * p / 27)
          r2 <- 2 * r1^(1 / 3)
          theta <- acos(-qq / (2 * r1)) / 3
          alpha <- -(cos(theta) * r2 - a2 / 3)
          beta <- -(cos(theta + 2 / 3 * pi) * r2 - a2 / 3)
          gamma <- -(cos(theta + 4 / 3 * pi) * r2 - a2 / 3)
          A <- (K21x - alpha) * (K31x - alpha) / (alpha - beta) / (alpha - gamma) / Vx
          B <- (K21x - beta) * (K31x - beta) / (beta - alpha) / (beta - gamma) / Vx
          C <- (K21x - gamma) * (K31x - gamma) / (gamma - alpha) / (gamma - beta) / Vx
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.3cA1, 3)

      sol.3cVp <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          Vp <- theta[3]
          Q <- theta[4]
          Q2 <- theta[5]
          Vp2 <- theta[6]
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.3cVp, 3)

      sol.3cVt <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          Vt <- theta[3]
          Q <- theta[4]
          Q2 <- theta[5]
          Vt2 <- theta[6]
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.3cVt, 3)

      o.3c <- ode.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)

      s.3c <- sol.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)

      s.3cK <- sol.3cK %>%
        solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)
      s.3cA1 <- sol.3cA1 %>%
        solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)
      s.3cVp <- sol.3cVp %>%
        solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)
      s.3cVt <- sol.3cVt %>%
        solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)

      test_that("3 compartment solved models and ODEs same.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
        expect_equal(o.3c$C2, s.3cK$C2, tolerance = tol)
        expect_equal(o.3c$C2, s.3cA1$C2, tolerance = tol)
        expect_equal(o.3c$C2, s.3cVp$C2, tolerance = tol)
        expect_equal(o.3c$C2, s.3cVt$C2, tolerance = tol)
      })

      o.3c <- ode.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSs, sensType = sensType)

      s.3c <- sol.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSs, sensType = sensType)

      test_that("3 compartment solved models and ODEs same with steady state.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      ode.3c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
      })

      sol.3c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3, KA)
        },
        linCmtSens = sens
      )

      goodP(sol.3c.ka, 3, 1)

      sol.3cK <- rxode2(
        {
          V <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Q2x <- theta[5]
          V3x <- theta[6]
          ka <- theta[7]
          K <- CLx / V
          K12 <- Qx / V
          K21 <- Qx / V2x
          k13 <- Q2x / V
          k31 <- Q2x / V3x
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.3cK, 3, 1)

      sol.3cA1 <- rxode2(
        {
          Vx <- theta[1]
          CLx <- theta[2]
          V2x <- theta[3]
          Qx <- theta[4]
          Q2x <- theta[5]
          V3x <- theta[6]
          ka <- theta[7]
          Kx <- CLx / Vx
          K12x <- Qx / Vx
          K21x <- Qx / V2x
          K13x <- Q2x / Vx
          K31x <- Q2x / V3x
          a0 <- Kx * K21x * K31x
          a1 <- Kx * K31x + K21x * K31x + K21x * K13x +
            Kx * K21x + K31x * K12x
          a2 <- Kx + K12x + K13x + K21x + K31x
          p <- a1 - a2 * a2 / 3
          qq <- 2 * a2 * a2 * a2 / 27 - a1 * a2 / 3 + a0
          r1 <- sqrt(-p * p * p / 27)
          r2 <- 2 * r1^(1 / 3)
          theta <- acos(-qq / (2 * r1)) / 3
          alpha <- -(cos(theta) * r2 - a2 / 3)
          beta <- -(cos(theta + 2 / 3 * pi) * r2 - a2 / 3)
          gamma <- -(cos(theta + 4 / 3 * pi) * r2 - a2 / 3)
          A <- (K21x - alpha) * (K31x - alpha) / (alpha - beta) / (alpha - gamma) / Vx
          B <- (K21x - beta) * (K31x - beta) / (beta - alpha) / (beta - gamma) / Vx
          C <- (K21x - gamma) * (K31x - gamma) / (gamma - alpha) / (gamma - beta) / Vx
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(sol.3cA1, 3, 1)

      o.3c <- ode.3c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = et, sensType = sensType)
      s.3c <- sol.3c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = et, sensType = sensType)
      s.3cK <- sol.3cK %>%
        solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = et, sensType = sensType)
      s.3cA1 <- sol.3cA1 %>%
        solve(theta = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = et, sensType = sensType)

      test_that("3 compartment oral solved models and ODEs same.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
        expect_equal(o.3c$C2, s.3cK$C2, tolerance = tol)
        expect_equal(o.3c$C2, s.3cA1$C2, tolerance = tol)
      })

      o.3c <- ode.3c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSs, sensType = sensType)
      s.3c <- sol.3c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = etSs, sensType = sensType)

      ## Again the 4 hour strange discontinuity because ss=1
      test_that("3 compartment oral solved models and ODEs same for steady state.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      context(sprintf("Infusion Models 1 cmt (%s)", .txt))

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      etSs <- et() %>%
        et(amt = 3, rate = 1.5) %>%
        et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24) %>%
        et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24) %>%
        et(seq(0, 24, length.out = 200))

      ode.1c <- rxode2({
        C2 <- center / V
        d / dt(center) <- -CL * C2
      })

      ## Solved systems can check the variables in the rxode2 statement
      ## to figure out what type of solved system is being requested
      ode.1cs <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.1cs, 1)

      ## Instead of specifying parameters in the solved system, you can
      ## specify them in the linCmt variable.
      ode.1cs2 <- rxode2(
        {
          C2 <- linCmt(CL, V)
        },
        linCmtSens = sens
      )

      goodP(ode.1cs2, 1)

      ## The solved systems can be mixed with ODE solving routines (to
      ## speed them up a bit...?)

      o.1c <- ode.1c %>%
        solve(params = c(V = 20, CL = 25), events = et, sensType = sensType)
      s.1c <- ode.1cs2 %>%
        solve(params = c(V = 20, CL = 25), events = et, sensType = sensType)
      s.2c <- ode.1cs %>%
        solve(theta = c(20, 25), events = et, sensType = sensType)

      test_that("1 compartment solved models and ODEs same.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2c$C2, tolerance = tol)
      })

      o.1c <- ode.1c %>%
        solve(params = c(V = 20, CL = 10), events = etSs, sensType = sensType)
      s.1c <- ode.1cs2 %>%
        solve(params = c(V = 20, CL = 10), events = etSs, sensType = sensType)
      s.2c <- ode.1cs %>%
        solve(theta = c(20, 10), events = etSs, sensType = sensType)

      test_that("1 compartment solved models and ODEs same; Steady State", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2c$C2, tolerance = tol)
      })

      context(sprintf("Infusion Models 2 cmt (%s)", .txt))

      ode.2c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
      })

      sol.2c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q)
        },
        linCmtSens = sens
      )

      goodP(sol.2c, 2)

      o.2c <- ode.2c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)
      s.2c <- sol.2c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)

      test_that("2 compartment solved models and ODEs same.", {
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
      })

      o.2c <- ode.2c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = etSs, sensType = sensType)
      s.2c <- sol.2c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = etSs, sensType = sensType)

      test_that("2 compartment steady state solved models and ODEs same.", {
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
      })

      context(sprintf("Infusion Models 3 cmt (%s)", .txt))

      ode.3c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
      })

      sol.3c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
        },
        linCmtSens = sens
      )

      goodP(sol.3c, 3)

      o.3c <- ode.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)
      s.3c <- sol.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)

      test_that("3 compartment solved models and ODEs same.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      o.3c <- ode.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSs, sensType = sensType)
      s.3c <- sol.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = etSs, sensType = sensType)

      test_that("3 compartment steady state solved models and ODEs same.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      context(sprintf("Infusion + Bolus 1 cmt (%s)", .txt))

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8) %>%
        add.dosing(dose = 1.5, nbr.doses = 6, dosing.interval = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      ode.1c <- rxode2({
        C2 <- center / V
        d / dt(center) <- -CL * C2
      })

      ## Solved systems can check the variables in the rxode2 statement
      ## to figure out what type of solved system is being requested
      ode.1cs <- rxode2(
        {
          V <- theta[1]
          CL <- theta[2]
          C2 <- linCmt()
        },
        linCmtSens = sens
      )

      goodP(ode.1cs, 1)

      ## Instead of specifying parameters in the solved system, you can
      ## specify them in the linCmt variable.
      ode.1cs2 <- rxode2(
        {
          C2 <- linCmt(CL, V)
        },
        linCmtSens = sens
      )

      goodP(ode.1cs2, 1)

      ## The solved systems can be mixed with ODE solving routines (to
      ## speed them up a bit...?)

      o.1c <- ode.1c %>% solve(params = c(V = 20, CL = 25), events = et, sensType = sensType)

      s.1c <- ode.1cs2 %>% solve(params = c(V = 20, CL = 25), events = et, sensType = sensType)

      s.2c <- ode.1cs %>% solve(theta = c(20, 25), events = et, sensType = sensType)

      test_that("1 compartment solved models and ODEs same.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        expect_equal(o.1c$C2, s.2c$C2, tolerance = tol)
      })

      context(sprintf("Infusion + Bolus 2 cmt (%s)", .txt))

      ode.2c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
      })

      sol.2c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q)
        },
        linCmtSens = sens
      )

      goodP(sol.2c, 2)

      o.2c <- ode.2c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)
      s.2c <- sol.2c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = et, sensType = sensType)

      test_that("2 compartment solved models and ODEs same.", {
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
      })

      context(sprintf("Infusion + Bolus 3 cmt (%s)", .txt))

      ode.3c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
      })

      sol.3c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
        },
        linCmtSens = sens
      )

      goodP(sol.3c, 3)

      o.3c <- ode.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)
      s.3c <- sol.3c %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = et, sensType = sensType)

      test_that("3 compartment solved models and ODEs same.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      context(sprintf("Oral + Infusion + Bolus Models 1 cmt (%s)", .txt))

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
        add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
        add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 1, start.time = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      ode.1c.ka <- rxode2({
        C2 <- center / V
        d / dt(depot) <- -KA * depot
        d / dt(center) <- KA * depot - CL * C2
      })

      sol.1c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, KA)
        },
        linCmtSens = sens
      )

      goodP(sol.1c.ka, 1, 1)

      o.1c <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = et, sensType = sensType)
      s.1c <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2), events = et, sensType = sensType)

      test_that("1 compartment solved models and ODEs same for mixed oral, iv and infusion.", {
        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
      })

      context(sprintf("Oral + Infusion + Bolus Models 2 cmt (%s)", .txt))

      ode.2c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
      })

      sol.2c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, KA)
        },
        linCmtSens = sens
      )

      goodP(sol.2c.ka, 2, 1)

      o.2c <- ode.2c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = et, sensType = sensType)
      s.2c <- sol.2c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), events = et, sensType = sensType)

      test_that("2 compartment solved models and ODEs same for mixed oral, iv and infusion.", {
        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
      })

      context(sprintf("Oral + Infusion + Bolus Models 3 cmt (%s)", .txt))

      ode.3c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
      })

      sol.3c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3, KA)
        },
        linCmtSens = sens
      )

      goodP(sol.3c.ka, 3, 1)

      o.3c <- ode.3c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = et, sensType = sensType)
      s.3c <- sol.3c.ka %>%
        solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3), events = et, sensType = sensType)

      test_that("3 compartment solved models and ODEs same for mixed oral, iv and infusion.", {
        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      context(sprintf("Modeled bio-availability 1 cmt (%s)", .txt))

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
        add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
        add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 1, start.time = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      ode.1c.ka <- rxode2({
        C2 <- center / V
        d / dt(depot) <- -KA * depot
        d / dt(center) <- KA * depot - CL * C2
        f(depot) <- fDepot
        f(center) <- fCenter
      })

      sol.1c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, KA)
          f(depot) <- fDepot
          f(central) <- fCenter
        },
        linCmtSens = sens
      )

      goodP(sol.1c.ka, 1, 1)

      for (fd in c(0.5, 1, 2)) {
        for (fc in c(0.5, 1, 2)) {
          o.1c <- ode.1c.ka %>%
            solve(params = c(V = 20, CL = 25, KA = 2, fDepot = fd, fCenter = fc), events = et, sensType = sensType)
          s.1c <- sol.1c.ka %>%
            solve(params = c(V = 20, CL = 25, KA = 2, fDepot = fd, fCenter = fc), events = et, sensType = sensType)
          test_that(sprintf("1 compartment solved models and ODEs same for mixed oral, iv and infusion + Fd=%f,Fc=%f", fd, fc), {
            expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
          })
        }
      }

      context(sprintf("Modeled bio-availability 2 cmt (%s)", .txt))

      ode.2c.ka <- rxode2(
        {
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(depot) <- -KA * depot
          d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
          f(depot) <- fDepot
          f(centr) <- fCenter
          ## FIXME:
          ## f(central) should throw an error
        },
        linCmtSens = sens
      )

      sol.2c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, KA)
          f(depot) <- fDepot
          f(central) <- fCenter
        },
        linCmtSens = sens
      )

      goodP(sol.2c.ka, 2, 1)

      for (fd in c(0.5, 1, 2)) {
        for (fc in c(0.5, 1, 2)) {
          o.2c <- ode.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3, fDepot = fd, fCenter = fc), events = et, sensType = sensType)
          s.2c <- sol.2c.ka %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3, fDepot = fd, fCenter = fc), events = et, sensType = sensType)
          test_that(sprintf("2 compartment solved models and ODEs same for mixed oral, iv and infusion + Fd=%f,Fc=%f", fd, fc), {
            expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
          })
        }
      }

      context(sprintf("Modeled bio-availability 3 cmt (%s)", .txt))

      ode.3c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
        f(depot) <- fDepot
        f(centr) <- fCenter
      })

      sol.3c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3, KA)
          f(depot) <- fDepot
          f(central) <- fCenter
        },
        linCmtSens = sens
      )

      goodP(sol.3c.ka, 3, 1)

      for (fd in c(0.5, 1, 2)) {
        for (fc in c(0.5, 1, 2)) {
          o.3c <- ode.3c.ka %>%
            solve(params = c(
              V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7,
              V3 = 400, KA = 0.3, fDepot = fd, fCenter = fc
            ), events = et, sensType = sensType)
          s.3c <- sol.3c.ka %>%
            solve(params = c(
              V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3,
              fDepot = fd, fCenter = fc
            ), events = et, sensType = sensType)
          test_that(sprintf("3 compartment solved models and ODEs same for mixed oral, iv and infusion + Fd=%f,Fc=%f", fd, fc), {
            expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
          })
        }
      }

      context(sprintf("Modeled lag time 1 cmt (%s)", .txt))

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
        add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
        add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 1, start.time = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      ode.1c.ka <- rxode2(
        {
          C2 <- center / V
          d / dt(depot) <- -KA * depot
          d / dt(center) <- KA * depot - CL * C2
          alag(depot) <- lagDepot
          alag(center) <- lagCenter
        },
        linCmtSens = sens
      )

      sol.1c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, KA)
          alag(depot) <- lagDepot
          alag(central) <- lagCenter
        },
        linCmtSens = sens
      )

      goodP(sol.1c.ka, 1, 1)

      for (fd in c(1, 2, 10)) {
        for (fc in c(1, 2, 10)) {
          o.1c <- ode.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2, lagDepot = fd, lagCenter = fc), events = et, sensType = sensType)
          s.1c <- sol.1c.ka %>% solve(params = c(V = 20, CL = 25, KA = 2, lagDepot = fd, lagCenter = fc), events = et, sensType = sensType)
          test_that(sprintf("1 compartment solved models and ODEs same for mixed oral, iv and infusion + Fd=%f,Fc=%f", fd, fc), {
            expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
          })
        }
      }

      context(sprintf("Modeled lag time 2 cmt (%s)", .txt))

      ode.2c.ka <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        alag(depot) <- lagDepot
        alag(centr) <- lagCenter
      })

      sol.2c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, KA)
          alag(depot) <- lagDepot
          alag(central) <- lagCenter
        },
        linCmtSens = sens
      )

      goodP(sol.2c.ka, 2, 1)

      for (fd in c(1, 2, 10)) {
        for (fc in c(1, 2, 10)) {
          o.2c <- ode.2c.ka %>%
            solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3, lagDepot = fd, lagCenter = fc), events = et, sensType = sensType)
          s.2c <- sol.2c.ka %>%
            solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3, lagDepot = fd, lagCenter = fc), events = et, sensType = sensType)
          test_that(sprintf("2 compartment solved models and ODEs same for mixed oral, iv and infusion + Fd=%f,Fc=%f", fd, fc), {
            expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
          })
        }
      }

      context(sprintf("Modeled lag time 3 cmt (%s)", .txt))

      ode.3c.ka <- rxode2(
        {
          C2 <- centr / V
          C3 <- peri / V2
          C4 <- peri2 / V3
          d / dt(depot) <- -KA * depot
          d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
          d / dt(peri) <- Q * C2 - Q * C3
          d / dt(peri2) <- Q2 * C2 - Q2 * C4
          alag(depot) <- lagDepot
          alag(centr) <- lagCenter
        },
        linCmtSens = sens
      )

      sol.3c.ka <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3, KA)
          alag(depot) <- lagDepot
          alag(central) <- lagCenter
        },
        linCmtSens = sens
      )

      goodP(sol.3c.ka, 3, 1)

      for (fd in c(1, 2, 10)) {
        for (fc in c(1, 2, 10)) {
          o.3c <- ode.3c.ka %>% solve(params = c(
            V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7,
            V3 = 400, KA = 0.3, lagDepot = fd, lagCenter = fc
          ), events = et, sensType = sensType)
          s.3c <- sol.3c.ka %>% solve(params = c(
            V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, KA = 0.3,
            lagDepot = fd, lagCenter = fc
          ), events = et, sensType = sensType)
          test_that(sprintf("3 compartment solved models and ODEs same for mixed oral, iv and infusion + Fd=%f,Fc=%f", fd, fc), {
            expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
          })
        }
      }

      context(sprintf("Modeled rate 1 cmt (%s)", .txt))

      ode.1c <- rxode2({
        C2 <- center / V
        d / dt(center) <- -CL * C2
        rate(center) <- rt
      })

      sol.1c <- rxode2(
        {
          C2 <- linCmt(CL, V)
          rate(central) <- rt
        },
        linCmtSens = sens
      )

      goodP(sol.1c, 1)

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = -1, nbr.doses = 3, cmt = 1, dosing.interval = 12) %>%
        add.sampling(seq(0, 36, length.out = 200))

      for (rt in seq(0.5, 1, 1.5)) {
        o.1c <- ode.1c %>% solve(params = c(V = 20, CL = 25, rt = rt), events = et, sensType = sensType)
        s.1c <- sol.1c %>% solve(params = c(V = 20, CL = 25, rt = rt), events = et, sensType = sensType)
        test_that(sprintf("1 compartment solved models and ODEs same for rate-modeled infusion: %s", rt), {
          expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        })
      }

      context(sprintf("Modeled rate 2 cmt (%s)", .txt))

      ode.2c <- rxode2(
        {
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
          rate(centr) <- rt
        },
        linCmtSens = sens
      )

      sol.2c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q)
          rate(central) <- rt
        },
        linCmtSens = sens
      )

      goodP(sol.2c, 2)

      for (rt in seq(0.5, 1, 1.5)) {
        o.2c <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, rt = rt), events = et, sensType = sensType)
        s.2c <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, rt = rt), events = et, sensType = sensType)
        test_that(sprintf("2 compartment solved models and ODEs same for rate-modeled infusion: %s", rt), {
          expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
        })
      }

      context(sprintf("Modeled rate 3 cmt (%s)", .txt))

      ode.3c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
        rate(centr) <- rt
      })

      sol.3c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
          rate(central) <- rt
        },
        linCmtSens = sens
      )

      goodP(sol.3c, 3)

      for (rt in seq(0.5, 1, 1.5)) {
        s.3c <- sol.3c %>%
          solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, rt = rt), events = et, sensType = sensType)
        o.3c <- ode.3c %>%
          solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, rt = rt), events = et, sensType = sensType)
        test_that(sprintf("3 compartment solved models and ODEs same for rate-modeled infusion: %s", rt), {
          expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
        })
      }

      context(sprintf("Modeled duration 1 cmt (%s)", .txt))

      ode.1c <- rxode2({
        C2 <- center / V
        d / dt(center) <- -CL * C2
        dur(center) <- dr
      })

      sol.1c <- rxode2(
        {
          C2 <- linCmt(CL, V)
          dur(central) <- dr
        },
        linCmtSens = sens
      )

      goodP(sol.1c, 1)

      et <- eventTable() %>%
        add.dosing(dose = 3, rate = -2, nbr.doses = 3, cmt = 1, dosing.interval = 12) %>%
        add.sampling(seq(0, 36, length.out = 200))

      for (dur in seq(0.5, 1, 1.5)) {
        o.1c <- ode.1c %>% solve(params = c(V = 20, CL = 25, dr = dur), events = et, sensType = sensType)
        s.1c <- sol.1c %>% solve(params = c(V = 20, CL = 25, dr = dur), events = et, sensType = sensType)
        test_that(sprintf("1 compartment solved models and ODEs same for dur-modeled infusion: %s", dur), {
          expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)
        })
      }

      context(sprintf("Modeled duration 2 cmt (%s)", .txt))

      ode.2c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        dur(centr) <- dr
      })

      sol.2c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q)
          dur(central) <- dr
        },
        linCmtSens = sens
      )

      goodP(sol.2c, 2)

      for (dur in seq(0.5, 1, 1.5)) {
        o.2c <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, dr = dur), events = et, sensType = sensType)
        s.2c <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, dr = dur), events = et, sensType = sensType)
        test_that(sprintf("2 compartment solved models and ODEs same for dur-modeled infusion: %s", dur), {
          expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)
        })
      }

      context(sprintf("Modeled duration 3 cmt (%s)", .txt))

      ode.3c <- rxode2({
        C2 <- centr / V
        C3 <- peri / V2
        C4 <- peri2 / V3
        d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(peri2) <- Q2 * C2 - Q2 * C4
        dur(centr) <- dr
      })

      sol.3c <- rxode2(
        {
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
          dur(central) <- dr
        },
        linCmtSens = sens
      )

      goodP(sol.3c, 3)

      for (dur in seq(0.5, 1, 1.5)) {
        o.3c <- ode.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, dr = dur), events = et, sensType = sensType)
        s.3c <- sol.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400, dr = dur), events = et, sensType = sensType)
        test_that(sprintf("3 compartment solved models and ODEs same for dur-modeled infusion: %s", dur), {
          expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
        })
      }

      test_that("central should throw error", {
        expect_error(rxode2({
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(depot) <- -KA * depot
          d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
          alag(depot) <- lagDepot
          alag(centr) <- lagCenter
          alag(central) <- matt
        }))
      })

      test_that("depot should throw error", {
        expect_error(rxode2({
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(dep) <- -KA * dep
          d / dt(centr) <- KA * dep - CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
          alag(dep) <- lagDepot
          alag(centr) <- lagCenter
          alag(depot) <- matt
        }))
      })

      ode.1cs2 <- rxode2(
        {
          C2 <- linCmt(CL, V)
          mtime(t1) <- mt1
          mtime(t2) <- mt2
        },
        linCmtSens = sens
      )

      goodP(ode.1cs2, 1)

      et <- eventTable() %>%
        add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
        add.sampling(0:48)

      s.1c <- ode.1cs2 %>% solve(
        params = c(V = 20, CL = 25, mt1 = 0.5, mt2 = 1.75),
        events = et, sensType = sensType
      )

      test_that("mtime with solved systems work", {
        expect_equal(s.1c$time[1:4], c(0, 0.5, 1, 1.75))
      })

      context(sprintf("evid=3 (%s)", .txt))

      test_that(sprintf("evid==3 (%s)", .txt), {
        ode.1c <- rxode2(
          {
            C2 <- center / V
            d / dt(center) <- -CL * C2
          },
          linCmtSens = sens
        )

        sol.1c <- rxode2(
          {
            C2 <- linCmt(CL, V)
          },
          linCmtSens = sens
        )

        et <- eventTable() %>%
          add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
          et(time = 25, evid = 3) %>%
          add.sampling(seq(0, 48, length.out = 200))

        o1 <- rxSolve(ode.1c, params = c(V = 20, CL = 25), events = et)
        s1 <- rxSolve(sol.1c, params = c(V = 20, CL = 25), events = et, sensType = sensType)

        expect_equal(o1$C2, s1$C2, tol = tol)
      })
    }
    if (length(types) > 1) {
      test_that(
        "double linCmt has error",
        expect_error(rxode2({
          C2 <- linCmt(CL, V)
          C2 <- linCmt(CL, V)
        }))
      )

      context(sprintf("Steady State Infusions (%s)", .txt))

      test_that("Steady state IV infusion", {
        ev <- et(amt = 0, ss = 1, rate = 10000 / 8)

        ode.1c <- rxode2({
          C2 <- center / V
          d / dt(center) <- -CL * C2
        })

        ode.1cs2 <- rxode2({
          C2 <- linCmt(CL, V)
        })

        goodP(ode.1cs2, 1)

        o.1c <- ode.1c %>% solve(params = c(V = 20, CL = 25), events = ev, sensType = sensType)

        s.1c <- ode.1cs2 %>% solve(params = c(V = 20, CL = 25), events = ev, sensType = sensType)

        expect_equal(o.1c$C2, s.1c$C2, tolerance = tol)

        ode.2c <- rxode2({
          C2 <- centr / V
          C3 <- peri / V2
          d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
          d / dt(peri) <- Q * C2 - Q * C3
        })

        sol.2c <- rxode2({
          C2 <- linCmt(V, CL, V2, Q1)
        })

        goodP(sol.2c, 2)

        o.2c <- ode.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10), events = ev, sensType = sensType)

        s.2c <- sol.2c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q1 = 10), events = ev, sensType = sensType)

        expect_equal(o.2c$C2, s.2c$C2, tolerance = tol)

        ode.3c <- rxode2({
          C2 <- centr / V
          C3 <- peri / V2
          C4 <- peri2 / V3
          d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3 - Q2 * C2 + Q2 * C4
          d / dt(peri) <- Q * C2 - Q * C3
          d / dt(peri2) <- Q2 * C2 - Q2 * C4
        })

        sol.3c <- rxode2({
          C2 <- linCmt(V, CL, V2, Q, Q2, V3)
        })

        goodP(sol.3c, 3)

        o.3c <- ode.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = ev, sensType = sensType)

        s.3c <- sol.3c %>% solve(params = c(V = 40, CL = 18, V2 = 297, Q = 10, Q2 = 7, V3 = 400), events = ev, sensType = sensType)

        expect_equal(o.3c$C2, s.3c$C2, tolerance = tol)
      })

      context("Issue #258")

      test_that("Issue #258", {
        m258 <- rxode2(
          {
            ka <- 1
            cl <- 3.5
            vc <- 40
            Conc <- linCmt()
            alag(depot) <- 1
          },
          linCmtSens = sens
        )

        m258o <- rxode2({
          ka <- 1
          cl <- 3.5
          vc <- 40
          d / dt(depot) <- -ka * depot
          d / dt(central) <- ka * depot - cl / vc * central
          Conc <- central / vc
          alag(depot) <- 1
        })

        s1 <- m258 %>%
          et(dose = 100, time = 0, addl = 6, ii = 24) %>%
          et(0, 250, by = 0.1) %>%
          rxSolve(sensType = sensType)

        s2 <- m258o %>%
          et(dose = 100, time = 0, addl = 6, ii = 24) %>%
          et(0, 250, by = 0.1) %>%
          rxSolve()

        expect_equal(s1$Conc, s2$Conc, tol = tol)
      })


      tol <- 1e-5 ## Current difference for all equations
      type <- 1

      ## forward/central differences don't work here...

      for (type in c(1, 4)) {
        sens <- switch(type,
          "autodiff",
          "forward",
          "central",
          "advan"
        )

        context(sprintf("1 cmt sensitivities (%s)", sens))
        test_that("1 compartment sensitivities; IV bolus, Cl, V", {
          pred <- function() {
            return(Central)
          }

          pk <- function() {
            lCl <- THETA[1]
            lVc <- THETA[2]
            prop.err <- THETA[3]
            eta.Vc <- ETA[1]
            eta.Cl <- ETA[2]
            Vc <- exp(lVc + eta.Vc)
            Cl <- exp(lCl + eta.Cl)
          }

          err <- function() {
            return(prop(prop.err))
          }

          mod <- rxode2({
            Central <- linCmt(Vc, Cl)
          })

          pk1s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

          mod2 <- rxode2({
            Central <- center / Vc
            d / dt(center) <- -Cl * Central
          })

          pk1o <- rxSymPySetupPred(mod2, predfn = pred, pkpars = pk, err = err)

          et <- eventTable() %>%
            add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))


          parms <- c(
            "THETA[1]" = 20, "THETA[2]" = 25, "ETA[1]" = 1, "ETA[2]" = 1,
            "THETA[3]" = 0.2
          )

          s1 <- rxSolve(pk1s$inner, parms, et, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, et, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_)

          ## ETA1, ETA2 is zero for central

          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___)
          expect_equal(s1$rx_r_, o1$rx_r_)
          ## ETA1, ETA2 is zero for central
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___)

          etSs <- et() %>%
            et(amt = 3) %>%
            et(time = 4, amt = 3, ss = 1, ii = 24) %>%
            et(amt = 3, ss = 2, ii = 24, time = 8) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etSs, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___)
          expect_equal(s1$rx_r_, o1$rx_r_)

          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___)

          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___)
          expect_equal(s1$rx_r_, o1$rx_r_)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___)
          expect_equal(s1$rx_r_, o1$rx_r_)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___)
        })

        ## Now oral

        test_that("1 compartment sensitivities; Oral Cl, V, Ka", {
          pred <- function() {
            return(Central)
          }

          pk <- function() {
            lCl <- THETA[1]
            lVc <- THETA[2]
            lKa <- THETA[3]
            prop.err <- THETA[4]
            eta.Vc <- ETA[1]
            eta.Cl <- ETA[2]
            eta.Ka <- ETA[3]
            Vc <- exp(lVc + eta.Vc)
            Cl <- exp(lCl + eta.Cl)
            Ka <- exp(lKa + eta.Ka)
          }

          err <- function() {
            return(prop(prop.err))
          }

          mod <- rxode2({
            Central <- linCmt(Vc, Cl, Ka)
          })

          pk1s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

          mod2 <- rxode2({
            Central <- center / Vc
            d / dt(depot) <- -Ka * depot
            d / dt(center) <- Ka * depot - Cl * Central
          })

          pk1o <- rxSymPySetupPred(mod2, predfn = pred, pkpars = pk, err = err)

          et <- eventTable() %>%
            add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          parms <- c(
            "THETA[1]" = log(20), "THETA[2]" = log(25), "THETA[3]" = log(2),
            "ETA[1]" = 0, "ETA[2]" = 0, "ETA[3]" = 0,
            "THETA[4]" = 0.2
          )

          s1 <- rxSolve(pk1s$inner, parms, et, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, et, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)

          etSs <- et() %>%
            et(amt = 3) %>%
            et(time = 4, amt = 3, ss = 1, ii = 24) %>%
            et(amt = 3, ss = 2, ii = 24, time = 8) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etSs, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etSs, sensType = sens)

          ## library(patchwork)
          ## plot(s1, rx_pred_) / plot(o1, rx_pred_)
          ## plot(s1, rx__sens_rx_pred__BY_ETA_1___) / plot(o1, rx__sens_rx_pred__BY_ETA_1___)
          ## plot(s1, rx__sens_rx_pred__BY_ETA_2___) / plot(o1, rx__sens_rx_pred__BY_ETA_2___)
          ## plot(s1, rx__sens_rx_pred__BY_ETA_3___) / plot(o1, rx__sens_rx_pred__BY_ETA_3___)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 2) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5, cmt = 2) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 2) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 2) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)

          etMix <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
            add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
            add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 1, start.time = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk1s$inner, parms, etMix, sensType = sens)
          o1 <- rxSolve(pk1o$inner, parms, etMix, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
        })


        context(sprintf("2 cmt sensitivities (%s)", sens))
        test_that("2 compartment sensitivities; IV bolus Cl, Vc, Q, Vp", {
          pred <- function() {
            return(Central)
          }

          pk <- function() {
            lCl <- THETA[1]
            lVc <- THETA[2]
            lQ <- THETA[3]
            lVp <- THETA[4]
            prop.err <- THETA[5]
            eta.Vc <- ETA[1]
            eta.Cl <- ETA[2]
            eta.Vp <- ETA[3]
            eta.Q <- ETA[4]
            Vc <- exp(lVc + eta.Vc)
            Cl <- exp(lCl + eta.Cl)
            Vp <- exp(lVp + eta.Vp)
            Q <- exp(lQ + eta.Q)
          }

          pk2 <- function() {
            lK10 <- THETA[1]
            lVc <- THETA[2]
            lK12 <- THETA[3]
            lK21 <- THETA[4]
            prop.err <- THETA[5]
            eta.Vc <- ETA[1]
            eta.K10 <- ETA[2]
            eta.K12 <- ETA[3]
            eta.K21 <- ETA[4]
            Vc <- exp(lVc + eta.Vc)
            k10 <- exp(lK10 + eta.K10)
            k12 <- exp(lK12 + eta.K12)
            k21 <- exp(lK21 + eta.K21)
          }

          err <- function() {
            return(prop(prop.err))
          }

          mod <- rxode2({
            Central <- linCmt(Vc, Cl, Vp, Q)
          })

          modK <- rxode2({
            Central <- linCmt(Vc, k10, k12, k21)
          })

          pk2s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

          pk2sK <- rxSymPySetupPred(modK, predfn = pred, pkpars = pk2, err = err)

          mod2 <- rxode2({
            Central <- centr / Vc
            C3 <- peri / Vp
            d / dt(centr) <- -Cl * Central - Q * Central + Q * C3
            d / dt(peri) <- Q * Central - Q * C3
          })

          modK2 <- rxode2({
            Central <- centr / Vc
            d / dt(centr) <- -k10 * centr - k12 * centr + k21 * peri
            d / dt(peri) <- k12 * centr - k21 * peri
          })

          pk2o <- rxSymPySetupPred(mod2, predfn = pred, pkpars = pk, err = err)
          pk2oK <- rxSymPySetupPred(modK2, predfn = pred, pkpars = pk2, err = err)

          parms2 <- c(
            "THETA[1]" = log(18 / 40), "THETA[2]" = log(40),
            "THETA[3]" = log(10 / 40), "THETA[4]" = log(10 / 297),
            "ETA[1]" = 0, "ETA[2]" = 0,
            "ETA[3]" = 0, "ETA[4]" = 0,
            "THETA[5]" = 0.2
          )

          parms <- c(
            "THETA[1]" = log(18), "THETA[2]" = log(40),
            "THETA[3]" = log(10), "THETA[4]" = log(297),
            "ETA[1]" = 0, "ETA[2]" = 0,
            "ETA[3]" = 0, "ETA[4]" = 0,
            "THETA[5]" = 0.2
          )

          et <- eventTable() %>%
            add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, et, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, et, sensType = sens)

          s1 <- rxSolve(pk2sK$inner, parms2, et, sensType = sens)
          o1 <- rxSolve(pk2oK$inner, parms2, et, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)

          etSs <- et() %>%
            et(amt = 3) %>%
            et(time = 4, amt = 3, ss = 1, ii = 24) %>%
            et(amt = 3, ss = 2, ii = 24, time = 8) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 1) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tol = tol)

          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tol = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tol = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tol = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tol = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
        })


        test_that("2 compartment sensitivities; Oral Cl, Vc, Q, Vp, Ka", {
          pred <- function() {
            return(Central)
          }

          pk <- function() {
            lCl <- THETA[1]
            lVc <- THETA[2]
            lQ <- THETA[3]
            lVp <- THETA[4]
            lKa <- THETA[5]
            prop.err <- THETA[6]
            eta.Vc <- ETA[1]
            eta.Cl <- ETA[2]
            eta.Vp <- ETA[3]
            eta.Q <- ETA[4]
            eta.Ka <- ETA[5]
            Vc <- exp(lVc + eta.Vc)
            Cl <- exp(lCl + eta.Cl)
            Vp <- exp(lVp + eta.Vp)
            Q <- exp(lQ + eta.Q)
            Ka <- exp(lKa + eta.Ka)
          }

          err <- function() {
            return(prop(prop.err))
          }

          mod <- rxode2({
            Central <- linCmt(Vc, Cl, Vp, Q, Ka)
          })

          pk2s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

          mod2 <- rxode2({
            Central <- centr / Vc
            C3 <- peri / Vp
            d / dt(depot) <- -Ka * depot
            d / dt(centr) <- Ka * depot - Cl * Central - Q * Central + Q * C3
            d / dt(peri) <- Q * Central - Q * C3
          })

          pk2o <- rxSymPySetupPred(mod2, predfn = pred, pkpars = pk, err = err)

          parms <- c(
            "THETA[1]" = log(18), ## Cl
            "THETA[2]" = log(40), ## Vc
            "THETA[3]" = log(10), ## Q
            "THETA[4]" = log(297), ## Vp
            "THETA[5]" = log(0.3), ## Ka
            "ETA[1]" = 0, "ETA[2]" = 0,
            "ETA[3]" = 0, "ETA[4]" = 0,
            "ETA[5]" = 0,
            "THETA[6]" = 0.2
          )

          et <- eventTable() %>%
            add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, et, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, et, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)

          etSs <- et() %>%
            et(amt = 3) %>%
            et(time = 4, amt = 3, ss = 1, ii = 24) %>%
            et(amt = 3, ss = 2, ii = 24, time = 8) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 2) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 1) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5, cmt = 2) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 2) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 2) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5, cmt = 1) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 1) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 1) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)

          etMix <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
            add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
            add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 1, start.time = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etMix, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etMix, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
        })

        context(sprintf("3 cmt sensitivities (%s)", sens))

        test_that("3 compartment sensitivities; IV bolus Cl, Vc, Q, Vp", {
          pred <- function() {
            return(Central)
          }

          pk <- function() {
            lCl <- THETA[1]
            lVc <- THETA[2]
            lQ <- THETA[3]
            lVp <- THETA[4]
            lQ2 <- THETA[5]
            lVp2 <- THETA[6]
            prop.err <- THETA[7]
            eta.Vc <- ETA[1]
            eta.Cl <- ETA[2]
            eta.Vp <- ETA[3]
            eta.Q <- ETA[4]
            eta.Q2 <- ETA[5]
            eta.Vp2 <- ETA[6]
            Vc <- exp(lVc + eta.Vc)
            Cl <- exp(lCl + eta.Cl)
            Vp <- exp(lVp + eta.Vp)
            Q <- exp(lQ + eta.Q)
            Q2 <- exp(lQ2 + eta.Q2)
            Vp2 <- exp(lVp2 + eta.Vp2)
          }

          err <- function() {
            return(prop(prop.err))
          }

          mod <- rxode2({
            Central <- linCmt(Vc, Cl, Vp, Q, Vp2, Q2)
          })

          pk3s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

          mod2 <- rxode2({
            Central <- centr / Vc
            C3 <- peri / Vp
            C4 <- peri2 / Vp2
            d / dt(centr) <- -Cl * Central - Q * Central + Q * C3 + Q2 * C4 - Q2 * Central
            d / dt(peri) <- Q * Central - Q * C3
            d / dt(peri2) <- Q2 * Central - Q2 * C4
          })

          pk3o <- rxSymPySetupPred(mod2, predfn = pred, pkpars = pk, err = err)

          parms <- c(
            "THETA[1]" = log(18), "THETA[2]" = log(40),
            "THETA[3]" = log(10), "THETA[4]" = log(297),
            "THETA[5]" = log(7), "THETA[6]" = log(400),
            "ETA[1]" = 0, "ETA[2]" = 0,
            "ETA[3]" = 0, "ETA[4]" = 0,
            "ETA[5]" = 0, "ETA[6]" = 0,
            "THETA[7]" = 0.2
          )

          et <- eventTable() %>%
            add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk3s$inner, parms, et, sensType = sens)
          o1 <- rxSolve(pk3o$inner, parms, et, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)

          etSs <- et() %>%
            et(amt = 3) %>%
            et(time = 4, amt = 3, ss = 1, ii = 24) %>%
            et(amt = 3, ss = 2, ii = 24, time = 8) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk3s$inner, parms, etSs, sensType = sens)
          o1 <- rxSolve(pk3o$inner, parms, etSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 1) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk3s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk3o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5, cmt = 1) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 1) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 1) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk3s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk3o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
        })

        test_that("3 compartment sensitivities; Oral Cl, Vc, Q, Vp, Ka", {
          pred <- function() {
            return(Central)
          }

          pk <- function() {
            lCl <- THETA[1]
            lVc <- THETA[2]
            lQ <- THETA[3]
            lVp <- THETA[4]
            lQ2 <- THETA[5]
            lVp2 <- THETA[6]
            lKa <- THETA[7]
            prop.err <- THETA[8]
            eta.Vc <- ETA[1]
            eta.Cl <- ETA[2]
            eta.Vp <- ETA[3]
            eta.Q <- ETA[4]
            eta.Q2 <- ETA[5]
            eta.Vp2 <- ETA[6]
            eta.Ka <- ETA[7]
            Vc <- exp(lVc + eta.Vc)
            Cl <- exp(lCl + eta.Cl)
            Vp <- exp(lVp + eta.Vp)
            Q <- exp(lQ + eta.Q)
            Q2 <- exp(lQ2 + eta.Q2)
            Vp2 <- exp(lVp2 + eta.Vp2)
            Ka <- exp(lKa + eta.Ka)
          }

          err <- function() {
            return(prop(prop.err))
          }

          mod <- rxode2({
            Central <- linCmt(Vc, Cl, Vp, Q, Vp2, Q2, Ka)
          })

          pk2s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

          mod2 <- rxode2({
            Central <- centr / Vc
            C3 <- peri / Vp
            C4 <- peri2 / Vp2
            d / dt(depot) <- -Ka * depot
            d / dt(centr) <- Ka * depot - Cl * Central - Q * Central + Q * C3 - Q2 * Central + Q2 * C4
            d / dt(peri) <- Q * Central - Q * C3
            d / dt(peri2) <- Q2 * Central - Q2 * C4
          })

          pk2o <- rxSymPySetupPred(mod2, predfn = pred, pkpars = pk, err = err)

          parms <- c(
            "THETA[1]" = log(18), "THETA[2]" = log(40),
            "THETA[3]" = log(10), "THETA[4]" = log(297),
            "THETA[5]" = log(7), "THETA[6]" = log(400),
            "THETA[7]" = log(0.3),
            "ETA[1]" = 0, "ETA[2]" = 0,
            "ETA[3]" = 0, "ETA[4]" = 0,
            "ETA[5]" = 0, "ETA[6]" = 0, "ETA[7]" = 0,
            "THETA[8]" = 0.2
          )

          et <- eventTable() %>%
            add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, et, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, et, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)

          etSs <- et() %>%
            et(amt = 3) %>%
            et(time = 4, amt = 3, ss = 1, ii = 24) %>%
            et(amt = 3, ss = 2, ii = 24, time = 8) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)

          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 2) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)


          etInf <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 6, dosing.interval = 8, cmt = 1) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInf, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInf, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)

          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5, cmt = 2) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 2) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 2) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)


          etInfSs <- et() %>%
            et(amt = 3, rate = 1.5, cmt = 1) %>%
            et(time = 4, amt = 3, rate = 1.5, ss = 1, ii = 24, cmt = 1) %>%
            et(time = 8, amt = 3, rate = 1.5, ss = 2, ii = 24, cmt = 1) %>%
            et(seq(0, 24, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etInfSs, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etInfSs, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)

          etMix <- eventTable() %>%
            add.dosing(dose = 3, rate = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
            add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 2) %>%
            add.dosing(dose = 1.5, nbr.doses = 3, dosing.interval = 16, cmt = 1, start.time = 8) %>%
            add.sampling(seq(0, 48, length.out = 200))

          s1 <- rxSolve(pk2s$inner, parms, etMix, sensType = sens)
          o1 <- rxSolve(pk2o$inner, parms, etMix, sensType = sens)

          expect_equal(s1$rx_pred_, o1$rx_pred_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_1___, o1$rx__sens_rx_pred__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_2___, o1$rx__sens_rx_pred__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_3___, o1$rx__sens_rx_pred__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_4___, o1$rx__sens_rx_pred__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_5___, o1$rx__sens_rx_pred__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_6___, o1$rx__sens_rx_pred__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_pred__BY_ETA_7___, o1$rx__sens_rx_pred__BY_ETA_7___, tolerance = tol)
          expect_equal(s1$rx_r_, o1$rx_r_, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_1___, o1$rx__sens_rx_r__BY_ETA_1___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_2___, o1$rx__sens_rx_r__BY_ETA_2___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_3___, o1$rx__sens_rx_r__BY_ETA_3___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_4___, o1$rx__sens_rx_r__BY_ETA_4___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_5___, o1$rx__sens_rx_r__BY_ETA_5___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_6___, o1$rx__sens_rx_r__BY_ETA_6___, tolerance = tol)
          expect_equal(s1$rx__sens_rx_r__BY_ETA_7___, o1$rx__sens_rx_r__BY_ETA_7___, tolerance = tol)
        })
      }

      context("Make sure central differences are working")
      test_that("central differences", {

        ## One compartment model without oral absorption
        pred <- function() {
          return(Central)
        }

        pk <- function() {
          lCl <- THETA[1]
          lVc <- THETA[2]
          prop.err <- THETA[3]
          lF <- THETA[4]
          lLag <- THETA[5]
          eta.Vc <- ETA[1]
          eta.Cl <- ETA[2]
          eta.f <- ETA[3]
          eta.lag <- ETA[4]
          Vc <- exp(lVc + eta.Vc)
          Cl <- exp(lCl + eta.Cl)
          fc <- exp(lF + eta.f)
          lagc <- exp(lLag + eta.lag)
        }

        err <- function() {
          return(prop(prop.err))
        }

        mod <- rxode2({
          Central <- linCmt(Vc, Cl)
          f(central) <- fc
          alag(central) <- lagc
        })

        parms <- c(
          "THETA[1]" = 20, "THETA[2]" = 25, "ETA[1]" = 1, "ETA[2]" = 1,
          "THETA[3]" = 0.2, "THETA[4]" = log(1), "THETA[5]" = log(0.5),
          "ETA[3]" = 1, "ETA[4]" = 1
        )

        et <- eventTable() %>%
          add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
          add.sampling(seq(0, 48, length.out = 200))

        pk2s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

        s1 <- rxSolve(pk2s$inner, parms, et, sensType = "advan")

        expect_false(any(is.na(s1$rx__sens_rx_pred__BY_ETA_3___)))
        expect_false(any(is.na(s1$rx__sens_rx_pred__BY_ETA_4___)))

        expect_false(any(is.na(s1$rx__sens_rx_r__BY_ETA_3___)))
        expect_false(any(is.na(s1$rx__sens_rx_r__BY_ETA_4___)))

        ## One compartment model with oral absorption

        pred <- function() {
          return(Central)
        }

        pk <- function() {
          lCl <- THETA[1]
          lVc <- THETA[2]
          lQ <- THETA[3]
          lVp <- THETA[4]
          lKa <- THETA[5]
          prop.err <- THETA[6]
          lF <- THETA[7]
          lLag <- THETA[8]
          eta.Vc <- ETA[1]
          eta.Cl <- ETA[2]
          eta.Vp <- ETA[3]
          eta.Q <- ETA[4]
          eta.Ka <- ETA[5]
          eta.F <- ETA[6]
          eta.lag <- ETA[7]
          Vc <- exp(lVc + eta.Vc)
          Cl <- exp(lCl + eta.Cl)
          Vp <- exp(lVp + eta.Vp)
          Q <- exp(lQ + eta.Q)
          Ka <- exp(lKa + eta.Ka)
          F <- exp(lF + eta.F)
          lag <- exp(lLag + eta.lag)
        }

        err <- function() {
          return(prop(prop.err))
        }

        mod <- rxode2({
          Central <- linCmt(Vc, Cl, Vp, Q, Ka)
          f(depot) <- F
          alag(depot) <- lag
        })

        pk2s <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

        et <- eventTable() %>%
          add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
          add.sampling(seq(0, 48, length.out = 200))

        parms <- c(
          "THETA[1]" = log(18), ## Cl
          "THETA[2]" = log(40), ## Vc
          "THETA[3]" = log(10), ## Q
          "THETA[4]" = log(297), ## Vp
          "THETA[5]" = log(0.3), ## Ka
          "ETA[1]" = 0, "ETA[2]" = 0,
          "ETA[3]" = 0, "ETA[4]" = 0,
          "ETA[5]" = 0,
          "THETA[6]" = 0.2, "THETA[7]" = log(1.5), "THETA[8]" = log(0.5),
          "ETA[6]" = 0, "ETA[7]" = 0
        )

        s1 <- rxSolve(pk2s$inner, parms, et, sensType = "advan")

        expect_false(any(is.na(s1$rx__sens_rx_pred__BY_ETA_6___)))
        expect_false(any(is.na(s1$rx__sens_rx_pred__BY_ETA_7___)))

        expect_false(any(is.na(s1$rx__sens_rx_r__BY_ETA_6___)))
        expect_false(any(is.na(s1$rx__sens_rx_r__BY_ETA_7___)))
      })
    }
  },
  silent = TRUE,
  test = "lincmt"
)
