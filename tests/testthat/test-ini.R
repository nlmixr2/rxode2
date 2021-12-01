rxode2Test(
  {
    library(rxode2)
    context("Test Inis")
    m1 <- rxode2({
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    })

    test_that(
      "blank names works",
      expect_equal(
        suppressWarnings(rxInits(m1, c(0, 0, 0, 1), rxState(m1), 0)),
        structure(c(0, 0, 0, 1), .Names = c("depot", "centr", "peri", "eff"))
      )
    )

    out <- rxode2("ini = 1; fun_ini = 2; fun = 4; addit = ini + fun_ini + pi + no_ini")

    test_that("Initial constants are correct", {
      expect_equal(rxInits(out)["pi"], c(pi = pi))
      expect_equal(rxInits(out)["ini"], c(ini = 1))
      expect_equal(rxInits(out)["fun_ini"], c(fun_ini = 2))
      expect_equal(rxInits(out)["fun"], c(fun = 4))
    })

    test_that("Constants are not included in get.modelVars()", {
      expect_equal(out$get.modelVars()$params, "no_ini")
    })

    .rxWithOptions(list(rxode2.syntax.allow.ini = FALSE), {
      out2 <- rxode2("ini = 1; fun_ini = 2; fun = 4; addit = ini + fun_ini + pi + no_ini")

      test_that("Initial constants only include pi.", {
        expect_equal(names(rxInits(out2)), "pi")
        expect_true(rxDll(out) != rxDll(out2))
        expect_equal(out2$get.modelVars()$params, "no_ini")
      })
    })

    .rxWithOptions(list(rxode2.syntax.allow.ini = TRUE), {

      ## out <- rxode2({
      ##     theta[1] = 3
      ##     eta[1] = 2
      ##     k = exp(theta[1] + eta[1])
      ##     d / dt(central) = -theta[1] * central
      ## })

      ## test_that("Allow THETA[#] and ETA[#]s.", {
      ## })

      fini <- rxode2({
        C2 <- centr / V2
        C3 <- peri / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        eff(0) <- theta1 + eta1
      })

      theta <-
        c(
          KA = 2.94E-01, CL = 1.86E+01, V2 = 4.02E+01, # central
          Q = 1.05E+01, V3 = 2.97E+02, # peripheral
          Kin = 1, Kout = 1, EC50 = 200, eta1 = 0, theta1 = 1
        ) # effects

      ev <- eventTable(amount.units = "mg", time.units = "hours")
      ev$add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12)
      ev$add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24)
      ev$add.sampling(0:240)

      s <- fini %>% solve(theta, ev)

      expect_equal(as.data.frame(s)[1, "eff"], 1)

      rxDelete(fini)

      fini <- rxode2({
        C2 <- centr / V2
        C3 <- peri / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        eff(0) <- theta1 + eta1
      })

      theta <-
        c(
          KA = 2.94E-01, CL = 1.86E+01, V2 = 4.02E+01, # central
          Q = 1.05E+01, V3 = 2.97E+02, # peripheral
          Kin = 1, Kout = 1, EC50 = 200, eta1 = 0, theta1 = 1
        ) # effects

      ev <- eventTable(amount.units = "mg", time.units = "hours")
      ev$add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12)
      ev$add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24)
      ev$add.sampling(0:240)

      s <- fini %>% solve(theta, ev)

      expect_equal(as.data.frame(s)[1, "eff"], 1)

      theta["eta1"] <- 0.5

      s <- fini %>% solve(theta, ev)
      expect_equal(as.data.frame(s)[1, "eff"], 1.5)

      theta["eta1"] <- -0.5

      s <- fini %>% solve(theta, ev)
      expect_equal(as.data.frame(s)[1, "eff"], 0.5)



      ## Now with rxGetModel
      m1 <- rxGetModel({
        C2 <- centr / V2
        C3 <- peri / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      })

      test_that(
        "blank names works",
        expect_equal(
          suppressWarnings(rxInits(m1, c(0, 0, 0, 1), rxState(m1), 0)),
          structure(c(0, 0, 0, 1), .Names = c("depot", "centr", "peri", "eff"))
        )
      )
    })

    .rxWithOptions(list(rxode2.syntax.allow.ini = TRUE), {
      out <- rxGetModel("ini = 1; fun_ini = 2; fun = 4; addit = ini + fun_ini + pi + no_ini")

      test_that("Initial constants are correct", {
        expect_equal(rxInits(out)["pi"], c(pi = pi))
        expect_equal(rxInits(out)["ini"], c(ini = 1))
        expect_equal(rxInits(out)["fun_ini"], c(fun_ini = 2))
        expect_equal(rxInits(out)["fun"], c(fun = 4))
      })
    })

    .rxWithOptions(list(rxode2.syntax.allow.ini = FALSE), {
      out2 <- rxGetModel("ini = 1; fun_ini = 2; fun = 4; addit = ini + fun_ini + pi + no_ini")

      test_that("Initial constants only include pi.", {
        expect_equal(names(rxInits(out2)), "pi")
      })

      test_that("Initial conditions are zero length before and after compile", {
        expect_equal(rxModelVars("KA=exp(THETA[1]);\nCL=exp(THETA[2]+ETA[1]);\nV=exp(THETA[3]+ETA[2]);\nd/dt(depot)=-KA*depot;\nd/dt(centr)=KA*depot-CL/V*centr;\nrx_yj_=2;\nrx_lambda_=1;\nrx_pred_f_~centr;\nrx_pred_=centr;\nrx_r_=(THETA[4])^2;\n")$ini, structure(numeric(0), .Names = character(0)))
        tmp <- rxode2("KA=exp(THETA[1]);\nCL=exp(THETA[2]+ETA[1]);\nV=exp(THETA[3]+ETA[2]);\nd/dt(depot)=-KA*depot;\nd/dt(centr)=KA*depot-CL/V*centr;\nrx_yj_=2;\nrx_lambda_=1;\nrx_pred_f_~centr;\nrx_pred_=centr;\nrx_r_=(THETA[4])^2;\n")
        expect_equal(rxModelVars(tmp)$ini, structure(numeric(0), .Names = character(0)))
      })
    })
  },
  silent = TRUE,
  test = "cran"
)
