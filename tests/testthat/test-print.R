rxTest({
  ## To keep basename the same
  .rxWithOptions(list(
    "rxode2.basename.print" = "basename",
    "rxode2.dll.print" = "dll",
    "rxode2.c.print" = "cfile"
  ), {
    ## test_path("test-print.txt")
    .df <- expand.grid(color = c(TRUE, FALSE), unicode = c(TRUE, FALSE))
    lapply(seq_along(.df$color), function(.i) {
      .path <- test_path(sprintf(
        "test-print%s%s.txt", ifelse(.df$color[.i], "-color", ""),
        ifelse(.df$unicode[.i], "-unicode", "")
      ))
      test_that(paste0(.i, ":", .path), {
        verify_output(
          .path,
          {
            mod <- rxode2({
              a <- 6
              b <- 0.6 + a / 100
              kel <- b * 0.01
              V <- 10
              d / dt(intestine) <- -a * intestine
              d / dt(blood) <- a * intestine - b * blood
              l2 <- linCmt()
            })

            print(mod)

            summary(mod)
            str(mod) ## fragile

            print(mod$cmpMgr$rxDll())
            summary(mod$cmpMgr$rxDll())

            coef(mod)

            print(rxModelVars(mod))

            print(rxC(mod))
            summary(rxC(mod)) # too fragile

            print(mod$.rxDll)

            et <- eventTable(time.units = "days")
            et$add.sampling(seq(0, 10, by = 1 / 24))
            et$add.dosing(
              dose = 2 / 24, rate = 2, start.time = 0,
              nbr.doses = 10, dosing.interval = 1
            )

            print(et)
            summary(et)
            str(et)

            et2 <- et %>% et(id = 1:10)
            print(et2)

            tmp <- class(et2)
            et2$matt <- 4
            class(et2) <- tmp
            print(et2)

            print(attr(class(et), ".rxode2.lst"))
            str(attr(class(et), ".rxode2.lst"))

            print(format(structure(0:7, class = "rxEvid")))
            print(structure(0:7, class = "rxEvid"))

            print(format(structure(c(-2, -1, 0, 1, 2), class = "rxRateDur")))
            print(structure(c(-2, -1, 0, 1, 2), class = "rxRateDur"))


            pk <- solve(mod, et)
            print(pk)

            print(pk, width = 40)

            print(pk, bound = "k")

            print(pk, n = 10)

            .rxWithOptions(
              list(rxode2.display.tbl = FALSE),
              print(pk)
            )

            summary(pk)
            str(pk)

            print(summary(pk, bound = "k"))


            modS <- rxode2(mod, calcSens = TRUE)

            print(coef(modS))

            ## Now "destroy" the rxode2 solved object and change the printout
            tmp <- class(pk)
            pk$matt <- 4
            class(tmp) <- tmp
            print(pk)
            class(pk) <- tmp
            summary(pk)

            noOde <- rxode2({
              matt <- a^2 + b^2
            })

            print(coef(noOde))


            rxUnload(mod)
            print(mod)
            print(mod$cmpMgr$rxDll())
            summary(mod)
            str(mod)

            rxDelete(mod)
            print(mod)
            print(mod$cmpMgr$rxDll())
            summary(mod)
            str(mod)

            mod <- rxode2(mod, indLin = TRUE)

            print(mod)
            summary(mod)
            str(mod)

            rxUnload(mod)
            print(mod)
            summary(mod)
            str(mod)

            rxDelete(mod)
            print(mod)
            summary(mod)
            str(mod)

            rxWithSeed(
              42,
              tmp <- matrix(rnorm(5^2), 5, 5)
            )
            mcov <- tcrossprod(tmp, tmp)
            v <- rxSymInvCholCreate(mcov, "sqrt")

            print(v)
            str(v)

            class(v) <- NULL
            v$env$theta <- NULL
            class(v) <- "rxSymInvCholEnv"
            print(v)

            mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")


            et <- eventTable()
            et$add.dosing(
              dose = 2 / 24, rate = 2, start.time = 0,
              nbr.doses = 10, dosing.interval = 1
            )
            et <- et %>%
              et(0.05, evid = 2) %>%
              et(amt = 3, time = 0.5, cmt = out) %>%
              et(amt = 3, time = 0.1, cmt = intestine, ss = 1, ii = 3) %>%
              et(amt = 3, time = 0.3, cmt = intestine, ss = 2, ii = 3) %>%
              et(time = 0.2, cmt = "-intestine") %>%
              as.data.frame()

            ett1 <- etTrans(et, mod, keepDosingOnly = TRUE)

            print(ett1)

            mod2 <- rxode2({
              ## the order of variables do not matter, the type of compartmental
              ## model is determined by the parameters specified.
              CL ~ TCL * exp(eta.Cl)
              C2 ~ linCmt(KA, CL, V2, Q, V3)
              eff(0) <- 1 + eff0^2 ## This specifies that the effect compartment starts at 1.
              d / dt(eff) ~ Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
              ##
              resp <- eff + err1
              pk <- C2 * exp(err2)
            })


            ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
              add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
              add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
              add.sampling(0:240)

            ## Add Residual differences
            sigma <- diag(2) * 0.05
            dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))

            pk3 <- rxSolve(mod2, c(
              KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
              Kin = 1, Kout = 1, EC50 = 200
            ),
            omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
            nSub = 4, ev, sigma = sigma, cores = 2
            )
            print(pk3)
            str(pk3)

            pk3a <- rxSolve(mod2, c(
              KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
              Kin = 1, Kout = 1, EC50 = 200
            ),
            omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
            nSub = 4, ev, sigma = sigma, cores = 2, addDosing = TRUE
            )
            print(pk3a)
            str(pk3a)

            ode <- rxode2({
              b <- -1
              d / dt(X) <- a * X + Y * Z
              d / dt(Y) <- b * (Y - Z)
              d / dt(Z) <- -X * Y + c * Y - Z
            })

            et <- eventTable(time.units = "hr") # default time units
            et$add.sampling(seq(from = 0, to = 100, by = 0.01))

            cov <- data.frame(c = et$get.EventTable()$time + units::set_units(1, h))

            et0 <- et

            et <- cbind(et, cov)

            et$extra <- 1

            out <- rxSolve(ode,
                           params = c(a = -8 / 3, b = -10),
                           events = et,
                           inits = c(X = 1, Y = 1, Z = 1),
                           covsInterpolation = "linear",
                           keep = "extra"
                           )
            print(out)
            str(out)

            mmModel <- rxode2(
            {
              ka <- 1
              Vc <- 1
              Vmax <- 0.00734
              Km <- 0.3672
              Cp <- center / Vc
              d / dt(center) <- -Vmax / (Km + Cp) * Cp + exp(-10 * t)
            },
            indLin = TRUE
            )

            print(mmModel)
            summary(mmModel)

            mmModel <- rxode2(
            {
              ka <- 1
              Vc <- 1
              Vmax <- 0.00734
              Km <- 0.3672
              Cp <- center / Vc
              d / dt(center) <- -Vmax / (Km + Cp) * Cp
            },
            indLin = TRUE
            )

            print(mmModel)
            summary(mmModel)

            mod2 <- rxode2({
              C2 <- centr / V2
              C3 ~ peri / V3
              CL ~ TCL * exp(eta.Cl)
              d / dt(depot) ~ -KA * depot
              d / dt(centr) ~ KA * depot - CL * C2 - Q * C2 + Q * C3
              d / dt(peri) ~ Q * C2 - Q * C3
              d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
              eff(0) <- 1000
              e1 <- err1
              e2 <- err2
              resp <- eff + e1
              pk <- C2 * exp(e2)
            })


            thetaMat <- diag(3) * 0.01
            dimnames(thetaMat) <- list(NULL, c("KA", "TCL", "V2"))
            sigma <- diag(2) * 0.05
            dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))

            ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
              add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
              add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
              add.sampling(0:240)


            pk4 <- rxSolve(mod2, c(
              KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
              Kin = 1, Kout = 1, EC50 = 200
            ),
            omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
            nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 1, method = "lsoda"
            )

            print(pk4)

            pk4 <- rxSolve(mod2, c(
              KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
              Kin = 1, Kout = 1, EC50 = 200
            ),
            omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
            nSub = 4, nStud = 4, dfObs = 4, dfSub = 4,
            thetaMat = thetaMat, sigma = sigma, ev,
            cores = 1, method = "lsoda"
            )

            print(pk4)

            pk4 <- rxSolve(mod2, c(
              KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
              Kin = 1, Kout = 1, EC50 = 200
            ),
            omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
            nSub = 4, nStud = 4,
            sigma = sigma, ev,
            cores = 1, method = "lsoda"
            )

            print(pk4)
          },
crayon = .df$color[.i],
unicode = .df$unicode[.i]
)
      })

      unlink(.path)
    })
  })
})
