rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    test_that("back transformation piping", {

      mod1 <- function() {
        ini({
          # central
          KA <- 2.94E-01
          backTransform("exp")
          CL <- 1.86E+01
          V2 <- 4.02E+01
          # peripheral
          Q <- 1.05E+01
          V3 <- 2.97E+02
          # effects
          Kin <- 1
          Kout <- 1
          EC50 <- 200
        })
        model({
          C2 <- centr/V2
          C3 <- peri/V3
          d/dt(depot) <- -KA*depot
          d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
          d/dt(peri)  <- Q*C2 - Q*C3
          eff(0) <- 1
          d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
        })
      }

      ui <- rxode(mod1)

      expect_equal(ui$iniDf$backTransform[ui$iniDf$name == "KA"], "exp")

      p1 <- ui |>
        ini(
          KA <- backTransform("log")
        )

      expect_equal(p1$iniDf$backTransform[ui$iniDf$name == "KA"], "log")

      p2 <-ui |>
        ini(
          KA <- backTransform(log)
        )

      expect_equal(p2$iniDf$backTransform[ui$iniDf$name == "KA"], "log")

      p3 <- ui |>
        ini(KA <- backTransform(NULL))

      expect_equal(p3$iniDf$backTransform[ui$iniDf$name == "KA"], NA_character_)

      expect_error(ui |>
                     ini(KA <- backTransform(matt)), "matt")

    })

    test_that("piping with ini can update labels (rxode2/issues#351)", {
      mod <- function() {
        ini({
          a <- 1
          label("foo")
          addSd <- 2
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      ui <- rxode2(mod)
      expect_equal(ui$iniDf$label[ui$iniDf$name == "a"], "foo")
      newLabelUi <- ini(ui, a = label("bar"))
      expect_equal(newLabelUi$iniDf$label[newLabelUi$iniDf$name == "a"], "bar")
    })

    test_that("piping with ini can remove labels (#627)", {

      mod <- function() {
        ini({
          a <- 1
          label("foo")
          addSd <- 2
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      ui <- rxode2(mod)
      expect_equal(ui$iniDf$label[ui$iniDf$name == "a"], "foo")
      newLabelUi <- ini(ui, a = label(NULL))
      expect_equal(newLabelUi$iniDf$label[ui$iniDf$name == "a"], NA_character_)
    })

    test_that("piping with ini gives an error pointing the user to use label for character rhs (rxode2/issues#351)", {
      mod <- function() {
        ini({
          a <- 1
          label("foo")
          addSd <- 2
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      ui <- rxode2(mod)
      expect_error(
        ini(ui, a = "bar"),
        regexp = "to assign a new label, use 'a <- label(\"bar\")'",
        fixed = TRUE
      )
    })

    test_that("piping with ini can update labels (rxode2/issues#351)", {
      mod <- function() {
        ini({
          a <- 1
          label("foo")
          addSd <- 2
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      ui <- rxode2(mod)
      expect_equal(ui$iniDf$label[ui$iniDf$name == "a"], "foo")
      newLabelUi <- ini(ui, a = label("bar"))
      expect_equal(newLabelUi$iniDf$label[newLabelUi$iniDf$name == "a"], "bar")
    })

    test_that("piping with ini gives an error pointing the user to use label for character rhs (rxode2/issues#351)", {
      mod <- function() {
        ini({
          a <- 1
          label("foo")
          addSd <- 2
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      ui <- rxode2(mod)
      expect_error(
        ini(ui, a = "bar"),
        regexp = "to assign a new label, use 'a <- label(\"bar\")'",
        fixed = TRUE
      )
    })


    test_that(".iniSimplifyFixUnfix", {
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("fix")),
        as.name("fix")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("fixed")),
        as.name("fix")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("FIX")),
        as.name("fix")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("FIXED")),
        as.name("fix")
      )

      expect_equal(
        .iniSimplifyFixUnfix(str2lang("unfix")),
        as.name("unfix")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("unfixed")),
        as.name("unfix")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("UNFIX")),
        as.name("unfix")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("UNFIXED")),
        as.name("unfix")
      )

      expect_equal(
        .iniSimplifyFixUnfix(str2lang("FIXED(a)")),
        str2lang("fix(a)")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("c <- FIXED(a+b)")),
        str2lang("c <- fix(a + b)")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("c <- UNFIXED(a+b)")),
        str2lang("c <- unfix(a + b)")
      )
      expect_equal(
        .iniSimplifyFixUnfix(str2lang("c <- NULL")),
        str2lang("c <- NULL")
      )
    })

    test_that(".iniSimplifyAssignArrow", {
      expect_equal(
        .iniSimplifyAssignArrow(str2lang("a <- b")),
        str2lang("a <- b")
      )
      expect_equal(
        .iniSimplifyAssignArrow(str2lang("a = b")),
        str2lang("a <- b")
      )
      # non-assignment equal signs are not modified
      expect_equal(
        .iniSimplifyAssignArrow(str2lang("a = b(c=d)")),
        str2lang("a <- b(c=d)")
      )
    })

    test_that("piping with ini can update reorder parameters (rxode2/issues#352)", {

      mod <- function() {
        ini({
          a <- 1
          b <- 2
          c <- 3
          addSd <- 2
        })
        model({
          b <- a + b*log(c)
          b ~ add(addSd)
        })
      }

      ui <- rxode2(mod)

      # No modification
      expect_equal(ui$iniDf$name, c("a", "b", "c", "addSd"))
      # b to the top by number
      expect_equal(suppressMessages(ini(ui, b <- 1, append = 0))$iniDf$name, c("b", "a", "c", "addSd"))
      # b to the top by logical
      expect_equal(suppressMessages(ini(ui, b <- 1, append = FALSE))$iniDf$name, c("b", "a", "c", "addSd"))
      # b to the bottom by number
      expect_equal(suppressMessages(ini(ui, b <- 1, append = Inf))$iniDf$name, c("a", "c", "addSd", "b"))
      # b to the bottom by logical
      expect_equal(suppressMessages(ini(ui, b <- 1, append = TRUE))$iniDf$name, c("a", "c", "addSd", "b"))
      # b to the bottom by name
      expect_equal(suppressMessages(ini(ui, b <- 1, append = "addSd"))$iniDf$name, c("a", "c", "addSd", "b"))

      expect_equal(suppressMessages(ini(ui, b <- 1, append = addSd))$iniDf$name, c("a", "c", "addSd", "b"))

      # b after c
      expect_equal(suppressMessages(ini(ui, b <- 1, append = "c"))$iniDf$name, c("a", "c", "b", "addSd"))
      # a and b after c; counter-intuitive: the order of a and b are reversed
      expect_equal(suppressMessages(ini(ui, a <- 1, b <- 1, append = "c"))$iniDf$name, c("c", "b", "a", "addSd"))
      # b to b, warn and no change
      expect_warning(
        expect_equal(suppressMessages(ini(ui, b <- 1, append = "b"))$iniDf$name, c("a", "b", "c", "addSd")),
        regexp = "parameter 'b' set to be moved after itself, no change in order made"
      )

      expect_error(
        ini(ui, b <- 1, append = d/dt(fun)),
        "append")

      # Invalid parameter is correctly caught
      expect_error(
        ini(ui, b <- 1, append = "foo"),
        "append"
      )

    })

    test_that(".iniAddCovarianceBetweenTwoEtaValues", {
      # Promote a covariate to a correlated eta

      mod <- function() {
        ini({
          a <- 1
          b <- 2
          c <- 3
          d ~ 1
          h ~ 2
          addSd <- 2
        })
        model({
          b <- a + b*log(c)
          f <- a + d + e
          i <- j + h
          b ~ add(addSd)
        })
      }

      suppressMessages(
        expect_message(
          ini(mod, d + e ~ c(1, 0.5, 3)),
          regexp = "promote `e` to between subject variability"
        )
      )

      suppressMessages(
        expect_message(
          ini(mod, d ~ 1,  e ~ c(0.5, 3)),
          regexp = "promote `e` to between subject variability"
        )
      )

      suppressMessages(
        expect_message(
          ini(mod, {
            d ~ 1
            e ~ c(0.5, 3)})
        ))

      # Non-existent correlated eta
      suppressMessages(
        expect_error(
          ini(mod, d + g ~ c(1, 0.5, 3)),
          regexp = "cannot find parameter 'g'"
        )
      )

      suppressMessages(
        expect_error(
          ini(mod, d ~ 1, g ~ c(0.5, 3)),
          regexp = "cannot find parameter 'g'"
        )
      )


      # Update eta order
      suppressMessages(
        expect_equal(
          ini(mod, h + d ~ c(1, 0.5, 3))$iniDf$name,
          c("a", "b", "c", "addSd", "h", "d", "(h,d)")
        )
      )

      suppressMessages(
        expect_equal(
          ini(mod, h ~ 1,  d ~ c(0.5, 3))$iniDf$name,
          c("a", "b", "c", "addSd", "h", "d", "(h,d)")
        )
      )

    })

    test_that(".iniHandleLabel", {
      mod <- function() {
        ini({
          a <- 1
          b <- 2
          c <- 3
          d ~ 1
          h ~ 2
          addSd <- 2
        })
        model({
          b <- a + b*log(c)
          f <- a + d + e
          i <- j + h
          b ~ add(addSd)
        })
      }

      # non-existent parameter
      expect_error(
        ini(mod, q = label("foo")),
        regexp = "cannot find parameter 'q'"
      )
      # invalid label value
      expect_error(
        ini(mod, a = label(5)),
        regexp = "the new label for 'a' must be a character string"
      )
    })

    test_that(".iniHandleAppend", {
      mod <- function() {
        ini({
          a <- 1
          b <- 2
          c <- 3
          d ~ 1
          h ~ 2
          addSd <- 2
        })
        model({
          b <- a + b*log(c)
          f <- a + d + e
          i <- j + h
          b ~ add(addSd)
        })
      }

      expect_error(
        ini(mod, a <- 1, append=factor("A")),
        regexp = "'append' must be NULL, logical, numeric, or character/expression of variable in model"
      )
      expect_error(
        ini(mod, q <- 1, append=0),
        regexp = "cannot find parameter 'q'"
      )
      # Non-theta parameters cannot be moved
      expect_error(
        ini(mod, h ~ 1, append=0),
        regexp = "only theta parameters can be moved"
      )
    })

    test_that("ini tests for different types of expressions", {

      mod <- function() {
        ini({
          a <- 1
          b <- 2
          c <- 3
          d ~ 1
          h ~ 2
          addSd <- 2
        })
        model({
          b <- a + b*log(c)
          f <- a + d + e
          i <- j + h
          b ~ add(addSd)
        })
      }

      expect_error(mod |> ini("h~3"), NA)

      expect_error(mod |> ini("h~3;4*"))

      expect_error(mod |> ini(factor("A")))

    })

    test_that("zeroRe", {
      modOmegaSigma <- function() {
        ini({
          a <- 1; label("foo") #nolint
          iiva ~ 3
          addSd <- 2
        })
        model({
          b <- a + iiva
          b ~ add(addSd)
        })
      }
      modOmega <- function() {
        ini({
          a <- 1; label("foo") # nolint
          iiva ~ 3
        })
        model({
          b <- a + iiva
        })
      }
      modSigma <- function() {
        ini({
          a <- 1; label("foo") # nolint
          addSd <- 2
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      modSigmaBound <- function() {
        ini({
          a <- 1; label("foo") # nolint
          addSd <- c(1, 2)
        })
        model({
          b <- a
          b ~ add(addSd)
        })
      }
      modNone <- function() {
        ini({
          a <- 1; label("foo") # nolint
        })
        model({
          b <- a
        })
      }
      uiOmegaSigma <- rxode2(modOmegaSigma)
      uiOmega <- rxode2(modOmega)
      uiSigma <- rxode2(modSigma)
      uiSigmaBound <- rxode2(modSigmaBound)
      uiNone <- rxode2(modNone)

      expect_silent(
        suppressMessages(
          newMod <- zeroRe(modOmegaSigma, which = c("omega", "sigma"))
        )
      )
      expect_silent(
        suppressMessages(
          newUi <- zeroRe(uiOmegaSigma, which = c("omega", "sigma"))
        )
      )
      expect_equal(newMod$iniDf, newUi$iniDf)
      # detect change
      expect_equal(uiOmegaSigma$iniDf$est, c(1, 2, 3))
      expect_equal(newMod$iniDf$est, c(1, 0, 0))

      # Confirm that you can simulate from the model
      suppressMessages(
        expect_equal(
          rxSolve(newMod, events = data.frame(TIME = 0:2))$b,
          rep(1, 3)
        )
      )

      # Confirm that the `fix` flag is respected
      expect_silent(
        suppressMessages(
          newUiNoFix <- zeroRe(uiOmegaSigma, which = c("omega", "sigma"), fix = FALSE)
        )
      )
      # detect change
      expect_equal(uiOmegaSigma$iniDf$fix, rep(FALSE, 3))
      expect_equal(newUi$iniDf$fix, c(FALSE, TRUE, TRUE))
      expect_equal(newUiNoFix$iniDf$fix, rep(FALSE, 3))

      suppressMessages(
        expect_warning(
          newMod <- zeroRe(modOmega, which = c("omega", "sigma")),
          regexp = "No sigma parameters in the model"
        )
      )
      suppressMessages(
        expect_warning(
          newUi <- zeroRe(uiOmega, which = c("omega", "sigma")),
          regexp = "No sigma parameters in the model"
        )
      )
      expect_equal(newMod$iniDf, newUi$iniDf)
      # detect change
      expect_equal(uiOmega$iniDf$est, c(1, 3))
      expect_equal(newMod$iniDf$est, c(1, 0))

      suppressMessages(
        expect_warning(
          newMod <- zeroRe(modSigmaBound, which = c("omega", "sigma")),
          regexp = "No omega parameters in the model"
        )
      )
      suppressMessages(
        expect_warning(
          newUi <- zeroRe(uiSigmaBound, which = c("omega", "sigma")),
          regexp = "No omega parameters in the model"
        )
      )
      expect_equal(newMod$iniDf, newUi$iniDf)
      # detect change
      expect_equal(uiSigmaBound$iniDf$est, c(1, 2))
      expect_equal(newMod$iniDf$est, c(1, 0))
      # confirm lower bound change
      expect_equal(uiSigmaBound$iniDf$lower, c(-Inf, 1))
      expect_equal(newMod$iniDf$lower, c(-Inf, 0))

      suppressMessages(
        expect_warning(
          newMod <- zeroRe(modSigma, which = c("omega", "sigma")),
          regexp = "No omega parameters in the model"
        )
      )
      suppressMessages(
        expect_warning(
          newUi <- zeroRe(uiSigma, which = c("omega", "sigma")),
          regexp = "No omega parameters in the model"
        )
      )
      expect_equal(newMod$iniDf, newUi$iniDf)
      # detect change
      expect_equal(uiSigma$iniDf$est, c(1, 2))
      expect_equal(newMod$iniDf$est, c(1, 0))

      suppressMessages(
        expect_warning(expect_warning(
          newMod <- zeroRe(modNone, which = c("omega", "sigma")),
          regexp = "No omega parameters in the model"),
          regexp = "No sigma parameters in the model"
          )
      )
      suppressMessages(
        expect_warning(expect_warning(
          newUi <- zeroRe(uiNone, which = c("omega", "sigma")),
          regexp = "No omega parameters in the model"),
          regexp = "No sigma parameters in the model"
          )
      )
      expect_equal(newMod$iniDf, newUi$iniDf)
      # detect no change
      expect_equal(uiNone$iniDf$est, 1)
      expect_equal(newMod$iniDf$est, 1)

      # expected errors
      expect_error(zeroRe("A"), regexp = "'object' needs to be a rxUi model")
      expect_error(zeroRe(modOmegaSigma, which = "foo"), regexp = "should be one of")
    })

    test_that("zeroRe works with correlated etas (#480)", {
      mod <- function() {
        ini({
          lka <- 0.45
          lcl <- 1
          lvc <- 3.45
          propSd <- c(0, 0.5)
          etalka + etalcl + etalvc ~ c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
        })
        model({
          ka <- exp(lka + etalka)
          cl <- exp(lcl + etalcl)
          vc <- exp(lvc + etalvc)
          cp <- linCmt()
          cp ~ prop(propSd)
        })
      }
      ui <- rxode2(mod)
      expect_equal(ui$iniDf$est[!is.na(ui$iniDf$neta1)], (1:6)/10)
      suppressMessages(zeroUi <- zeroRe(mod))
      expect_equal(zeroUi$iniDf$est[!is.na(zeroUi$iniDf$neta1)], c(0, 0, 0))
    })

    test_that("Piping outside the boundaries", {

      m1 <- function() {
        ini({
          x2 <- c(0, 1)
          x3 <- c(0, 1, 2)
        })
        model({
          f <- x2+x3*4
        })
      }

      suppressMessages({
        f2 <- m1 |> ini(x2=-1)
        expect_equal(f2$iniDf[f2$iniDf$name == "x2","lower"], -Inf)
      })
      suppressMessages({
        f2 <- m1 |> ini(x3=4)
        expect_equal(f2$iniDf[f2$iniDf$name == "x3","upper"], Inf)
      })
      suppressMessages({
        f2 <- m1 |> ini(x3=c(0,3))
        expect_equal(f2$iniDf[f2$iniDf$name == "x3","upper"], Inf)
      })
    })

    test_that("append allows promoting from covariate (#472)", {
      mod <- function() {
        ini({
          lka <- 0.45
          lcl <- 1
          lvc  <- 3.45
          propSd <- 0.5
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)

          kel <- cl / vc

          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central

          cp <- central / vc
          cp ~ prop(propSd)
        })
      }
      suppressMessages(
        newmod <-
          mod |>
          model(
            ka <- exp(lka + ka_dose*DOSE),
            auto = FALSE
          ) |>
          ini(
            ka_dose <- 1,
            append = "lka"
          )
      )
      expect_equal(newmod$iniDf$name, c("lka", "ka_dose", "lcl", "lvc", "propSd"))
    })

    test_that("change ini type with ~", {

      mod <- function() {
        ini({
          lka <- 0.45
          lcl <- 1
          lvc  <- 3.45
          propSd <- 0.5
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)
          kel <- cl / vc
          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central
          cp <- central / vc
          cp ~ prop(propSd)
        })
      }

      mod1 <- mod |> ini( ~ lka)
      expect_equal(mod1$omega, lotri(lka ~ 0.45))

      mod2 <- mod1 |> ini( ~ lka)
      expect_equal(mod2$omega, NULL)

      expect_error(mod1 |> ini( ~ propSd))

      expect_error(mod1 |> ini( ~ matt))

      ## all etas

      mod <- function() {
        ini({
          lka ~ 0.45
          lcl ~ 1
          lvc ~ 3.45
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)
          kel <- cl / vc
          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central
          cp <- central / vc
        })
      }

      mod2 <- mod |> ini( ~ lka)

      expect_equal(mod2$omega, lotri(lcl ~ 1, lvc ~ 3.45))

      # remove correlated eta

      mod <- function() {
        ini({
          lka + lcl + lvc ~
            c(0.45,
              0.01, 1,
              0.01, -0.01, 3.45)
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)
          kel <- cl / vc
          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central
          cp <- central / vc
        })
      }

      mod2 <- mod |> ini( ~ lka)

      expect_equal(mod2$omega, lotri(lcl + lvc ~ c(1,
                                                   -0.01, 3.45)))


      # negative and zero

      mod <- function() {
        ini({
          lka <- 0.45
          lcl <- -1
          lvc <- 0
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)
          kel <- cl / vc
          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central
          cp <- central / vc
        })
      }

      mod2 <- mod |> ini( ~ lcl)

      expect_equal(mod2$omega, lotri(lcl ~ 1))

      mod2 <- mod |> ini( ~ lvc)

      expect_equal(mod2$omega, lotri(lvc ~ 1))

      mod3 <- mod2 |> ini( ~ lvc)

      expect_equal(mod3$omega, NULL)

      mod4 <- mod3 |> ini( ~ lvc)

      expect_equal(mod4$omega, lotri(lvc ~ 1))

    })



    test_that("change ini variable to covariate with -", {

      mod <- function() {
        ini({
          lka + lcl + lvc ~
            c(0.45,
              0.01, 1,
              0.01, -0.01, 3.45)
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)
          kel <- cl / vc
          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central
          cp <- central / vc
        })
      }

      mod2 <- mod |> ini(-lka)

      expect_equal(mod2$allCovs, "lka")
      expect_equal(mod2$omega, lotri(lcl + lvc ~ c(1, -0.01, 3.45)))

      mod <- function() {
        ini({
          lka ~ 0.45
          lcl ~ 1
          lvc ~ 3.45
        })
        model({
          ka <- exp(lka)
          cl <- exp(lcl)
          vc  <- exp(lvc)
          kel <- cl / vc
          d/dt(depot) <- -ka*depot
          d/dt(central) <- ka*depot-kel*central
          cp <- central / vc
        })
      }

      mod2 <- mod |> ini(-lka)

      expect_equal(mod2$allCovs, "lka")


    })
  }

  test_that("empty arguments to rxRename() give a warning (#688)", {
    mod1 <- function() {
      ini({
        Kin=1
      })
      model({
        eff <- Kin
      })
    }

    expect_warning(
      rxRename(mod1, ),
      "empty argument ignored"
    )
    expect_warning(
      rxRename(mod1, foo = eff, ),
      "empty argument ignored"
    )
  })

  test_that("parameters can be promoted from covariate to parameter with bounds (#692)", {
    mod1 <- function() {
      model({
        eff <- Kin
      })
    }

    expect_message(
      mod1 |> ini(Kin = 2),
      "promote `Kin` to population parameter with initial estimate 2"
    )
    expect_message(
      expect_message(
        mod1 |> ini(Kin = c(1, 2)),
        "promote `Kin` to population parameter with initial estimate 2"
      ),
      regexp = "change initial estimate (2) and lower bound (1) of `Kin`",
      fixed = TRUE
    )
    expect_message(
      expect_message(
        mod1 |> ini(Kin = c(1, 2, 3)),
        "promote `Kin` to population parameter with initial estimate 2"
      ),
      regexp = "change initial estimate (2) and upper/lower bound (1 to 3) of `Kin`",
      fixed = TRUE
    )
  })

  test_that("ini(diag) and ini(-cov()) tests", {

    mod2 <- function() {
      ini({
        lka ~ 0.45
        lcl ~ c(0.01, 1)
        lvc ~ c(-0.01, 0.01, 3.45)
        lfun ~ c(-0.1, 0.1, 0.01, 4)
      })
      model({
        ka <- exp(lka)
        cl <- exp(lcl)
        vc  <- exp(lvc)
        kel <- cl / vc
        d/dt(depot) <- -ka*depot
        d/dt(central) <- ka*depot-kel*central
        cp <- central / vc + lfun
      })
    }

    expect_error(
      mod2 |> ini(diag(lcl, matt)),
      "matt"
    )

    expect_error(
      mod2 |> ini(diag(matt, lcl)),
      "matt"
    )

    tmp <- mod2 |> ini(-cov(lcl, lvc))
    expect_equal(tmp$omega,
                 lotri({
                   lvc ~ 3.45
                   lfun ~ c(0.01, 4)
                   lka ~ c(-0.01, -0.1, 0.45)
                   lcl ~ c(0, 0.1, 0.01, 1)
                 }))

    tmp <- mod2 |> ini(-cor(lcl, lvc))
    expect_equal(tmp$omega,
                 lotri({
                   lvc ~ 3.45
                   lfun ~ c(0.01, 4)
                   lka ~ c(-0.01, -0.1, 0.45)
                   lcl ~ c(0, 0.1, 0.01, 1)
                 }))

    tmp <- mod2 |> ini(cor(lcl, lvc) <- NULL)

    expect_equal(tmp$omega,
                 lotri({
                   lvc ~ 3.45
                   lfun ~ c(0.01, 4)
                   lka ~ c(-0.01, -0.1, 0.45)
                   lcl ~ c(0, 0.1, 0.01, 1)
                 }))

    tmp <- mod2 |> ini(cor(lcl, lvc) ~ NULL)
    expect_equal(tmp$omega,
                 lotri({
                   lvc ~ 3.45
                   lfun ~ c(0.01, 4)
                   lka ~ c(-0.01, -0.1, 0.45)
                   lcl ~ c(0, 0.1, 0.01, 1)
                 }))

    expect_error(mod2 |> ini(diag(matt)),
                 "matt")

    # Will reorder
    tmp <- mod2 |> ini(diag(lcl, lvc))
    expect_equal(tmp$omega,
                 lotri({
                   lfun ~ 4
                   lka ~ c(-0.1, 0.45)
                   lvc ~ 3.45
                   lcl ~ 1
                 }))

    tmp <- mod2 |> ini(diag)
    expect_equal(tmp$omega,
                 lotri({
                   lka ~ 0.45
                   lcl ~ 1
                   lvc ~ 3.45
                   lfun ~ 4
                 }))

    tmp <- mod2 |> ini(diag(lvc))

    expect_equal(tmp$omega,
                 lotri({
                   lfun ~ 4
                   lcl ~ c(0.1, 1)
                   lka ~ c(-0.1, 0.01, 0.45)
                   lvc ~ 3.45
                 }))

    mod <- function() {
      ini({
        lka ~ 0.45
        lcl ~ c(0.01, 1)
        lvc ~ c(-0.01, 0.01, 3.45)
      })
      model({
        ka <- exp(lka)
        cl <- exp(lcl)
        vc  <- exp(lvc)
        kel <- cl / vc
        d/dt(depot) <- -ka*depot
        d/dt(central) <- ka*depot-kel*central
        cp <- central / vc
      })
    }


    tmp <- mod |> ini(diag)

    expect_equal(tmp$omega,
                 lotri({
                   lka ~ 0.45
                   lcl ~ 1
                   lvc ~ 3.45
                 }))

    tmp <- mod |> ini(diag())

    expect_equal(tmp$omega,
                 lotri({
                   lka ~ 0.45
                   lcl ~ 1
                   lvc ~ 3.45
                 }))

  })

  test_that("piping a ui's ini() keeps an eta when only one is shared", {
    # a single surviving eta is subset out of the omega with [.w, .w]; without
    # drop=FALSE that is a bare scalar with no dimnames, and the eta was
    # silently dropped from the piped ini() -- the destination kept its own
    # estimate with no error and no message
    .from <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl * central
        cp <- central
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ 0.1
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - central
        cp <- central
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .toUi <- rxode2(.to)
    # eta.ka is the only random effect the two models share
    .piped <- .toUi |> ini(.fromUi)
    .iniDf <- as.data.frame(.piped$iniDf)
    .etaKa <- .iniDf[.iniDf$name == "eta.ka", ]
    expect_equal(nrow(.etaKa), 1L)
    expect_equal(.etaKa$est, 0.6)
    expect_equal(.piped$omega, lotri::lotri(eta.ka ~ 0.6))
    # the theta it shares comes across too, and eta.cl does not
    expect_equal(.iniDf$est[.iniDf$name == "tka"], 0.45)
    expect_false("eta.cl" %in% .iniDf$name)
  })

  test_that("piping a ui's ini() keeps a shared eta fixed", {
    # subsetting the omega drops the logical matrix marking the fixed entries,
    # and the lotri round-trip in .iniHandleLine() then drops the fix() the
    # piped line carried -- either one silently unfixes the eta
    .from <- function() {
      ini({
        tka <- 0.45
        eta.ka ~ fix(0.6)
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(1 + eta.cl)
        d/dt(depot) <- -ka * depot
        cp <- depot * cl
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ 0.1
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .piped <- rxode2(.to) |> ini(.fromUi)
    .iniDf <- as.data.frame(.piped$iniDf)
    expect_equal(.iniDf$est[.iniDf$name == "eta.ka"], 0.6)
    expect_true(.iniDf$fix[.iniDf$name == "eta.ka"])
  })

  test_that("piping a ui's ini() keeps etas at more than one level", {
    # random effects at several levels come back from lotri::as.lotri() as a
    # list of blocks with no dimnames of their own, and every eta was then
    # silently left out of the piped ini() with no error and no message
    .from <- function() {
      ini({
        tka <- 0.45
        eta.ka ~ 0.6
        eta.occ ~ 0.2 | occ
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka + eta.occ)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ 0.1
        eta.occ ~ 0.05 | occ
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka + eta.occ)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .piped <- rxode2(.to) |> ini(.fromUi)
    .iniDf <- as.data.frame(.piped$iniDf)
    expect_equal(.iniDf$est[.iniDf$name == "eta.ka"], 0.6)
    expect_equal(.iniDf$est[.iniDf$name == "eta.occ"], 0.2)
    expect_equal(.iniDf$condition[.iniDf$name == "eta.occ"], "occ")
  })

  test_that("piping a ui's ini() drops the covariance of an unshared eta", {
    # the surviving eta comes out of a correlated block, so its covariance with
    # the eta the destination does not have has to go
    .from <- function() {
      ini({
        tka <- 0.45
        eta.ka + eta.cl ~ c(0.6, 0.01, 0.3)
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(1 + eta.cl)
        d/dt(depot) <- -ka * depot
        cp <- depot * cl
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ 0.1
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .piped <- rxode2(.to) |> ini(.fromUi)
    .iniDf <- as.data.frame(.piped$iniDf)
    expect_equal(.iniDf$est[.iniDf$name == "eta.ka"], 0.6)
    expect_false("eta.cl" %in% .iniDf$name)
    expect_equal(.piped$omega, lotri::lotri(eta.ka ~ 0.6))
  })

  test_that("ini() piping of a single eta honors fix(), unfix() and a condition", {
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ fix(0.1)
        eta.occ ~ 0.05 | occ
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka + eta.occ)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .toUi <- rxode2(.to)
    # the lotri round-trip used to drop `unfix()` entirely
    .unfixed <- .toUi |> ini(eta.ka ~ unfix(0.7))
    expect_equal(.unfixed$iniDf$est[.unfixed$iniDf$name == "eta.ka"], 0.7)
    expect_false(.unfixed$iniDf$fix[.unfixed$iniDf$name == "eta.ka"])
    # a `| condition` eta came back from lotri as a list of blocks, which the
    # `[1, 1]` below it could not subset
    .cond <- .toUi |> ini(eta.occ ~ 0.2 | occ)
    expect_equal(.cond$iniDf$est[.cond$iniDf$name == "eta.occ"], 0.2)
    expect_equal(.cond$iniDf$condition[.cond$iniDf$name == "eta.occ"], "occ")
    .condFix <- .toUi |> ini(eta.occ ~ fix(0.3) | occ)
    expect_equal(.condFix$iniDf$est[.condFix$iniDf$name == "eta.occ"], 0.3)
    expect_true(.condFix$iniDf$fix[.condFix$iniDf$name == "eta.occ"])
  })

  test_that("a piped correlated block keeps its covariance at its own level", {
    # the covariance row was written with a hard-coded condition of "id", which
    # put a `| occ` block's covariance in the wrong omega
    .from <- function() {
      ini({
        tka <- 0.45
        eta.a + eta.b ~ c(0.6, 0.01, 0.3) | occ
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.a + eta.b)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        eta.a + eta.b ~ c(0.1, 0.001, 0.05) | occ
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.a + eta.b)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .toUi <- rxode2(.to)
    .expected <- lotri::lotri(eta.a + eta.b ~ c(0.6, 0.01, 0.3) | occ)
    .piped <- .toUi |> ini(.fromUi)
    .iniDf <- as.data.frame(.piped$iniDf)
    expect_equal(.iniDf$condition[.iniDf$name == "(eta.a,eta.b)"], "occ")
    expect_equal(.piped$omega, .expected$occ)
    # the same block piped directly used to stop with `argument is of length zero`
    .direct <- .toUi |> ini(eta.a + eta.b ~ c(0.6, 0.01, 0.3) | occ)
    expect_equal(.direct$omega, .expected$occ)
  })

  test_that("ini() piping honors unfix() under a condition", {
    # the `| condition` wraps the estimate, so a check on the top of the right
    # hand side does not see the unfix() underneath it and the lotri round-trip
    # silently drops it, leaving the eta fixed
    .to <- function() {
      ini({
        tka <- 0.1
        eta.occ ~ fix(0.05) | occ
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.occ)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .toUi <- rxode2(.to)
    .unfixed <- .toUi |> ini(eta.occ ~ unfix(0.2) | occ)
    expect_equal(.unfixed$iniDf$est[.unfixed$iniDf$name == "eta.occ"], 0.2)
    expect_false(.unfixed$iniDf$fix[.unfixed$iniDf$name == "eta.occ"])
  })

  test_that("ini() piping says so when a condition does not match the eta level", {
    # piping an estimate does not restructure the model, so the level the eta
    # already sits at is kept -- but silently discarding the piped condition
    # would leave no way to tell
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ 0.1
        eta.occ ~ 0.05 | occ
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka + eta.occ)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .toUi <- rxode2(.to)
    expect_message(.moved <- .toUi |> ini(eta.ka ~ 0.5 | occ),
                   "keeping .*eta.ka.* at level")
    expect_equal(.moved$iniDf$est[.moved$iniDf$name == "eta.ka"], 0.5)
    expect_equal(.moved$iniDf$condition[.moved$iniDf$name == "eta.ka"], "id")
    # a matching condition says nothing
    .msgs <- testthat::capture_messages(.same <- .toUi |> ini(eta.occ ~ 0.3 | occ))
    expect_false(any(grepl("keeping", .msgs)))
    expect_equal(.same$iniDf$est[.same$iniDf$name == "eta.occ"], 0.3)
  })

  test_that("piping a ui's ini() keeps the covariance of two shared etas", {
    # the shared etas are not next to each other in the source block, so the
    # covariance that survives is the one across the eta the destination does
    # not have
    .from <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        eta.a + eta.b + eta.c ~ c(0.6,
                                  0.05, 0.3,
                                  0.02, 0.01, 0.2)
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.a)
        cl <- exp(tcl + eta.b)
        v <- exp(1 + eta.c)
        d/dt(depot) <- -ka * depot
        cp <- depot * cl / v
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        tcl <- 1
        eta.a + eta.c ~ c(0.1, 0.001, 0.05)
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.a)
        v <- exp(tcl + eta.c)
        d/dt(depot) <- -ka * depot
        cp <- depot / v
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .piped <- rxode2(.to) |> ini(.fromUi)
    .iniDf <- as.data.frame(.piped$iniDf)
    expect_false("eta.b" %in% .iniDf$name)
    expect_equal(.iniDf$est[.iniDf$name == "(eta.a,eta.c)"], 0.02)
    expect_equal(.piped$omega, lotri::lotri(eta.a + eta.c ~ c(0.6, 0.02, 0.2)))
  })

  test_that("piping a ui's ini() carries an eta's label like a theta's", {
    # subsetting the omega drops the labels alongside the estimates, so a theta
    # kept its piped label while an eta silently lost it
    .from <- function() {
      ini({
        tka <- 0.45
        label("ka pop")
        eta.ka ~ 0.6
        label("ka BSV")
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(1 + eta.cl)
        d/dt(depot) <- -ka * depot
        cp <- depot * cl
        cp ~ add(add.sd)
      })
    }
    .to <- function() {
      ini({
        tka <- 0.1
        eta.ka ~ 0.1
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.ka)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .fromUi <- rxode2(.from)
    .toUi <- rxode2(.to)
    withr::with_options(list(rxode2.ignoreLabels=FALSE), {
      .piped <- .toUi |> ini(.fromUi)
    })
    .iniDf <- as.data.frame(.piped$iniDf)
    expect_equal(.iniDf$label[.iniDf$name == "tka"], "ka pop")
    expect_equal(.iniDf$label[.iniDf$name == "eta.ka"], "ka BSV")
  })

  test_that("ini() piping does not build a covariance across two levels", {
    # the etas keep their own levels, so a covariance bridging them cannot be
    # assembled into an omega at all
    .to <- function() {
      ini({
        tka <- 0.1
        eta.a ~ 0.1
        eta.b ~ 0.2 | occ
        add.sd <- 0.2
      })
      model({
        ka <- exp(tka + eta.a + eta.b)
        d/dt(depot) <- -ka * depot
        cp <- depot
        cp ~ add(add.sd)
      })
    }
    .toUi <- rxode2(.to)
    expect_message(.piped <- .toUi |> ini(eta.a + eta.b ~ c(0.1, 0.01, 0.2) | occ),
                   "not adding a covariance")
    expect_false("(eta.a,eta.b)" %in% .piped$iniDf$name)
    # the omega assembles, which it cannot do with a cross level covariance
    .omega <- .piped$omega
    expect_equal(.omega$id, lotri::lotri(eta.a ~ 0.1))
    expect_equal(.omega$occ, lotri::lotri(eta.b ~ 0.2))
  })

})
