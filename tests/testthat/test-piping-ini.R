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

    p1 <- ui %>%
      ini(
        KA <- backTransform("log")
      )

    expect_equal(p1$iniDf$backTransform[ui$iniDf$name == "KA"], "log")

    p2 <-ui %>%
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

    expect_error(mod %>% ini("h~3"), NA)

    expect_error(mod %>% ini("h~3;4*"))

    expect_error(mod %>% ini(factor("A")))

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
      f2 <- m1 %>% ini(x2=-1)
      expect_equal(f2$iniDf[f2$iniDf$name == "x2","lower"], -Inf)
    })
    suppressMessages({
      f2 <- m1 %>% ini(x3=4)
      expect_equal(f2$iniDf[f2$iniDf$name == "x3","upper"], Inf)
    })
    suppressMessages({
      f2 <- m1 %>% ini(x3=c(0,3))
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
        mod %>%
        model(
          ka <- exp(lka + ka_dose*DOSE),
          auto = FALSE
        ) %>%
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
    mod1 %>% ini(Kin = 2),
    "promote `Kin` to population parameter with initial estimate 2"
  )
  expect_message(
    expect_message(
      mod1 %>% ini(Kin = c(1, 2)),
      "promote `Kin` to population parameter with initial estimate 2"
    ),
    regexp = "change initial estimate (2) and lower bound (1) of `Kin`",
    fixed = TRUE
  )
  expect_message(
    expect_message(
      mod1 %>% ini(Kin = c(1, 2, 3)),
      "promote `Kin` to population parameter with initial estimate 2"
    ),
    regexp = "change initial estimate (2) and upper/lower bound (1 to 3) of `Kin`",
    fixed = TRUE
  )
})
