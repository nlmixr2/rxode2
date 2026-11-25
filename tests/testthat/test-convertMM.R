rxTest({
  test_that(".replaceMultC works", {
    # uniary operators
    e1 <- str2lang("d/dt(central) <- - ka * depot")
    e2 <- str2lang("d/dt(central) <- - fun")

    expect_equal(.replaceMultC(e1, str2lang("ka"), str2lang("depot"), str2lang("fun")), e2)

    e1 <- str2lang("d/dt(central) <- + ka * depot")
    e2 <- str2lang("d/dt(central) <- fun")

    expect_equal(.replaceMultC(e1, str2lang("ka"), str2lang("depot"), str2lang("fun")), e2)

    expect_equal(.replaceMultC(e1, str2lang("ka"), str2lang("depot"), str2lang("fun")), e2)

    e1 <- str2lang("d/dt(central) <- ka * depot - kel * central")

    e2 <- str2lang("d/dt(central) <- ka * depot - (vm * central/vc)/(km + central/vc)")

    expect_equal(
      .replaceMultC(e1, str2lang("kel"), str2lang("central"), str2lang("(vm*central/vc)/(km+central/vc)")),
      e2
    )

    expect_equal(
      .replaceMultC(e1, str2lang("central"), str2lang("kel"), str2lang("(vm*central/vc)/(km+central/vc)")),
      e2
    )

    expect_equal(.replaceMultC(e1, str2lang("funny"), str2lang("kel"), str2lang("(vm*central/vc)/(km+central/vc)")), e1)
  })

  test_that("convertMM fun", {
    f <- function() {
      ini({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(convertMM(f), NA)

    f <- function() {
      ini({
      lka <- 0.45
      lkel <- 1
      lvc <- 3.45
      propSd <- 0.5
      eta.cl ~ 0.1
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl + eta.cl)
      vc <- exp(lvc)
      kel <- exp(lkel)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(convertMM(f), NA)

    f <- function() {
      ini({
      lka <- 0.45
      lkel <- 1
      lvc <- 3.45
      propSd <- 0.5
      eta.kel ~ 0.1
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- exp(lkel + eta.kel)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(convertMM(f), NA)

    f <- function() {
      ini({
      lka <- 0.45
      lvc <- 3.45
      propSd <- 0.5
      kel <- 0.1
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(convertMM(f), NA)

    f <- function() {
      ini({
      lka <- 0.45
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(convertMM(f), NA)

    f <- function() {
      ini({
      lka <- 0.45
      lvc <- 3.45
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
    })
    }
    # doesn't need endpoint
    expect_error(convertMM(f), NA)

    # doesn't need estimates
    f <- function() {
      model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
    })
    }

    expect_error(convertMM(f), NA)

    # handles only eta
    f <- function() {
      ini({
      kel ~ 0.1
    })
      model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
    })
    }

    expect_error(convertMM(f), NA)
  })
})
