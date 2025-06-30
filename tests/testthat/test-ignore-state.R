rxTest({
  # Make sure the right state is ignored.
  ## https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf p11

  ## 6.1
  mod <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  test_that("Nothing ignored", {
    expect_equal(rxModelVars(mod)$state.ignore, c(0L, 0L))
  })

  mod <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  mod.f <- rxode2({
    a <- 6
    b <- 0.6
    f(intestine) <- 1
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  mod.alag <- rxode2({
    a <- 6
    b <- 0.6
    alag(intestine) <- 0
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  mod.rate <- rxode2({
    a <- 6
    b <- 0.6
    rate(intestine) <- 2
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  mod.dur <- rxode2({
    a <- 6
    b <- 0.6
    dur(intestine) <- 2
    d / dt(intestine) ~ -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  test_that("Ignore first comparment", {
    expect_equal(rxModelVars(mod)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.f)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.alag)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.rate)$state.ignore, c(1L, 0L))
    expect_equal(rxModelVars(mod.dur)$state.ignore, c(1L, 0L))
  })

  mod <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.f <- rxode2({
    a <- 6
    b <- 0.6
    f(blood) <- 1
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.alag <- rxode2({
    a <- 6
    b <- 0.6
    alag(blood) <- 0
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.rate <- rxode2({
    a <- 6
    b <- 0.6
    rate(blood) <- 2
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.dur <- rxode2({
    a <- 6
    b <- 0.6
    dur(blood) <- 2
    d / dt(intestine) <- -a * intestine
    d / dt(blood) ~ a * intestine - b * blood
  })

  test_that("Nothing ignored", {
    expect_equal(rxModelVars(mod)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.f)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.f)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.alag)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.alag)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.rate)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.rate)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.dur)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.dur)$state, c("intestine", "blood"))
  })

  mod.f <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    f(blood) <- 1
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.alag <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    alag(blood) <- 0
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.rate <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    rate(blood) <- 2
    d / dt(blood) ~ a * intestine - b * blood
  })

  mod.dur <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    dur(blood) <- 2
    d / dt(blood) ~ a * intestine - b * blood
  })


  test_that("Ignore Compartment #2", {
    expect_equal(rxModelVars(mod)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.f)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.f)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.alag)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.alag)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.rate)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.rate)$state, c("intestine", "blood"))
    expect_equal(rxModelVars(mod.dur)$state.ignore, c(0L, 1L))
    expect_equal(rxModelVars(mod.dur)$state, c("intestine", "blood"))
  })

  test_that("ignore state corruption issue #857", {
    d_clean <-
      structure(list(
        amt = c(0, 150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        evid = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        dv = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
        mdv = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
        cmt = c(2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
        cens = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
        id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
        ii = c(0, 24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        addl = c(0, 28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        time = c(0, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8),
        FED = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
        FORMTAB = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
      ), row.names = c(NA, -18L), class = c("tbl_df", "tbl", "data.frame"))

    fit_clean <- function () {
      ini({
        e_f_fed <- fix(0)
        e_f_form <- fix(0)
        lka <- -0.4
        e_ka_fed <- -2
        e_ka_form <- -2
        lcl <- -0.3
        lvc <- 2.5
        lvp <- 5
        lq <- 0.3
        prop_sd <- 0.4
        add_sd <- 17
        bsv_ka ~ 0.3
        bsv_cl ~ 0.5
        bsv_vc ~ 0.5
      })
      model({
        f_depot <- exp(e_f_fed * FED + e_f_form * FORMTAB)
        ka <- exp(lka + bsv_ka + e_ka_fed * FED + e_ka_form *
                    FORMTAB)
        cl <- exp(lcl + bsv_cl)
        vc <- exp(lvc + bsv_vc)
        vp <- exp(lvp)
        q <- exp(lq)
        kel <- cl/vc
        k12 <- q/vc
        k21 <- q/vp
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * f_depot * depot - kel * central -
          k12 * central + k21 * peripheral1
        d/dt(peripheral1) <- k12 * central - k21 * peripheral1
        Cc <- central/vc * 1000
        Cc ~ prop(prop_sd) + add(add_sd)
      })
    }
    expect_error(rxSolve(fit_clean, d_clean), NA)
  })

})
