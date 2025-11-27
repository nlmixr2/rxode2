rxTest({
  test_that("plot tests", {

    skip_if_not_installed("units")
    skip_if_not_installed("ggplot2", minimum_version="3.3.5")
    skip_if_not_installed("vdiffr")
    rxUnloadAll()

    # vdiffr doesn't cause errors on CRAN, so skip there.
    skip_on_cran()

    ## Model from rxode2 tutorial
    m1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      ## Added modeled bioavaiblity, duration and rate
      fdepot <- 1
      durDepot <- 8
      rateDepot <- 1250
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      f(depot) <- fdepot
      dur(depot) <- durDepot
      rate(depot) <- rateDepot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1
    })

    ev <- et(timeUnits = "hr") %>%
      et(amt = 10000, ii = 12, until = 24) %>%
      et(seq(0, 24, length.out = 100))

    evR <- et(timeUnits = "hr") %>% et(evid = 3)

    evR2 <- et(timeUnits = "hr") %>%
      et(amt = 5000, ii = 12, until = 24) %>%
      et(seq(0, 24, length.out = 100))

    evR <- dplyr::bind_rows(as.data.frame(ev), as.data.frame(evR), as.data.frame(evR2))

    evR4 <- rbind(data.frame(id = 1, evR), data.frame(id = 2, evR), data.frame(id = 3, evR), data.frame(id = 4, evR))

    s <- rxSolve(m1, ev)

    sR <- rxSolve(m1, evR)

    ev <- et(timeUnits = "hr") %>%
      et(amt = 10000, until = units::set_units(3, days), ii = 12) %>%
      # loading doses
      et(seq(0, 48, length.out = 200)) %>%
      et(id = 1:4)

    rxWithSeed(
      42,
      s2 <- rxSolve(m1, ev, params = data.frame(KA = 0.294 * exp(rnorm(4)), 18.6 * exp(rnorm(4))))
    )

    rxWithSeed(
      42,
      s2R <- rxSolve(m1, evR4, params = data.frame(KA = 0.294 * exp(rnorm(4)), 18.6 * exp(rnorm(4))))
    )

    rxWithSeed(
      42,
      s20 <- rxSolve(m1, ev, params = data.frame(KA = 0.294 * exp(rnorm(20)), 18.6 * exp(rnorm(20))))
    )

    rxWithSeed(
      42,
      s20R <- rxSolve(m1, evR4, params = data.frame(KA = 0.294 * exp(rnorm(20)), 18.6 * exp(rnorm(20))))
    )

    m2 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01 * exp(eta.Cl)
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      ## Added modeled bioavaiblity, duration and rate
      fdepot <- 1
      durDepot <- 8
      rateDepot <- 1250
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      f(depot) <- fdepot
      dur(depot) <- durDepot
      rate(depot) <- rateDepot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1
    })

    omega <- lotri(eta.Cl ~ 0.4^2)

    ev <- et(amount.units = "mg", time.units = "hours") %>%
      et(amt = 10000, cmt = "centr", until = 48, ii = 8) %>%
      et(0, 48, length.out = 100)

    evR <- et(amount.units = "mg", time.units = "hours") %>% et(evid = 3)

    evR2 <- et(amount.units = "mg", time.units = "hours") %>%
      et(amt = 5000, cmt = "centr", until = 48, ii = 8) %>%
      et(0, 48, length.out = 100)

    evR <- dplyr::bind_rows(as.data.frame(ev), as.data.frame(evR), as.data.frame(evR2))

    ev$extra <- c(rep(1, 50), rep(0, 51))

    sim <- rxSolve(m2, ev, omega = omega, nSub = 100, keep="extra")

    simR <- rxSolve(m2, evR, omega = omega, nSub = 100)

    expect_error(plot(sim, log = "f"))

    ev2 <- et() %>%
      et(amt = 10000, cmt = "centr", until = 48, ii = 8) %>%
      et(0, 48, length.out = 100)

    ev2R <- et(evid = 3)

    ev2R2 <- et() %>%
      et(amt = 5000, cmt = "centr", until = 48, ii = 8) %>%
      et(0, 48, length.out = 100)

    ev2R <- dplyr::bind_rows(as.data.frame(ev2), as.data.frame(ev2R), as.data.frame(ev2R2))

    sim3 <- rxSolve(m2, ev2, omega = omega, nSub = 3)
    sim3R <- rxSolve(m2, ev2R, omega = omega, nSub = 3)

    .rxWithOptions(list(rxode2.theme = TRUE), {
      #vdiffr::expect_doppelganger("sim.id-unitless", plot(sim3, C2))
      expect_error(plot(sim3, C2), NA)
    })

    .rxWithOptions(list(rxode2.theme = FALSE), {
      #vdiffr::expect_doppelganger("sim.id-unitless-notheme", plot(sim3, C2))
      expect_error(plot(sim3, C2), NA)
    })

    .rxWithOptions(list(rxode2.theme = TRUE), {

      ci1.C2 <- confint(sim, "C2", ci=0.99)

      ci1.C3 <- confint(sim,  ci=0.99)

      ci1.C2.e <- confint(sim, "C2", by="extra")

      ci1.C2.eff <- confint(sim, c("C2", "eff"))

      ci1.C2.eff.e <- confint(sim, c("C2", "eff"), by="extra")

      sim2 <- rxSolve(m2, ev, omega = omega, nSub = 2500, keep="extra")

      sim2R <- rxSolve(m2, evR, omega = omega, nSub = 2500)

      ci2.C2 <- confint(sim2, "C2")

      ci2.C2.e <- confint(sim2, "C2", by="extra")

      ci2.C2.eff <- confint(sim2, c("C2", "eff"))

      ci2.C2.eff.e <- confint(sim2, c("C2", "eff"), by="extra")

      f <- function(xgxr = FALSE, repel = FALSE) {
        if (xgxr) {
          .xgxtxt <- "xgxr-"
          .xgxOp <- list(rxode2.xgxr = TRUE)
        } else {
          .xgxtxt <- ""
          .xgxOp <- list(rxode2.xgxr = FALSE)
        }
        .rxWithOptions(.xgxOp, {
          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2"), suppressWarnings(s %>% plot(C2)))
          expect_error(suppressWarnings(s %>% plot(C2)), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-x"), suppressWarnings(s %>% plot(C2, log = "x")))
          expect_error(suppressWarnings(s %>% plot(C2, log = "x")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-y"), suppressWarnings(s %>% plot(C2, log = "y")))
          expect_error(suppressWarnings(s %>% plot(C2, log = "y")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-xy"), suppressWarnings(s %>% plot(C2, log = "xy")))
          expect_error(suppressWarnings(s %>% plot(C2, log = "xy")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-yx"), suppressWarnings(s %>% plot(C2, log = "yx")))

          expect_error(suppressWarnings(s %>% plot(C2, log = "yx")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all"), suppressWarnings(s %>% plot()))
          expect_error(suppressWarnings(s %>% plot()), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-x"), suppressWarnings(s %>% plot(log = "x")))
          expect_error(suppressWarnings(s %>% plot(log = "x")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-y"), suppressWarnings(s %>% plot(log = "y")))
          expect_error(suppressWarnings(s %>% plot(log = "y")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-xy"), suppressWarnings(s %>% plot(log = "xy")))
          expect_error(suppressWarnings(s %>% plot(log = "xy")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-yx"), suppressWarnings(s %>% plot(log = "yx")))
          expect_error(suppressWarnings(s %>% plot(log = "yx")), NA)


          #vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-r"), suppressWarnings(sR %>% plot(C2)))
          expect_error(suppressWarnings(sR %>% plot(C2)), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-x-r"), suppressWarnings(sR %>% plot(C2, log = "x")))
          expect_error(suppressWarnings(sR %>% plot(C2, log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-y-r"), suppressWarnings(sR %>% plot(C2, log = "y")))
          expect_error(suppressWarnings(sR %>% plot(C2, log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-xy-r"), suppressWarnings(sR %>% plot(C2, log = "xy")))
          expect_error(suppressWarnings(sR %>% plot(C2, log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "C2-log-yx-r"), suppressWarnings(sR %>% plot(C2, log = "yx")))
          expect_error(suppressWarnings(sR %>% plot(C2, log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-r"), suppressWarnings(sR %>% plot()))
          expect_error(suppressWarnings(sR %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-x-r"), suppressWarnings(sR %>% plot(log = "x")))
          expect_error(suppressWarnings(sR %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-y-r"), suppressWarnings(sR %>% plot(log = "y")))
          expect_error(suppressWarnings(sR %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-xy-r"), suppressWarnings(sR %>% plot(log = "xy")))
          expect_error(suppressWarnings(sR %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-", .xgxtxt, "all-log-yx-r"), suppressWarnings(sR %>% plot(log = "yx")))
          expect_error(suppressWarnings(sR %>% plot(log = "yx")), NA)


          # vdiffr::expect_doppelganger(paste0("plot-ci1c2", .xgxtxt), suppressWarnings(ci1.C2 %>% plot()))
          expect_error(suppressWarnings(ci1.C2 %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c3", .xgxtxt, "-centr"), suppressWarnings(ci1.C3 %>% plot("centr")))
          expect_error(suppressWarnings(ci1.C3 %>% plot("centr")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c3", .xgxtxt, "-full"), suppressWarnings(ci1.C3 %>% plot()))
          expect_error(suppressWarnings(ci1.C3 %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2", .xgxtxt, "log-x"), suppressWarnings(ci1.C2 %>% plot(log = "x")))
          expect_error(suppressWarnings(ci1.C2 %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2", .xgxtxt, "log-y"), suppressWarnings(ci1.C2 %>% plot(log = "y")))
          expect_error(suppressWarnings(ci1.C2 %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2", .xgxtxt, "log-xy"), suppressWarnings(ci1.C2 %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci1.C2 %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2", .xgxtxt, "log-yx"), suppressWarnings(ci1.C2 %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci1.C2 %>% plot(log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-e", .xgxtxt), suppressWarnings(ci1.C2.e %>% plot()))
          expect_error(suppressWarnings(ci1.C2.e %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-e", .xgxtxt, "log-x"), suppressWarnings(ci1.C2.e %>% plot(log = "x")))
          expect_error(suppressWarnings(ci1.C2.e %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-e", .xgxtxt, "log-y"), suppressWarnings(ci1.C2.e %>% plot(log = "y")))
          expect_error(suppressWarnings(ci1.C2.e %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-e", .xgxtxt, "log-xy"), suppressWarnings(ci1.C2.e %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci1.C2.e %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-e", .xgxtxt, "log-yx"), suppressWarnings(ci1.C2.e %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci1.C2.e %>% plot(log = "yx")), NA)


          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff", .xgxtxt), suppressWarnings(ci1.C2.eff %>% plot()))
          expect_error(suppressWarnings(ci1.C2.eff %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff", .xgxtxt, "log-x"), suppressWarnings(ci1.C2.eff %>% plot(log = "x")))
          expect_error(suppressWarnings(ci1.C2.eff %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff", .xgxtxt, "log-y"), suppressWarnings(ci1.C2.eff %>% plot(log = "y")))

          expect_error(suppressWarnings(ci1.C2.eff %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff", .xgxtxt, "log-xy"), suppressWarnings(ci1.C2.eff %>% plot(log = "xy")))

          expect_error(suppressWarnings(ci1.C2.eff %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff", .xgxtxt, "log-yx"), suppressWarnings(ci1.C2.eff %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci1.C2.eff %>% plot(log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff-e", .xgxtxt), suppressWarnings(ci1.C2.eff.e %>% plot()))
          expect_error(suppressWarnings(ci1.C2.eff.e %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff-e", .xgxtxt, "log-x"), suppressWarnings(ci1.C2.eff.e %>% plot(log = "x")))
          expect_error(suppressWarnings(ci1.C2.eff.e %>% plot(log = "x")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff-e", .xgxtxt, "log-y"), suppressWarnings(ci1.C2.eff.e %>% plot(log = "y")))
          expect_error(suppressWarnings(ci1.C2.eff.e %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff-e", .xgxtxt, "log-xy"), suppressWarnings(ci1.C2.eff.e %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci1.C2.eff.e %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci1c2-eff-e", .xgxtxt, "log-yx"), suppressWarnings(ci1.C2.eff.e %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci1.C2.eff.e %>% plot(log = "yx")), NA)


          # vdiffr::expect_doppelganger(paste0("plot-ci2c2", .xgxtxt), suppressWarnings(ci2.C2 %>% plot()))
          expect_error(suppressWarnings(ci2.C2 %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2", .xgxtxt, "log-x"), suppressWarnings(ci2.C2 %>% plot(log = "x")))
          expect_error(suppressWarnings(ci2.C2 %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2", .xgxtxt, "log-y"), suppressWarnings(ci2.C2 %>% plot(log = "y")))
          expect_error(suppressWarnings(ci2.C2 %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2", .xgxtxt, "log-xy"), suppressWarnings(ci2.C2 %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci2.C2 %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2", .xgxtxt, "log-yx"), suppressWarnings(ci2.C2 %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci2.C2 %>% plot(log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-e", .xgxtxt), suppressWarnings(ci2.C2.e %>% plot()))
          expect_error(suppressWarnings(ci2.C2.e %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-e", .xgxtxt, "log-x"), suppressWarnings(ci2.C2.e %>% plot(log = "x")))
          expect_error(suppressWarnings(ci2.C2.e %>% plot(log = "x")), NA)


          ## vdiffr::expect_doppelganger(paste0("plot-ci2c2-e", .xgxtxt, "log-y"), suppressWarnings(ci2.C2.e %>% plot(log = "y")))
          expect_error(suppressWarnings(ci2.C2.e %>% plot(log = "y")), NA)


          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-e", .xgxtxt, "log-xy"), suppressWarnings(ci2.C2.e %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci2.C2.e %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-e", .xgxtxt, "log-yx"), suppressWarnings(ci2.C2.e %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci2.C2.e %>% plot(log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff", .xgxtxt), suppressWarnings(ci2.C2.eff %>% plot()))
          expect_error(suppressWarnings(ci2.C2.eff %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff", .xgxtxt, "log-x"), suppressWarnings(ci2.C2.eff %>% plot(log = "x")))
          expect_error(suppressWarnings(ci2.C2.eff %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff", .xgxtxt, "log-y"), suppressWarnings(ci2.C2.eff %>% plot(log = "y")))
          expect_error(suppressWarnings(ci2.C2.eff %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff", .xgxtxt, "log-xy"), suppressWarnings(ci2.C2.eff %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci2.C2.eff %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff", .xgxtxt, "log-yx"), suppressWarnings(ci2.C2.eff %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci2.C2.eff %>% plot(log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff-e", .xgxtxt), suppressWarnings(ci2.C2.eff.e %>% plot()))
          expect_error(suppressWarnings(ci2.C2.eff.e %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff-e", .xgxtxt, "log-x"), suppressWarnings(ci2.C2.eff.e %>% plot(log = "x")))
          expect_error(suppressWarnings(ci2.C2.eff.e %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff-e", .xgxtxt, "log-y"), suppressWarnings(ci2.C2.eff.e %>% plot(log = "y")))
          expect_error(suppressWarnings(ci2.C2.eff.e %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff-e", .xgxtxt, "log-xy"), suppressWarnings(ci2.C2.eff.e %>% plot(log = "xy")))
          expect_error(suppressWarnings(ci2.C2.eff.e %>% plot(log = "xy")), NA)


          ## vdiffr::expect_doppelganger(paste0("plot-ci2c2-eff-e", .xgxtxt, "log-yx"), suppressWarnings(ci2.C2.eff.e %>% plot(log = "yx")))
          expect_error(suppressWarnings(ci2.C2.eff.e %>% plot(log = "yx")), NA)

          ##

          ## large
          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2"), suppressWarnings(s20 %>% plot(C2)))
          expect_error(suppressWarnings(s20 %>% plot(C2)), NA)


          #vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-x"), suppressWarnings(s20 %>% plot(C2, log = "x")))
          expect_error(suppressWarnings(s20 %>% plot(C2, log = "x")), NA)

          #vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-y"), suppressWarnings(s20 %>% plot(C2, log = "y")))
          expect_error(suppressWarnings(s20 %>% plot(C2, log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-xy"), suppressWarnings(s20 %>% plot(C2, log = "xy")))
          expect_error(suppressWarnings(s20 %>% plot(C2, log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-yx"), suppressWarnings(s20 %>% plot(C2, log = "yx")))
          expect_error(suppressWarnings(s20 %>% plot(C2, log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all"), suppressWarnings(s20 %>% plot()))
          expect_error(suppressWarnings(s20 %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-x"), suppressWarnings(s20 %>% plot(log = "x")))
          expect_error(suppressWarnings(s20 %>% plot(log = "x")), NA)

          ## vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-y"), suppressWarnings(s20 %>% plot(log = "y")))
          expect_error(suppressWarnings(s20 %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-xy"), suppressWarnings(s20 %>% plot(log = "xy")))
          expect_error(suppressWarnings(s20 %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-yx"), suppressWarnings(s20 %>% plot(log = "yx")))
          expect_error(suppressWarnings(s20 %>% plot(log = "yx")), NA)


          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-r"), suppressWarnings(s20R %>% plot(C2)))
          expect_error(suppressWarnings(s20R %>% plot(C2)), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-x-r"), suppressWarnings(s20R %>% plot(C2, log = "x")))
          expect_error(suppressWarnings(s20R %>% plot(C2, log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-y-r"), suppressWarnings(s20R %>% plot(C2, log = "y")))
          expect_error(suppressWarnings(s20R %>% plot(C2, log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-xy-r"), suppressWarnings(s20R %>% plot(C2, log = "xy")))
          expect_error(suppressWarnings(s20R %>% plot(C2, log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "C2-log-yx-r"), suppressWarnings(s20R %>% plot(C2, log = "yx")))
          expect_error(suppressWarnings(s20R %>% plot(C2, log = "yx")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-r"), suppressWarnings(s20R %>% plot()))
          expect_error(suppressWarnings(s20R %>% plot()), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-x-r"), suppressWarnings(s20R %>% plot(log = "x")))
          expect_error(suppressWarnings(s20R %>% plot(log = "x")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-y-r"), suppressWarnings(s20R %>% plot(log = "y")))
          expect_error(suppressWarnings(s20R %>% plot(log = "y")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-xy-r"), suppressWarnings(s20R %>% plot(log = "xy")))
          expect_error(suppressWarnings(s20R %>% plot(log = "xy")), NA)

          # vdiffr::expect_doppelganger(paste0("plot-sp-", .xgxtxt, "all-log-yx-r"), suppressWarnings(s20R %>% plot(log = "yx")))
          expect_error(suppressWarnings(s20R %>% plot(log = "yx")), NA)

          for (repel in c(TRUE, FALSE)) {
            if (repel) {
              .repel <- "repel-"
              .repelOp <- list(rxode2.ggrepel = TRUE)
            } else {
              .repel <- ""
              .repelOp <- list(rxode2.ggrepel = FALSE)
            }
            .rxWithOptions(.repelOp, {

              #vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2"), suppressWarnings(s2 %>% plot(C2)))
              expect_error(suppressWarnings(s2 %>% plot(C2)), NA)

              #vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-x"), suppressWarnings(s2 %>% plot(C2, log = "x")))
              expect_error(suppressWarnings(s2 %>% plot(C2, log = "x")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-y"), suppressWarnings(s2 %>% plot(C2, log = "y")))
              expect_error(suppressWarnings(s2 %>% plot(C2, log = "y")), NA)


              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-xy"), suppressWarnings(s2 %>% plot(C2, log = "xy")))
              expect_error(suppressWarnings(s2 %>% plot(C2, log = "xy")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-yx"), suppressWarnings(s2 %>% plot(C2, log = "yx")))
              expect_error(suppressWarnings(s2 %>% plot(C2, log = "yx")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all"), suppressWarnings(s2 %>% plot()))
              expect_error(suppressWarnings(s2 %>% plot()), NA)

              ## vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-x"), suppressWarnings(s2 %>% plot(log = "x")))
              expect_error(suppressWarnings(s2 %>% plot(log = "x")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-y"), suppressWarnings(s2 %>% plot(log = "y")))
              expect_error(suppressWarnings(s2 %>% plot(log = "y")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-xy"), suppressWarnings(s2 %>% plot(log = "xy")))
              expect_error(suppressWarnings(s2 %>% plot(log = "xy")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-yx"), suppressWarnings(s2 %>% plot(log = "yx")))
              expect_error(suppressWarnings(s2 %>% plot(log = "yx")), NA)

              ## Issue #284
              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "284-log-yx"), suppressWarnings(s2 %>% plot(C2, eff, log = "yx")))
              expect_error(suppressWarnings(s2 %>% plot(C2, eff, log = "yx")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "284-log-y"), suppressWarnings(s2 %>% plot(C2, eff, log = "y")))
              expect_error(suppressWarnings(s2 %>% plot(C2, eff, log = "y")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "284-log-x"), suppressWarnings(s2 %>% plot(C2, eff, log = "x")))
              expect_error(suppressWarnings(s2 %>% plot(C2, eff, log = "x")), NA)

              # reset
              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-r"), suppressWarnings(s2R %>% plot(C2)))
              expect_error(suppressWarnings(s2R %>% plot(C2)), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-x-r"), suppressWarnings(s2R %>% plot(C2, log = "x")))

              expect_error(suppressWarnings(s2R %>% plot(C2, log = "x")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-y-r"), suppressWarnings(s2R %>% plot(C2, log = "y")))
              expect_error(suppressWarnings(s2R %>% plot(C2, log = "y")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-xy-r"), suppressWarnings(s2R %>% plot(C2, log = "xy")))
              expect_error(suppressWarnings(s2R %>% plot(C2, log = "xy")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "C2-log-yx-r"), suppressWarnings(s2R %>% plot(C2, log = "yx")))
              expect_error(suppressWarnings(s2R %>% plot(C2, log = "yx")), NA)


              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-r"), suppressWarnings(s2R %>% plot()))
              expect_error(suppressWarnings(s2R %>% plot()), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-x-r"), suppressWarnings(s2R %>% plot(log = "x")))
              expect_error(suppressWarnings(s2R %>% plot(log = "x")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-y-r"), suppressWarnings(s2R %>% plot(log = "y")))
              expect_error(suppressWarnings(s2R %>% plot(log = "y")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-xy-r"), suppressWarnings(s2R %>% plot(log = "xy")))
              expect_error(suppressWarnings(s2R %>% plot(log = "xy")), NA)

              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "all-log-yx-r"), suppressWarnings(s2R %>% plot(log = "yx")))
              expect_error(suppressWarnings(s2R %>% plot(log = "yx")), NA)

              ## Issue #284
              # vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "284-log-yx-r"), suppressWarnings(s2R %>% plot(C2, eff, log = "yx")))
              expect_error(suppressWarnings(s2R %>% plot(C2, eff, log = "yx")), NA)

              #vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "284-log-y-r"), suppressWarnings(s2R %>% plot(C2, eff, log = "y")))

              expect_error(suppressWarnings(s2R %>% plot(C2, eff, log = "y")), NA)

              #vdiffr::expect_doppelganger(paste0("plot-multi-", .repel, .xgxtxt, "284-log-x-r"), suppressWarnings(s2R %>% plot(C2, eff, log = "x")))
              expect_error(suppressWarnings(s2R %>% plot(C2, eff, log = "x")), NA)

            })
          }
        })
      }
      f(TRUE)
      f(FALSE)
    })
  })
})
