rxTest({
  warfarin <- nlmixr2data::warfarin

  test_that("cmt() syntax makes sense", {

    mod <- rxode2({
      a <- 6
      b <- 0.6
      cmt(blood) # cmt = 1 now
      d / dt(intestine) <- -a * intestine
      d / dt(blood) <- a * intestine - b * blood
    })

    expect_equal(c("blood", "intestine"), rxState(mod))

    mod <- rxode2({
      a <- 6
      b <- 0.6
      d / dt(intestine) <- -a * intestine
      d / dt(blood) <- a * intestine - b * blood
    })

    expect_equal(c("intestine", "blood"), rxState(mod))


    expect_error(suppressMessages(rxode2({
      a <- 6
      b <- 0.6
      cmt(matt) # cmt = 1 now
      d/dt(intestine) <- -a * intestine
      d/dt(blood) <- a * intestine - b * blood
    })))


    tmp <- rxode2({
      a <- 6
      b <- 0.6
      cmt(blood) # cmt = 1 now
      cmt(intestine) # cmt = 2 now
      cmt(matt)
      d / dt(intestine) <- -a * intestine
      d / dt(blood) <- a * intestine - b * blood
    })
    expect_equal(tmp$stateExtra, "matt")

  })

  test_that("Compartment melding with dvid", {

    w <- rxode2({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      ##
      logit <- exp(poplogit + eta.emax)
      emax <- logit / (1 + logit)
      ec50 <- exp(tec50 + eta.ec50)
      kout <- exp(tkout + eta.kout)
      e0 <- exp(te0 + eta.e0)
      ##
      DCP <- center / v
      PD <- 1 - emax * DCP / (ec50 + DCP)
      ##
      pca(0) <- e0
      kin <- e0 * kout
      ##
      d / dt(depot) <- -ktr * depot
      d / dt(gut) <- ktr * depot - ka * gut
      d / dt(center) <- ka * gut - cl / v * center
      d / dt(pca) <- kin * PD - kout * pca
      ##
      cp <- center / v
      cmt(cp)
      ## These dvid w/ negative values are counted backward from the last
      dvid(-1, 4)
    })

    w2 <- nlmixr2data::warfarin

    tmp <- etTrans(w2, w, addCmt = TRUE)

    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 4L, 5L, 4L, 5L,
        4L, 4L, 4L, 4L
      )
    )

    w <- rxode2({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      ##
      logit <- exp(poplogit + eta.emax)
      emax <- logit / (1 + logit)
      ec50 <- exp(tec50 + eta.ec50)
      kout <- exp(tkout + eta.kout)
      e0 <- exp(te0 + eta.e0)
      ##
      DCP <- center / v
      PD <- 1 - emax * DCP / (ec50 + DCP)
      ##
      pca(0) <- e0
      kin <- e0 * kout
      ##
      d / dt(depot) <- -ktr * depot
      d / dt(gut) <- ktr * depot - ka * gut
      d / dt(center) <- ka * gut - cl / v * center
      d / dt(pca) <- kin * PD - kout * pca
      ##
      cp <- center / v
      cmt(cp)
      dvid(5, 4)
    })

    w2 <- nlmixr2data::warfarin

    tmp <- etTrans(w2, w, addCmt = TRUE)

    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 4L, 5L, 4L, 5L,
        4L, 4L, 4L, 4L
      )
    )

    w2$dvid <- paste(w2$dvid)
    tmp <- etTrans(w2, w, addCmt = TRUE)
    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 4L, 5L, 4L, 5L,
        4L, 4L, 4L, 4L
      )
    )

    w2 <- nlmixr2data::warfarin
    w2$dvid <- as.integer(w2$dvid)
    tmp <- etTrans(w2, w, addCmt = TRUE)
    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 4L, 5L, 4L, 5L,
        4L, 4L, 4L, 4L
      )
    )

    w2 <- nlmixr2data::warfarin
    w2$dvid <- as.integer(w2$dvid) * 10
    expect_warning(tmp <- etTrans(w2, w, addCmt = TRUE))
    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 4L, 5L, 4L, 5L,
        4L, 4L, 4L, 4L
      )
    )

    w <- rxode2({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      ##
      logit <- exp(poplogit + eta.emax)
      emax <- logit / (1 + logit)
      ec50 <- exp(tec50 + eta.ec50)
      kout <- exp(tkout + eta.kout)
      e0 <- exp(te0 + eta.e0)
      ##
      DCP <- center / v
      PD <- 1 - emax * DCP / (ec50 + DCP)
      ##
      pca(0) <- e0
      kin <- e0 * kout
      ##
      d / dt(depot) <- -ktr * depot
      d / dt(gut) <- ktr * depot - ka * gut
      d / dt(center) <- ka * gut - cl / v * center
      d / dt(pca) <- kin * PD - kout * pca
      ##
      cp <- center / v
      cmt(cp)
      dvid(5)
    })

    w2 <- nlmixr2data::warfarin
    w2$dvid <- as.integer(w2$dvid) * 10
    expect_warning(tmp <- etTrans(w2, w, addCmt = TRUE))
    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 1L, 5L, 1L, 5L, 1L, 5L,
        1L, 1L, 1L, 1L
      )
    )

    w <- rxode2({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      ##
      logit <- exp(poplogit + eta.emax)
      emax <- logit / (1 + logit)
      ec50 <- exp(tec50 + eta.ec50)
      kout <- exp(tkout + eta.kout)
      e0 <- exp(te0 + eta.e0)
      ##
      DCP <- center / v
      PD <- 1 - emax * DCP / (ec50 + DCP)
      ##
      pca(0) <- e0
      kin <- e0 * kout
      ##
      d / dt(depot) <- -ktr * depot
      d / dt(gut) <- ktr * depot - ka * gut
      d / dt(center) <- ka * gut - cl / v * center
      d / dt(pca) <- kin * PD - kout * pca
      ##
      cp <- center / v
      cmt(cp)
      dvid(4, 5)
    })

    w2 <- nlmixr2data::warfarin
    w2$dvid <- as.integer(w2$dvid)
    tmp <- etTrans(w2, w, addCmt = TRUE)
    expect_equal(
      tmp$CMT[tmp$ID == 1],
      c(
        1L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 4L, 5L, 4L, 5L, 4L,
        5L, 5L, 5L, 5L
      )
    )

    ## Warfarin model
    inner <- rxode2({
      cmt(depot)
      cmt(gut)
      cmt(center)
      cmt(effect)
      rx_expr_001 ~ -depot
      rx_expr_002 ~ -center
      rx_expr_003 ~ -ETA[4]
      rx_expr_004 ~ -effect
      rx_expr_005 ~ -2 * ETA[4]
      rx_expr_006 ~ 2 * THETA[4]
      rx_expr_007 ~ ETA[1] + THETA[1]
      rx_expr_008 ~ ETA[2] + THETA[2]
      rx_expr_009 ~ ETA[3] + THETA[3]
      rx_expr_010 ~ ETA[7] + THETA[9]
      rx_expr_011 ~ ETA[5] + THETA[7]
      rx_expr_012 ~ ETA[6] + THETA[8]
      rx_expr_013 ~ rx_expr_003 - THETA[4]
      rx_expr_014 ~ ETA[8] + THETA[10]
      rx_expr_015 ~ exp(rx_expr_007)
      rx_expr_016 ~ exp(rx_expr_008)
      rx_expr_017 ~ exp(rx_expr_009)
      rx_expr_018 ~ exp(rx_expr_010)
      rx_expr_019 ~ exp(rx_expr_011)
      rx_expr_020 ~ exp(rx_expr_012)
      rx_expr_021 ~ rx_expr_005 - rx_expr_006
      rx_expr_022 ~ Rx_pow_di(center, 2)
      rx_expr_023 ~ exp(rx_expr_013)
      rx_expr_024 ~ exp(rx_expr_014)
      rx_expr_025 ~ rx_expr_019 + 1
      rx_expr_026 ~ Rx_pow_di(THETA[5], 2)
      rx_expr_027 ~ gut * rx_expr_016
      rx_expr_028 ~ 2 * rx_expr_026
      rx_expr_029 ~ exp(rx_expr_021)
      rx_expr_030 ~ depot * rx_expr_015
      rx_expr_031 ~ rx_expr_001 * rx_expr_015
      rx_expr_032 ~ rx_expr_002 * rx_expr_017
      rx_expr_033 ~ rx_expr_004 * rx_expr_018
      rx_expr_034 ~ center * rx_expr_023
      rx_expr_035 ~ rx_expr_002 * rx_expr_023
      rx_expr_036 ~ rx_expr_028 * center
      rx_expr_037 ~ center * rx_expr_029
      rx_expr_038 ~ rx_expr_023 * rx_expr_019
      rx_expr_039 ~ rx__sens_gut_BY_ETA_1___ * rx_expr_016
      rx_expr_040 ~ rx__sens_gut_BY_ETA_2___ * rx_expr_016
      rx_expr_041 ~ rx__sens_gut_BY_ETA_3___ * rx_expr_016
      rx_expr_042 ~ rx__sens_gut_BY_ETA_4___ * rx_expr_016
      rx_expr_043 ~ rx__sens_gut_BY_ETA_5___ * rx_expr_016
      rx_expr_044 ~ rx__sens_gut_BY_ETA_6___ * rx_expr_016
      rx_expr_045 ~ rx__sens_gut_BY_ETA_7___ * rx_expr_016
      rx_expr_046 ~ rx__sens_gut_BY_ETA_8___ * rx_expr_016
      rx_expr_047 ~ rx__sens_depot_BY_ETA_1___ * rx_expr_015
      rx_expr_048 ~ rx_expr_034 + rx_expr_020
      rx_expr_049 ~ rx_expr_034 * rx_expr_019
      rx_expr_050 ~ rx_expr_032 * rx_expr_023
      rx_expr_051 ~ rx_expr_035 * rx_expr_019
      rx_expr_052 ~ rx_expr_037 * rx_expr_019
      rx_expr_053 ~ Rx_pow_di(rx_expr_048, 2)
      rx_expr_054 ~ (rx_expr_048) * (rx_expr_025)
      rx_expr_055 ~ rx_expr_053 * (rx_expr_025)
      rx_expr_056 ~ rx_expr_038 / (rx_expr_054)
      rx_expr_057 ~ rx_expr_051 / (rx_expr_054)
      rx_expr_058 ~ rx_expr_057 + 1
      rx_expr_059 ~ rx_expr_052 / (rx_expr_055)
      rx_expr_060 ~ (rx_expr_058) * rx_expr_018
      rx_expr_061 ~ rx_expr_060 * rx_expr_024
      rx_expr_062 ~ rx_expr_059 - rx_expr_056
      d / dt(depot) <- rx_expr_031
      d / dt(gut) <- rx_expr_030 - rx_expr_027
      d / dt(center) <- rx_expr_050 + rx_expr_027
      effect(0) <- rx_expr_024
      d / dt(effect) <- rx_expr_033 + rx_expr_061
      d / dt(rx__sens_depot_BY_ETA_1___) ~ rx_expr_031 - rx_expr_047
      d / dt(rx__sens_depot_BY_ETA_2___) ~ -rx__sens_depot_BY_ETA_2___ * rx_expr_015
      d / dt(rx__sens_depot_BY_ETA_3___) ~ -rx__sens_depot_BY_ETA_3___ * rx_expr_015
      d / dt(rx__sens_depot_BY_ETA_4___) ~ -rx__sens_depot_BY_ETA_4___ * rx_expr_015
      d / dt(rx__sens_depot_BY_ETA_5___) ~ -rx__sens_depot_BY_ETA_5___ * rx_expr_015
      d / dt(rx__sens_depot_BY_ETA_6___) ~ -rx__sens_depot_BY_ETA_6___ * rx_expr_015
      d / dt(rx__sens_depot_BY_ETA_7___) ~ -rx__sens_depot_BY_ETA_7___ * rx_expr_015
      d / dt(rx__sens_depot_BY_ETA_8___) ~ -rx__sens_depot_BY_ETA_8___ * rx_expr_015
      d / dt(rx__sens_gut_BY_ETA_1___) ~ rx_expr_030 + rx_expr_047 - rx_expr_039
      d / dt(rx__sens_gut_BY_ETA_2___) ~ -gut * rx_expr_016 + rx__sens_depot_BY_ETA_2___ * rx_expr_015 - rx_expr_040
      d / dt(rx__sens_gut_BY_ETA_3___) ~ rx__sens_depot_BY_ETA_3___ * rx_expr_015 - rx_expr_041
      d / dt(rx__sens_gut_BY_ETA_4___) ~ rx__sens_depot_BY_ETA_4___ * rx_expr_015 - rx_expr_042
      d / dt(rx__sens_gut_BY_ETA_5___) ~ rx__sens_depot_BY_ETA_5___ * rx_expr_015 - rx_expr_043
      d / dt(rx__sens_gut_BY_ETA_6___) ~ rx__sens_depot_BY_ETA_6___ * rx_expr_015 - rx_expr_044
      d / dt(rx__sens_gut_BY_ETA_7___) ~ rx__sens_depot_BY_ETA_7___ * rx_expr_015 - rx_expr_045
      d / dt(rx__sens_gut_BY_ETA_8___) ~ rx__sens_depot_BY_ETA_8___ * rx_expr_015 - rx_expr_046
      d / dt(rx__sens_center_BY_ETA_1___) ~ -rx__sens_center_BY_ETA_1___ * rx_expr_017 * rx_expr_023 + rx_expr_039
      d / dt(rx__sens_center_BY_ETA_2___) ~ rx_expr_027 - rx__sens_center_BY_ETA_2___ * rx_expr_017 * rx_expr_023 + rx_expr_040
      d / dt(rx__sens_center_BY_ETA_3___) ~ rx_expr_050 - rx__sens_center_BY_ETA_3___ * rx_expr_017 * rx_expr_023 + rx_expr_041
      d / dt(rx__sens_center_BY_ETA_4___) ~ center * rx_expr_017 * rx_expr_023 - rx__sens_center_BY_ETA_4___ * rx_expr_017 * rx_expr_023 + rx_expr_042
      d / dt(rx__sens_center_BY_ETA_5___) ~ -rx__sens_center_BY_ETA_5___ * rx_expr_017 * rx_expr_023 + rx_expr_043
      d / dt(rx__sens_center_BY_ETA_6___) ~ -rx__sens_center_BY_ETA_6___ * rx_expr_017 * rx_expr_023 + rx_expr_044
      d / dt(rx__sens_center_BY_ETA_7___) ~ -rx__sens_center_BY_ETA_7___ * rx_expr_017 * rx_expr_023 + rx_expr_045
      d / dt(rx__sens_center_BY_ETA_8___) ~ -rx__sens_center_BY_ETA_8___ * rx_expr_017 * rx_expr_023 + rx_expr_046
      d / dt(rx__sens_effect_BY_ETA_1___) ~ rx__sens_center_BY_ETA_1___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_1___ * rx_expr_018
      d / dt(rx__sens_effect_BY_ETA_2___) ~ rx__sens_center_BY_ETA_2___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_2___ * rx_expr_018
      d / dt(rx__sens_effect_BY_ETA_3___) ~ rx__sens_center_BY_ETA_3___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_3___ * rx_expr_018
      d / dt(rx__sens_effect_BY_ETA_4___) ~ rx__sens_center_BY_ETA_4___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_4___ * rx_expr_018 + (-rx_expr_022 * rx_expr_029 * rx_expr_019 / (rx_expr_055) + rx_expr_049 / (rx_expr_054)) * rx_expr_018 * rx_expr_024
      d / dt(rx__sens_effect_BY_ETA_5___) ~ rx__sens_center_BY_ETA_5___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_5___ * rx_expr_018 + (rx_expr_057 + rx_expr_034 * exp(2 * ETA[5] + 2 * THETA[7]) / ((rx_expr_048) * Rx_pow_di(rx_expr_025, 2))) * rx_expr_018 * rx_expr_024
      d / dt(rx__sens_effect_BY_ETA_6___) ~ rx_expr_049 * rx_expr_020 * rx_expr_018 * rx_expr_024 / (rx_expr_055) + rx__sens_center_BY_ETA_6___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_6___ * rx_expr_018
      d / dt(rx__sens_effect_BY_ETA_7___) ~ rx_expr_033 + rx__sens_center_BY_ETA_7___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_7___ * rx_expr_018 + rx_expr_061
      rx__sens_effect_BY_ETA_8___(0) <- rx_expr_024
      d / dt(rx__sens_effect_BY_ETA_8___) ~ rx__sens_center_BY_ETA_8___ * (rx_expr_062) * rx_expr_018 * rx_expr_024 - rx__sens_effect_BY_ETA_8___ * rx_expr_018 + rx_expr_061
      rx_yj_ ~ 2
      rx_lambda_ ~ 1
      if (CMT == 3) {
        rx_pred_ <- rx_expr_034
        rx__sens_rx_pred__BY_ETA_1___ <- rx__sens_center_BY_ETA_1___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_2___ <- rx__sens_center_BY_ETA_2___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_3___ <- rx__sens_center_BY_ETA_3___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_4___ <- rx_expr_035 + rx__sens_center_BY_ETA_4___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_5___ <- rx__sens_center_BY_ETA_5___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_6___ <- rx__sens_center_BY_ETA_6___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_7___ <- rx__sens_center_BY_ETA_7___ * rx_expr_023
        rx__sens_rx_pred__BY_ETA_8___ <- rx__sens_center_BY_ETA_8___ * rx_expr_023
        rx_r_ <- rx_expr_026 * rx_expr_022 * rx_expr_029 + Rx_pow_di(THETA[6], 2)
        rx__sens_rx_r__BY_ETA_1___ <- rx_expr_036 * rx__sens_center_BY_ETA_1___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_2___ <- rx_expr_036 * rx__sens_center_BY_ETA_2___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_3___ <- rx_expr_036 * rx__sens_center_BY_ETA_3___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_4___ <- -2 * rx_expr_026 * rx_expr_022 * rx_expr_029 + rx_expr_036 * rx__sens_center_BY_ETA_4___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_5___ <- rx_expr_036 * rx__sens_center_BY_ETA_5___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_6___ <- rx_expr_036 * rx__sens_center_BY_ETA_6___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_7___ <- rx_expr_036 * rx__sens_center_BY_ETA_7___ * rx_expr_029
        rx__sens_rx_r__BY_ETA_8___ <- rx_expr_036 * rx__sens_center_BY_ETA_8___ * rx_expr_029
      }
      if (CMT == 4) {
        rx_pred_ <- effect
        rx__sens_rx_pred__BY_ETA_1___ <- rx__sens_effect_BY_ETA_1___
        rx__sens_rx_pred__BY_ETA_2___ <- rx__sens_effect_BY_ETA_2___
        rx__sens_rx_pred__BY_ETA_3___ <- rx__sens_effect_BY_ETA_3___
        rx__sens_rx_pred__BY_ETA_4___ <- rx__sens_effect_BY_ETA_4___
        rx__sens_rx_pred__BY_ETA_5___ <- rx__sens_effect_BY_ETA_5___
        rx__sens_rx_pred__BY_ETA_6___ <- rx__sens_effect_BY_ETA_6___
        rx__sens_rx_pred__BY_ETA_7___ <- rx__sens_effect_BY_ETA_7___
        rx__sens_rx_pred__BY_ETA_8___ <- rx__sens_effect_BY_ETA_8___
        rx_r_ <- Rx_pow_di(THETA[11], 2)
        rx__sens_rx_r__BY_ETA_1___ <- 0
        rx__sens_rx_r__BY_ETA_2___ <- 0
        rx__sens_rx_r__BY_ETA_3___ <- 0
        rx__sens_rx_r__BY_ETA_4___ <- 0
        rx__sens_rx_r__BY_ETA_5___ <- 0
        rx__sens_rx_r__BY_ETA_6___ <- 0
        rx__sens_rx_r__BY_ETA_7___ <- 0
        rx__sens_rx_r__BY_ETA_8___ <- 0
      }
      cmt(cp)
      cmt(pca)
      dvid(3, 4, 5, 6)
    })

    # context("warfarin inner test")

    data.pkpd <- nlmixr2data::warfarin
    data.pkpd$dvid <- as.integer(data.pkpd$dvid)
    data.pkpd$cmt <- paste(factor(data.pkpd$dvid, c(1, 2), c("center", "effect")))
    data.pkpd$cmt[data.pkpd$amt > 0] <- "depot"

    t2 <- etTrans(data.pkpd[, names(data.pkpd) != "dvid"], inner)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L,
        4L, 4L, 4L, 4L
      )
    )

    data.pkpd$cmt <- paste(factor(data.pkpd$dvid, c(1, 2), c("cp", "pca")))
    data.pkpd$cmt[data.pkpd$amt > 0] <- "depot"

    t2 <- etTrans(data.pkpd[, names(data.pkpd) != "dvid"], inner)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 38L, 37L, 38L, 37L, 38L, 37L,
        38L, 38L, 38L, 38L
      )
    )

    data.pkpd$cmt <- factor(data.pkpd$cmt)

    t2 <- etTrans(data.pkpd[, names(data.pkpd) != "dvid"], inner)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 38L, 37L, 38L, 37L, 38L, 37L,
        38L, 38L, 38L, 38L
      )
    )

    data.pkpd$cmt <- paste(data.pkpd$cmt)
    .cmt <- data.pkpd$cmt
    data.pkpd$cmt <- 1
    data.pkpd$cmt[.cmt == "cp"] <- 5
    data.pkpd$cmt[.cmt == "pca"] <- 6

    ## This skips the sensitivity equations
    t2 <- etTrans(data.pkpd[, names(data.pkpd) != "dvid"], inner)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 38L, 37L, 38L, 37L, 38L, 37L,
        38L, 38L, 38L, 38L
      )
    )

    data.pkpd$cmt <- 1
    data.pkpd$cmt[.cmt == "cp"] <- 3
    data.pkpd$cmt[.cmt == "pca"] <- 4

    t2 <- etTrans(data.pkpd[, names(data.pkpd) != "dvid"], inner)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L,
        4L, 4L, 4L, 4L
      )
    )

    t2 <- etTrans(data.pkpd, inner)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L,
        4L, 4L, 4L, 4L
      )
    )

    data.pkpd$dvid[data.pkpd$dvid == 1] <- 3
    data.pkpd$dvid[data.pkpd$dvid == 2] <- 4

    expect_error(etTrans(data.pkpd, inner))

    t2 <- etTrans(data.pkpd[, names(data.pkpd) != "cmt"], inner, addCmt = TRUE)

    expect_equal(
      t2$CMT[t2$ID == 1],
      c(
        1L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 38L, 37L, 38L, 37L, 38L, 37L,
        38L, 38L, 38L, 38L
      )
    )

    ## TEST NA cmt/dvid w/factors
    ## TEST NA strings for cmt/dvid

    tmp <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp2 <- cp + nlmixr_extra_par
      cmt(central)
      cmt(c20)
    })

    expect_s3_class(tmp, "rxode2")

    # context("Check lhs allowed stateExtra while preserving lhs properties.")

    tmp <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cmt(cp)
    })

    expect_s3_class(tmp, "rxode2")
    expect_equal(tmp$lhs, "cp")
    expect_equal(tmp$stateExtra, "cp")

    tmp <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cmt(cp)
      cp <- center / v
    })

    expect_s3_class(tmp, "rxode2")
    expect_equal(tmp$lhs, "cp")
    expect_equal(tmp$stateExtra, "cp")

    tmp <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cmt(cp)
      cp <- 3
    })

    expect_s3_class(tmp, "rxode2")
    expect_equal(tmp$lhs, "cp")
    expect_equal(tmp$stateExtra, "cp")

    tmp <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- 3
      cmt(cp)
    })

    expect_s3_class(tmp, "rxode2")
    expect_equal(tmp$lhs, "cp")
    expect_equal(tmp$stateExtra, "cp")
  })

  test_that("cmt() and hidden lhs variables", {
    tmp <- rxode2({
      d / dt(depot) ~ -ka * depot
      d / dt(center) ~ ka * depot - cl / v * center
      cp ~ 3
      cmt(cp)
    })

    expect_s3_class(tmp, "rxode2")
    expect_equal(tmp$lhs, character(0))
    expect_equal(tmp$stateExtra, "cp")
  })

  test_that("compartment ordering does not affect c-code generation #785",
  {

    f1 <- function() {
      ini({
        TVCL <- c(0, 0.0308628127403366)
        TVV1 <- c(0, 0.132584062223269)
        TVQ <- c(0, 0.0107718034646091)
        TVV2 <- c(0, 0.0613517707801907)
        TVR0 <- c(0, 3.94111989610973)
        TVKON <- c(0, 1.31095109796496)
        TVKOFF <- c(0, 0.0758072298659602)
        TVKDEG <- c(0, 1.07290072658165)
        TVKINT <- c(0, 3.49422759360926)
        ADD.ERR.PK <- c(0, 0.446231462752061)
        ETA.CL ~ 0.0499998244205031
      })
      model({
        CL <- TVCL * exp(ETA.CL)
        V1 <- TVV1
        Q <- TVQ
        V2 <- TVV2
        R0 <- TVR0
        KON <- TVKON
        KOFF <- TVKOFF
        KDEG <- TVKDEG
        KINT <- TVKINT
        KEL <- CL/V1
        K12 <- Q/V1
        K21 <- Q/V2
        KD <- KOFF/KON
        KSYN <- R0 * KDEG
        A3(0) <- R0
        KEL <- 0
        d/dt(A1) = -(K12 + KEL) * A1 + K21 * A2 - KON * A1 *
          A3 + KOFF * A4 * V1
        d/dt(A2) = K12 * A1 - K21 * A2
        d/dt(A3) = KSYN - KDEG * A3 - KON * (A1/V1) * A3 + KOFF *
          A4
        d/dt(A4) = KON * (A1/V1) * A3 - (KINT + KOFF) * A4
        CFREE = A1/V1
        CTOT = CFREE + A4
        CP = log(CTOT)
        CP ~ add(ADD.ERR.PK)
        SPEC.OUT <- SPEC
        DGRP.OUT <- DGRP
        BW.OUT <- BW
      })
    }

    f1 <- f1 |> zeroRe("omega")

    f2 <- function() {
      ini({
        TVCL <- c(0, 0.0308628127403366)
        TVV1 <- c(0, 0.132584062223269)
        TVQ <- c(0, 0.0107718034646091)
        TVV2 <- c(0, 0.0613517707801907)
        TVR0 <- c(0, 3.94111989610973)
        TVKON <- c(0, 1.31095109796496)
        TVKOFF <- c(0, 0.0758072298659602)
        TVKDEG <- c(0, 1.07290072658165)
        TVKINT <- c(0, 3.49422759360926)
        ADD.ERR.PK <- c(0, 0.446231462752061)
        ETA.CL ~ 0.0499998244205031
      })
      model({
        cmt(A1)
        cmt(A2)
        cmt(A3)
        cmt(A4)
        CL <- TVCL * exp(ETA.CL)
        V1 <- TVV1
        Q <- TVQ
        V2 <- TVV2
        R0 <- TVR0
        KON <- TVKON
        KOFF <- TVKOFF
        KDEG <- TVKDEG
        KINT <- TVKINT
        KEL <- CL/V1
        K12 <- Q/V1
        K21 <- Q/V2
        KD <- KOFF/KON
        KSYN <- R0 * KDEG
        A3(0) <- R0
        KEL <- 0
        d/dt(A1) = -(K12 + KEL) * A1 + K21 * A2 - KON * A1 *
          A3 + KOFF * A4 * V1
        d/dt(A2) = K12 * A1 - K21 * A2
        d/dt(A3) = KSYN - KDEG * A3 - KON * (A1/V1) * A3 + KOFF *
          A4
        d/dt(A4) = KON * (A1/V1) * A3 - (KINT + KOFF) * A4
        CFREE = A1/V1
        CTOT = CFREE + A4
        CP = log(CTOT)
        CP ~ add(ADD.ERR.PK)
        SPEC.OUT <- SPEC
        DGRP.OUT <- DGRP
        BW.OUT <- BW
      })
    }

    f2 <- f2 |> zeroRe("omega")

    ev     <- eventTable()
    mybw   <- 70
    mymw   <- 150000
    mydgrp <- 50
    ev$add.dosing(dose=mydgrp*mybw*1000000/mymw ,nbr.doses= 13, dosing.interval= 7)
    ev$add.sampling(seq(0,13*7,length.out=7))

    ev2 <- ev |> as.data.frame() |>
      dplyr::mutate(BW=mybw, DGRP=mydgrp, SPEC=4)

    s2 <- rxSolve(f2, ev2, returnType="data.frame")

    s1 <- rxSolve(f1, ev2, returnType="data.frame")

    expect_equal(s1[,c("A1", "A2", "A3", "A4")],
                 s2[,c("A1", "A2", "A3", "A4")])

  })

})
