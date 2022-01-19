rxode2Test(
  {
    # Individual keep AGE==AGE2

    TVQ <- 4
    TVV3 <- 7
    dat <- data.frame(AGE = c(20, 30), ID = c(10, 20)) %>%
      dplyr::mutate(id = seq(from = 1, to = dplyr::n())) %>%
      dplyr::rename(NMID = ID)


    par.tab <- data.frame(
      ThetaKa = c(0.7, 0.9),
      ThetaCl = c(4.6, 5.4),
      ThetaV2 = c(7, 8),
      ID = c(10, 20)
    ) %>%
      dplyr::mutate(id = seq(from = 1, to = dplyr::n())) %>%
      dplyr::mutate(ThetaQ = TVQ, ThetaV3 = TVV3) %>%
      dplyr::rename(NMID = ID)

    tabtot <- dat %>%
      dplyr::left_join(., par.tab, by = c("NMID", "id"))

    mod1 <- rxode2({
      ## PK parameters
      CL <- exp(ThetaCl)
      KA <- exp(ThetaKa)
      V2 <- exp(ThetaV2)
      Q <- exp(ThetaQ)
      V3 <- exp(ThetaV3)

      K20 <- CL / V2
      K23 <- Q / V2
      K32 <- Q / V3

      CP <- A2 / V2

      ##
      d / dt(A1) <- -KA * A1
      d / dt(A2) <- KA * transit3 - K23 * A2 + K32 * A3 - K20 * A2
      d / dt(A3) <- K23 * A2 - K32 * A3

      d / dt(transit1) <- KA * A1 - KA * transit1
      d / dt(transit2) <- KA * transit1 - KA * transit2
      d / dt(transit3) <- KA * transit2 - KA * transit3

      f(A1) <- 1

      d / dt(AUC) <- CP
      A1(0) <- 0
      A2(0) <- 0
      A3(0) <- 0

      AGE2 <- AGE
    })


    NSubj <- length(tabtot$id)

    dose_ref <- 8000 ##

    ev_ref <- eventTable() %>%
      et(dose = dose_ref / 1000, time = seq(0, 24, 24)) %>%
      et(id = 1:NSubj) %>%
      add.sampling(seq(0, 24, 1)) %>%
      dplyr::mutate(DOSE = dose_ref) %>%
      dplyr::group_by(id) %>%
      tidyr::fill(DOSE, .direction = "downup") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Cycle = dplyr::case_when(
        time <= 12 ~ 1, #
        time >= 12 ~ 2, #
        TRUE ~ 0
      )) %>%
      dplyr::as_tibble()

    ev_ref <- ev_ref %>%
      dplyr::left_join(., tabtot, by = "id") %>%
      dplyr::as_tibble()

    PK.ev_ref2 <- rxSolve(mod1,
      events = ev_ref, cores = 2,
      seed = 123, addCov = TRUE, keep = c("Cycle", "AGE")
    )

    expect_equal(PK.ev_ref2$AGE, PK.ev_ref2$AGE2)
  },
  test = "cran"
)
