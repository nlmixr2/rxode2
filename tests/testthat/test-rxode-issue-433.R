rxTest({
  test_that("CMT translation matches input", {
    filename <- test_path("433.qs")
    skip_if_not(file.exists(filename))

    lst <- qs2::qs_read(filename)

    rx <- rxode2({
      cmt(parent)
      cmt(m1)
      parent(0) <- ETA[1] + THETA[1]
      d / dt(parent) <- -exp(ETA[2] + THETA[2]) * parent
      d / dt(m1) <- -exp(ETA[3] + THETA[3]) * m1 + 1 * exp(ETA[2] +
                                                             THETA[2]) * parent / (1 + exp(-(ETA[4] + THETA[4])))
      rx_yj_ ~ 2 * (1 - (CMT == 2)) * (CMT == 1) +
        2 * (CMT == 2)
      rx_lambda_ ~ 1 * (1 - (CMT == 2)) * (CMT == 1) + 1 * (CMT == 2)
      rx_hi_ ~ 1 * (1 - (CMT == 2)) * (CMT == 1) + 1 * (CMT == 2)
      rx_low_ ~ 0
      rx_pred_ <- (m1 * (CMT == 2) + parent * (1 - (CMT == 2)) *
                     (CMT == 1)) * (CMT == 2) + (m1 * (CMT == 2) +
                                                   parent * (1 - (CMT == 2)) * (CMT == 1)) * (1 - (CMT == 2)) * (CMT == 1)
      rx_r_ <- (Rx_pow_di(((m1 * (CMT == 2) + parent * (1 - (CMT == 2)) * (CMT == 1)) * (CMT == 2) + (m1 * (CMT == 2) + parent *
                                                                                                        (1 - (CMT == 2)) * (CMT == 1)) * (1 - (CMT == 2)) * (CMT == 1)), 2) * Rx_pow_di(THETA[8], 2) +
                  Rx_pow_di(THETA[7], 2)) * (CMT == 2) + (Rx_pow_di(((m1 * (CMT == 2) + parent * (1 - (CMT == 2)) * (CMT == 1)) * (CMT == 1)), 2) *
                                                            Rx_pow_di(THETA[6], 2) + Rx_pow_di(THETA[5], 2)) * (1 - (CMT == 2)) * (CMT == 1)
      parent_0 <- THETA[1]
      log_k_parent <- THETA[2]
      log_k_m1 <- THETA[3]
      f_parent_qlogis <- THETA[4]
      sigma_low_parent <- THETA[5]
      rsd_high_parent <- THETA[6]
      sigma_low_m1 <- THETA[7]
      rsd_high_m1 <- THETA[8]
      eta.parent_0 <- ETA[1]
      eta.log_k_parent <- ETA[2]
      eta.log_k_m1 <- ETA[3]
      eta.f_parent_qlogis <- ETA[4]
      parent_0_model <- ETA[1] + THETA[1]
      k_parent <- exp(ETA[2] + THETA[2])
      k_m1 <- exp(ETA[3] + THETA[3])
      f_parent_to_m1 <- 1 / (1 + exp(-(ETA[4] + THETA[4])))
      tad <- tad()
      dosenum <- dosenum()
    })

    lst <- c(list(rx), lst)

    s <- do.call(rxSolve, lst)

    expect_equal(
      subset(lst[[3]], time == 0 & CMT == 2, "dv"),
      subset(s, time == 0 & CMT == 2, "dv")
    )
  })
})
