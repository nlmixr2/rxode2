rxode2Test(
  {
    context("Issue 299")
    test_that("issue #229", {
      model <- rxode2({
        cmt(SC_AKR)
        cmt(Cent_AKR)
        rx_expr_0 ~ exp(THETA[1])
        d / dt(SC_AKR) <- -rx_expr_0 * SC_AKR
        rx_expr_2 ~ THETA[2] - THETA[3]
        rx_expr_4 ~ exp(rx_expr_2)
        d / dt(Cent_AKR) <- rx_expr_0 * SC_AKR - rx_expr_4 * Cent_AKR
        f(SC_AKR) <- THETA[4]
        rx_yj_ ~ 2
        rx_lambda_ ~ 1
        rx_hi_ ~ 1
        rx_low_ ~ 0
        rx_expr_1 ~ exp(-THETA[3])
        rx_expr_3 ~ 1000 * rx_expr_1
        rx_expr_5 ~ rx_expr_3 * Cent_AKR
        rx_pred_ <- rx_expr_5
        rx_r_ <- 1e+06 * Rx_pow_di((rx_expr_1 * Cent_AKR), 2) * Rx_pow_di(
          THETA[5],
          2
        ) + Rx_pow_di(THETA[6], 2)
        lKa <- THETA[1]
        lCL <- THETA[2]
        lVc <- THETA[3]
        F_SC <- THETA[4]
        prop_err <- THETA[5]
        add_err <- THETA[6]
        Ka <- rx_expr_0
        CL <- exp(THETA[2])
        Vc <- exp(THETA[3])
        kel <- rx_expr_4
        Cp_AKR <- rx_expr_5
        cmt(Cp_AKR)
        tad <- tad()
        dosenum <- dosenum()
      })

      parm <- setNames(
        c(-3.54456675092961, -2.30258509299405, 1.09861228866811, 0.3, 0.2, 10),
        paste0("THETA[", 1:6, "]")
      )

      d_mod <- readRDS(test_path("test-issue-299.rds"))

      s <- suppressWarnings(rxSolve(model, parm, d_mod))

      expect_true(all(diff(order(s$id, s$time)) == 1))


      d2 <- d_mod[d_mod$ID == 43, ]

      s <- suppressWarnings(rxSolve(model, parm, d2, addDosing = NA))
    })
  },
  test = "lvl2"
)
