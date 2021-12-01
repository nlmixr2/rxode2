rxode2Test(
  {
    test_that("issue 410", {
      mod <- rxode2("cmt(EFFECT);\nd/dt(EFFECT)=0;\nd/dt(rx__sens_EFFECT_BY_ETA_1___)=0;\nrx_yj_~2;\nrx_lambda_~1;\nrx_hi_~1;\nrx_low_~0;\nrx_expr_0~exp(THETA[4]);\nrx_expr_1~ETA[1]+THETA[3];\nrx_expr_2~SEX==\"F\";\nrx_expr_3~SEX==\"M\";\nrx_expr_4~exp(rx_expr_1);\nrx_expr_5~(rx_expr_2)*THETA[2];\nrx_expr_6~(rx_expr_3)*THETA[1];\nrx_expr_7~rx_expr_5+rx_expr_6;\nrx_expr_8~exp(rx_expr_7);\nrx_expr_9~rx_expr_4-rx_expr_8;\nrx_expr_10~(-2+NTS_birth)*(rx_expr_9);\nrx_expr_11~rx_expr_10/(-2+NTS_birth+rx_expr_0);\nrx_pred_=rx_expr_11+rx_expr_8;\nrx__sens_rx_pred__BY_ETA_1___=rx_expr_4*(-2+NTS_birth)/(-2+NTS_birth+rx_expr_0);\nrx_r_=Rx_pow_di((THETA[6]+THETA[5]*(rx_expr_11+rx_expr_8)),2);\nrx__sens_rx_r__BY_ETA_1___=2*rx_expr_4*(-2+NTS_birth)*(THETA[6]+THETA[5]*(rx_expr_11+rx_expr_8))*THETA[5]/(-2+NTS_birth+rx_expr_0);sexf=(SEX==\"F\");\nsexm=(SEX==\"M\");\ncmt(effect);\n")


      est <- c("THETA[1]" = 7.3132203870903, "THETA[2]" = 7.60090245954208, "THETA[3]" = 6.90775527898214, "THETA[4]" = 0.693147180559945, "THETA[5]" = 0.2, "THETA[6]" = 1, "ETA[1]" = 0)

      d <-
        data.frame(
          ID = rep(1:2, each = 2),
          CMT = "EFFECT",
          DV = 1000 + rnorm(4),
          NTS_birth = rep(2:3, 2),
          SEX = rep(c("F", "M"), each = 2),
          TIME = 1
        )

      e <- etTrans(d, mod)

      expect_error(rxSolve(mod, est, d), NA)

      s <- rxSolve(mod, est, d, addCov = TRUE)
      expect_true(inherits(s$SEX, "factor"))

      expect_equal(paste(d$SEX), paste(s$SEX))

      expect_equal((d$SEX == "F") * 1.0, s$sexf)

      expect_equal((d$SEX == "M") * 1.0, s$sexm)
    })
  },
  test = "lvl2"
)
