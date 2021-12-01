rxode2Test(
  {
    if (file.exists("test-issue-393.qs")) {
      d <- qs::qread("test-issue-393.qs")

      mod1 <- rxode2({
        #  CLH = THETA[1];
        #  CLD = THETA[2];
        #  V_centr = THETA[3];
        #  V_peri = THETA[4];
        CLR <- 0
        ka <- 0
        FQ_LV <- 2.604
        BW <- 6.2 # kg
        Qh <- FQ_LV * BW
        RBP <- 0.679
        Fh <- 1 - CLH / RBP / Qh
        FaFg <- 1
        C_centr <- A_centr / V_centr  #           venous (VE) total  venous blood
        C_peri <- A_peri / V_peri  #  			arterial (AR) total  arterial blood
        d / dt(A_centr) <- -C_centr * (CLR + CLH) + C_peri * CLD - C_centr * CLD + ka * A_absorption  # 	central
        d / dt(A_peri) <- -C_peri * CLD + C_centr * CLD  # peripheral
        d / dt(A_absorption) <- -ka * A_absorption
        f(A_absorption) <- Fh * FaFg
      })

      mod2 <- rxode2({
        CLH <- THETA[1]
        CLD <- THETA[2]
        V_centr <- THETA[3]
        V_peri <- THETA[4]
        CLR <- 0
        ka <- 0
        FQ_LV <- 2.604
        BW <- 6.2 # kg
        Qh <- FQ_LV * BW
        RBP <- 0.679
        Fh <- 1 - CLH / RBP / Qh
        FaFg <- 1
        C_centr <- A_centr / V_centr  #           venous (VE) total  venous blood
        C_peri <- A_peri / V_peri  #  			arterial (AR) total  arterial blood
        d / dt(A_centr) <- -C_centr * (CLR + CLH) + C_peri * CLD - C_centr * CLD + ka * A_absorption  # 	central
        d / dt(A_peri) <- -C_peri * CLD + C_centr * CLD  # peripheral
        d / dt(A_absorption) <- -ka * A_absorption
        f(A_absorption) <- Fh * FaFg
      })


      s1 <- rxSolve(mod1, d, params = c(CLH = 2, CLD = 10, V_centr = 77, V_peri = 10), returnType = "data.frame")
      s2 <- rxSolve(mod2, d, params = setNames(c(2, 10, 77, 10), paste0("THETA[", 1:4, "]")), returnType = "data.frame")
      s2 <- s2[, names(s1)]
      expect_equal(s1, s2)

      s3 <- rxSolve(mod2, d,
        params = c(c(CLR = 0, ka = 0, FQ_LV = 2.604, BW = 6.2, RBP = 0.679, FaFg = 1), setNames(c(2, 10, 77, 10), paste0("THETA[", 1:4, "]"))),
        returnType = "data.frame"
      )

      s3 <- s3[, names(s1)]
      expect_equal(s1, s3)

      s4 <- rxSolve(mod2, d, theta = c(2, 10, 77, 10), returnType = "data.frame")

      s4 <- s4[, names(s1)]
      expect_equal(s1, s3)

      s5 <- rxSolve(mod2, d, theta = c(2, 10, 77, 10))

      s6 <- rxSolve(mod2, d, params = setNames(c(2, 10, 77, 10), paste0("THETA[", 1:4, "]")))
    }
  },
  test = "lvl2"
)
