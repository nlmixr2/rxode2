rxTest({
  test_that("RxODE#398", {
    filename <- test_path("test-issue-398.qs")
    skip_if_not(file.exists(filename))

    NM_data <- qs::qread(filename)

    # Define and Compile model
    mod1 <- rxode2({
      CLH_int <- THETA[1]
      CLD <- THETA[2]
      V_centr <- THETA[3]
      V_peri <- THETA[4]
      fup <- 0.28 #
      RBP <- 1
      CLR <- 0
      ka <- 10
      FQ_LV <- 1.26
      BW <- 70 # kg
      Qh <- FQ_LV * BW
      CLH <- Qh * CLH_int * fup / (Qh + CLH_int * fup / RBP)
      Fh <- 1 - CLH / RBP / Qh
      FaFg <- 0.82
      C_centr <- A_centr / V_centr #           venous (VE) total  venous blood
      C_peri <- A_peri / V_peri #  			arterial (AR) total  arterial blood
      d / dt(A_centr) <- -C_centr * (CLR + CLH) + C_peri * CLD - C_centr * CLD + ka * A_absorption # 	central
      d / dt(A_peri) <- -C_peri * CLD + C_centr * CLD # peripheral
      d / dt(A_absorption) <- -ka * A_absorption
      f(A_absorption) <- Fh * FaFg
      dur(A_absorption) <- THETA[5]
      DV <- DV # copying data columns from data set for fitting
      MDV <- MDV
      PRED <- C_centr
    })

    # simulation
    THETA <- c(10, 2.619, 10, 22.14, 5)
    suppressWarnings(
      expect_error(solve(mod1, NM_data, params = setNames(THETA, paste0("THETA[", 1:5, "]"))), NA)
    )
  })
})
