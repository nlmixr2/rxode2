rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    test_that("infusion + bolus works correctly", {

      d <- test_path("test-infusion-bolus.qs")
      skip_if_not(file.exists(d))
      dataset <- qs::qread(d)

      base_model <- function() {
        ini({
          POP_CL <- c(0.0, 3.84718, Inf)
          POP_VC <- c(0.0, 19.4133, Inf)
          POP_QP1 <- c(0.0, 3.84718, 999999.0)
          POP_VP1 <- c(0.0, 0.970665, 999999.0)
          ETA_1 + ETA_2 + ETA_3 + ETA_4 ~ c(0.145644, 0.0211085, 0.0202481, 0.011449, 0.0042689, 0.09, 0.011449, 0.0042689, 0.009, 0.09)
          sigma <- 0.0888837
        })
        model({
          VP1 <- POP_VP1*exp(ETA_4)
          QP1 <- POP_QP1*exp(ETA_3)
          CL <- POP_CL*exp(ETA_1)
          VC <- POP_VC*exp(ETA_2)
          V1 <- VC
          Q <- QP1
          V2 <- VP1
          d/dt(A_CENTRAL) = Q*A_PERIPHERAL/V2 + (-CL/V1 - Q/V1)*A_CENTRAL
          d/dt(A_PERIPHERAL) = -Q*A_PERIPHERAL/V2 + Q*A_CENTRAL/V1
          Fv <- A_CENTRAL
          IPRED <- A_CENTRAL/VC
          if (IPRED == 0) {
            IPREDADJ <- 2.22500000000000E-16
          } else {
            IPREDADJ <- IPRED
          }
          Y <- IPREDADJ
          Y ~ prop(sigma)
        })
      }

      f <- base_model()

      etTrans(dataset, f$simulationModel)


      s <- rxSolve(f, dataset, addDosing = TRUE)
      expect_true(any(names(s) == "rate"))
      expect_equal(names(s)[2], "resetno")

    })
  }
})
