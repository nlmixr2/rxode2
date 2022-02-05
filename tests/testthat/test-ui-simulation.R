test_that("ordinal simulation", {

  f <- function() {
    ini({
      # Baseline cumulative effect
      b0_5 <- 8.79
      b1_0 <- -1.47
      b1_5 <- -1.03
      b2_0 <- -0.85
      b2_5 <- -1.88
      b3_0 <- -2.88
      b3_5 <- -3.02
      b4_0 <- -1.14

      # Vehicle Differential odds estimate
      v1_0 <- fix(10)
      v1_5 <- fix(10)
      v2_0 <- fix(10)
      v2_5 <- 2.3
      v3_0 <- 1.07
      v3_5 <- fix(10)
      v4_0 <- fix(10)

      # Drug Differential odds estimate
      d1_0 <- fix(10)
      d1_5 <- fix(10)
      d2_0 <- 3.64
      d2_5 <- 1.56
      d3_0 <- fix(10)
      d3_5 <- fix(-10)
      d4_0 <- fix(10)

      # Vehicle parameters
      Vb <- -4.98
      Vm <- 0.06

      t12 <- 18.46
      ec50 <- 0.03
      emax <- 3.4

      eta ~ 1.3^2

      add.sd <- 0.01

    })
    model({

      kel <- log(2) / t12
      d/dt(kpd) <- -kel * kpd

      dEff <- emax * kpd / (ec50 + kpd)

      dp0_5 <- 1
      dp1_0 <- d1_0 * dp0_5
      dp1_5 <- d1_5 * dp1_0
      dp2_0 <- d2_0 * dp1_5
      dp2_5 <- d2_5 * dp2_0
      dp3_0 <- d3_0 * dp2_5
      dp3_5 <- d3_5 * dp3_0
      dp4_0 <- d4_0 * dp3_5

      dVeh <- Vi * (Vb + Vm * time)

      vp0_5 <- 1
      vp1_0 <- v1_0 * vp0_5
      vp1_5 <- v1_5 * vp1_0
      vp2_0 <- v2_0 * vp1_5
      vp2_5 <- v2_5 * vp2_0
      vp3_0 <- v3_0 * vp2_5
      vp3_5 <- v3_5 * vp3_0
      vp4_0 <- v4_0 * vp3_5

      as0_5 <- b0_5
      as1_0 <- b1_0 + as0_5
      as1_5 <- b1_5 + as1_0
      as2_0 <- b2_0 + as1_5
      as2_5 <- b2_5 + as2_0
      as3_0 <- b3_0 + as2_5
      as3_5 <- b3_5 + as3_0
      as4_0 <- b4_0 + as3_5

      f0_5 <- logit(dEff * dp0_5 + dVeh * vp0_5 + as0_5 + eta)
      f1_0 <- logit(dEff * d1_0 + dVeh * vp1_0 + as1_0 + eta)
      f1_5 <- logit(dEff * dp1_5 + dVeh * vp1_5 + as1_5 + eta)
      f2_0 <- logit(dEff * dp2_0 + dVeh * vp2_0 + as2_0 + eta)
      f2_5 <- logit(dEff * dp2_5 + dVeh * vp2_5 + as2_5 + eta)
      f3_0 <- logit(dEff * dp3_0 + dVeh * vp3_0 + as3_0 + eta)
      f3_5 <- logit(dEff * dp3_5 + dVeh * vp3_5 + as3_5 + eta)
      f4_0 <- logit(dEff * dp4_0 + dVeh * vp4_0 + as4_0 + eta)

      # 0 is probability otherwise
      p0_0 <- 1    - f0_5 # probability of 0 score = probability < 0.5
      p0_5 <- f0_5 - f1_0 # probability of 0.5  score = probability < 1.0 - probability(0)
      p1_0 <- f1_0 - f1_5
      p1_5 <- f1_5 - f1_0
      p2_0 <- f2_0 - f1_5
      p2_5 <- f2_5 - f2_0
      p3_0 <- f3_0 - f2_5
      p3_5 <- f3_5 - f3_0

      kpd ~ lnorm(add.sd)

      cac ~ c(p0_0, p0_5, p1_0, p1_5, p2_0, p2_5, p3_0, p3_5)
    })
  }

  tmp <- rxode2(f)

  expect_equal(tmp$covariates, "Vi")
  expect_error(tmp$simulationModel, NA)

  # need to check the probabilities

})
