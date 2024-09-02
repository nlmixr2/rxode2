test_that("test warning/solve for out of bounds", {

  p <- test_path("test-warning-solve.qs")

  skip_if_not(file.exists(p))

  m <- rxode2({
    param(Kpm_pop, V_pop, k_pop, k12_pop, k21_pop, ka_pop,
          km_pop, a1_Cp, b1_Cp, a2_Cm, b2_Cm, omega_Kpm, omega_V,
          omega_k, omega_k12, omega_k21, omega_ka, omega_km)
    cmt(depot)
    cmt(central)
    cmt(cmt2)
    cmt(cmt3)
    Kpm = exp(Kpm_pop + omega_Kpm)
    V = exp(V_pop + omega_V)
    k = exp(k_pop + omega_k)
    k12 = exp(k12_pop + omega_k12)
    k21 = exp(k21_pop + omega_k21)
    ka = exp(ka_pop + omega_ka)
    km = exp(km_pop + omega_km)
    d/dt(depot) = -ka * depot
    d/dt(central) = -k12 * central + k21 * cmt2 + ka * depot -
      k * central - Kpm * central
    Cp = central/V
    d/dt(cmt2) = +k12 * central - k21 * cmt2
    d/dt(cmt3) = +Kpm * central - km * cmt3
    Cm = cmt3/V
    y1_Cp = Cp
    if (CMT == 5) {
      rx_yj_ ~ 2
      rx_lambda_ ~ 1
      rx_low_ ~ 0
      rx_hi_ ~ 1
      rx_pred_f_ ~ y1_Cp
      rx_pred_ ~ rx_pred_f_
      rx_r_ ~ ((a1_Cp) + (rx_pred_f_) * (b1_Cp))^2
      ipredSim = rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_,
                        rx_hi_)
      sim = rxTBSi(rx_pred_ + sqrt(rx_r_) * rxerr.y1_Cp,
                   rx_lambda_, rx_yj_, rx_low_, rx_hi_)
    }
    y2_Cm = Cm
    if (CMT == 6) {
      rx_yj_ ~ 2
      rx_lambda_ ~ 1
      rx_low_ ~ 0
      rx_hi_ ~ 1
      rx_pred_f_ ~ y2_Cm
      rx_pred_ ~ rx_pred_f_
      rx_r_ ~ ((a2_Cm) + (rx_pred_f_) * (b2_Cm))^2
      ipredSim = rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_,
                        rx_hi_)
      sim = rxTBSi(rx_pred_ + sqrt(rx_r_) * rxerr.y2_Cm,
                   rx_lambda_, rx_yj_, rx_low_, rx_hi_)
    }
    iwres = (DV - rx_pred_)/sqrt(rx_r_)
    ires = DV - rx_pred_
    cmt(y1_Cp)
    cmt(y2_Cm)
    dvid(5, 6)
  })

  expect_warning(do.call(rxSolve, c(list(m), qs::qread(p))), NA)

})
