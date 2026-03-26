rxTest({
  # This test is for issue #999
  library(rxode2)

  # 3-compartment TMDD model (matches the pharmacokinetic simulation in this project)
  model <- rxode2({
    Kel     <- CL / Vcentral
    K12     <- Qcp1 / Vcentral
    K21     <- Qcp1 / Vperiph1
    K13     <- Qcp2 / Vcentral
    K31     <- Qcp2 / Vperiph2
    KelTMDD <- CLTMDD / Vcentral * CENT / Vcentral / (C50TMDD + CENT / Vcentral)
    d/dt(GUT)  <- -Ka * GUT
    d/dt(CENT) <- Ka * GUT - (Kel + KelTMDD + K12 + K13) * CENT + K21 * P1 + K31 * P2
    d/dt(P1)   <- K12 * CENT - K21 * P1
    d/dt(P2)   <- K13 * CENT - K31 * P2
    CP_noerror <- CENT / Vcentral
  })

  make_dataset <- function(n_subjects) {
    obs_times <- c(0, 0.5, 1, 2, 3, 4, 8, 12, 16, 24, 36, 48)

    doses <- data.frame(
      id   = seq_len(n_subjects),
      time = 0,
      evid = 1,
      amt  = 1,
      cmt  = "CENT",  # IV bolus
      rate = 0
    )

    obs <- expand.grid(id = seq_len(n_subjects), time = obs_times)
    obs$evid <- 0; obs$amt <- 0; obs$cmt <- "CENT"; obs$rate <- 0

    events <- rbind(doses, obs)
    events <- events[order(events$id, events$time, -events$evid), ]

    # Vary parameters across subjects to match simulation diversity.
    # CLTMDD is set to a non-zero fraction of CL to activate the nonlinear
    # Michaelis-Menten TMDD elimination term, which is required to trigger
    # the segfault. Setting CLTMDD = 0 does NOT reproduce the crash.
    set.seed(42)
    cl_vals <- sample(c(0.25, 1, 4), n_subjects, replace = TRUE)
    params <- data.frame(
      id       = seq_len(n_subjects),
      Ka       = 0,
      CL       = cl_vals,
      CLTMDD   = cl_vals * sample(c(0.25, 1, 4), n_subjects, replace = TRUE),
      C50TMDD  = 1,
      Vcentral = 1,
      Vperiph1 = sample(c(0.25, 1, 4), n_subjects, replace = TRUE),
      Vperiph2 = sample(c(0.25, 1, 4), n_subjects, replace = TRUE),
      Qcp1     = sample(c(0.25, 1, 4), n_subjects, replace = TRUE),
      Qcp2     = sample(c(0.25, 1, 4), n_subjects, replace = TRUE)
    )

    list(events = events, params = params)
  }

  test_that("rxSolve handles large datasets without crashing", {
    for (n_sub in c(4000, 8000, 12000, 16000, 20000, 26208)) {
      dat <- make_dataset(n_sub)
      tryCatch({
        result <- expect_error(rxSolve(model, params = dat$params, events = dat$events,
                             atol = 1e-50, rtol = 1e-8), NA)
        rm(result); gc()
      }, error = function(e) {
        cat(n_sub, "subjects: ERROR -", conditionMessage(e), "\n")
      })
    }
  })
})
