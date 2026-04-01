rxTest({
  mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

  et <- eventTable()
  et$add.sampling(seq(0, 10, by = 1 / 24))
  et$add.dosing(
    dose = 2 / 24, rate = 2, start.time = 0,
    nbr.doses = 10, dosing.interval = 1
  )
  et <- et |> et(0.05, evid = 2)

  s1 <- solve(mod, et, addDosing = FALSE)

  s2 <- solve(mod, et, addDosing = NULL)

  s3 <- solve(mod, et, addDosing = NA)

  s4 <- solve(mod, et, addDosing = TRUE)


  s1tbs <- solve(mod, et, addDosing = FALSE, returnType = "data.frame.TBS")

  s2tbs <- solve(mod, et, addDosing = NULL, returnType = "data.frame.TBS")

  s3tbs <- solve(mod, et, addDosing = NA, returnType = "data.frame.TBS")

  s4tbs <- solve(mod, et, addDosing = TRUE, returnType = "data.frame.TBS")

  test_that("evid is included with evid=2", {
    expect_true("evid" %in% names(s1))
    expect_false("evid" %in% names(s2))
    expect_true("evid" %in% names(s3))
    expect_true("evid" %in% names(s4))
    expect_true("evid" %in% names(s1tbs))
    expect_false("evid" %in% names(s2tbs))
    expect_true("evid" %in% names(s3tbs))
    expect_true("evid" %in% names(s4tbs))
  })

  test_that("Includes and ignores EVID=2", {
    expect_true(any(s1$time == 0.05))
    expect_true(any(s1tbs$time == 0.05))
    expect_false(any(s2$time == 0.05))
    expect_false(any(s2tbs$time == 0.05))
  })

  test_that("addDosing=TRUE does not add an extra row", {
    expect_equal(sort(unique(s4$evid)), c(0L, 1L, 2L))
    expect_equal(sort(unique(s4tbs$evid)), c(0L, 1L, 2L))
  })

  ## Test mixed solved and ODEs
  mod2 <- rxode2({
    ## the order of variables do not matter, the type of compartmental
    ## model is determined by the parameters specified.
    CL ~ TCL * exp(eta.Cl)
    C2 ~ linCmt(KA, CL, V2, Q, V3)
    eff(0) <- 1 ## This specifies that the effect compartment starts at 1.
    d / dt(eff) ~ Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    ##
    resp <- eff + err1
    pk <- C2 * exp(err2)
  })

  sigma <- diag(2) * 0.05
  dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))


  ev <- eventTable() |>
    add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) |>
    add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) |>
    add.sampling(0:240)

  ev <- ev |> et(0.5, evid = 2)

  pk4 <- rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, events = ev, sigma = sigma, cores = 1, addDosing = TRUE
  )

  pk5 <- rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, events = ev, sigma = sigma, cores = 1, addDosing = NULL
  )

  test_that("Multi-subject solves keep evid=2", {
    expect_true(any(pk4$time == 0.5))
    expect_false(any(pk5$time == 0.5))
  })
})
