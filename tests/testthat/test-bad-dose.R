rxTest({
  # https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf p11
  mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

  test_that("Warning for bad dose", {
    et <- eventTable(time.units = "days") %>%
      add.sampling(seq(0, 10, by = 1 / 24)) %>%
      add.dosing(dose = 2 / 24, rate = 2, start.time = 0, nbr.doses = 10, dosing.interval = 1, dosing.to = 3)
    
    expect_warning(solve(mod, et), rex::rex("dose to compartment 3 ignored (not in system; 'id=1')"))
  })

  test_that("No Warning for good dose", {
    et <- eventTable(time.units = "days") %>%
      add.sampling(seq(0, 10, by = 1 / 24)) %>%
      add.dosing(dose = 2 / 24, rate = 2, start.time = 0, nbr.doses = 10, dosing.interval = 1, dosing.to = 1)
    
    expect_warning(solve(mod, et), NA)
  })
})
