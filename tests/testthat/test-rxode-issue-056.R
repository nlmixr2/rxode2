rxTest({
  test_that("Absolute Working Directory + Model Name; Issue RxODE#56", {
    skip_on_os("windows")
    skip_on_os("solaris")
    if (!dir.exists("/tmp/")) {
      skip()
    }
    ode <- " d/dt(test) = 0; "
    m1 <- rxode2(model = ode, modName = "m1", wd = "/tmp")
    expect_true(dir.exists("/tmp/m1.d"))
    rxDelete(m1)
    unlink("/tmp/m1.d", recursive = TRUE)
  })
}) 
