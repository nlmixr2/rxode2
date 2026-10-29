rxTest({
  test_that("Absolute Working Directory + Model Name; Issue RxODE#56", {
    skip_on_os("windows")
    skip_on_os("solaris")
    # a directory of its own, so tests that run at the same time on one machine
    # do not build into (and delete) the same path
    .wd <- tempfile("issue56-")
    dir.create(.wd)
    withr::defer(unlink(.wd, recursive = TRUE))
    expect_true(startsWith(.wd, "/"))
    ode <- " d/dt(test) = 0; "
    m1 <- rxode2(model = ode, modName = "m1", wd = .wd)
    expect_true(dir.exists(file.path(.wd, "m1.d")))
    rxDelete(m1)
  })
})
