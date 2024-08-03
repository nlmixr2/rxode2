rxTest({
  test_that("modelvars", {
    skip_on_cran()
    rxClean()
    rigid.txt <- "
y1(0)    = 1
y2(0)    = 0
y3(0)    = 0.9
a1       = -2
a2       = 1.25
a3       = -0.5
d/dt(y1) = a1*y2*y3
d/dt(y2) = a2*y1*y3
d/dt(y3) = a3*y1*y2
"
    rigid0 <- rxGetModel(rigid.txt)

    rigid <- rxode2(rigid.txt)

    et <- eventTable()
    et$add.sampling(seq(0, 20, by = 0.01))

    out <- solve(rigid, et)

    expect_equal(rxModelVars(rigid), rxModelVars(rigid$cmpMgr$rxDll()))
    expect_equal(rxModelVars(rigid), rxModelVars(out))
    # Drop the 'timeId' element because it is the compilation time, and it may
    # differ if the system is heavily loaded
    rigid0_compare <- rigid0[setdiff(names(rigid0), "timeId")]
    rigid_compare <- rxModelVars(rigid)
    rigid_compare <- rigid_compare[setdiff(names(rigid_compare), "timeId")]
    expect_equal(rigid0_compare, rigid_compare)
  })

  test_that("blank modelvars has same size", {
    blank <- rxModelVars("")
    full <-  rxModelVars("y = 1")
    expect_equal(length(rxModelVars(blank)), length(full))
  })
})
