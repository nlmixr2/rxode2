rxode2Test(
  {
    context("Test modelvars")
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

    test_that("modelvars", {
      expect_equal(rxModelVars(rigid), rxModelVars(rigid$cmpMgr$rxDll()))
      expect_equal(rxModelVars(rigid), rxModelVars(out))
      expect_equal(rigid0$trans, rxModelVars(rigid)$trans)
      expect_equal(rigid0$lhs, rxModelVars(rigid)$lhs)
      expect_equal(rigid0$ini, rxModelVars(rigid)$ini)
      expect_equal(rigid0$model, rxModelVars(rigid)$model)
      expect_equal(rigid0$podo, rxModelVars(rigid)$podo)
      expect_equal(rigid0$dfdy, rxModelVars(rigid)$dfdy)
      expect_equal(rigid0$sens, rxModelVars(rigid)$sens)
      expect_equal(rigid0$fn.ini, rxModelVars(rigid)$fn.ini)
      expect_equal(rigid0$state.ignore, rxModelVars(rigid)$state.ignore)
      expect_equal(rigid0$version, rxModelVars(rigid)$version)
      expect_equal(rigid0$normal.state, rxModelVars(rigid)$normal.state)
      expect_equal(rigid0$md5, rxModelVars(rigid)$md5)
      expect_equal(rigid0, rxModelVars(rigid))
      saveRDS(list(rigid0, rxModelVars(rigid)), "~/both.rds")
    })
  },
  silent = TRUE,
  test = "lvl2"
)
