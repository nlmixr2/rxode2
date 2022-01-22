# Test loading, and unloading of models

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

rigid <- rxode2(rigid.txt)

test_that("loading and unloading works.", {
  dll <- rxDll(rigid)
  expect_true(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
  rxUnload(rigid)
  expect_false(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
  rxLoad(rigid)
  expect_true(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
  rxDelete(rigid)
  expect_false(rxDllLoaded(rigid))
  expect_false(file.exists(dll))
  rxLoad(rigid)
  expect_true(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
  rxDelete(rigid)
  rxSyncOptions()
  expect_false(rxDllLoaded(rigid))
  expect_false(file.exists(dll))
  ## Test $ syntax
  rigid$compile()
  expect_true(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
  rigid$delete()
  expect_false(rxDllLoaded(rigid))
  expect_false(file.exists(dll))
  rigid$dynLoad()
  expect_true(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
  rigid$dynUnload()
  expect_false(rxDllLoaded(rigid))
  expect_true(file.exists(dll))
})
