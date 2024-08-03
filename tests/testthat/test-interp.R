test_that("interpolation functions", {

  tmp <- rxModelVars("locf(a);\n ret=a+b")

  expect_equal(rxNorm(tmp), "locf(a);\nret=a+b;\n")

  expect_equal(as.character(tmp$interp["a"]), "locf")
  expect_equal(as.character(tmp$interp["b"]), "default")

  tmp <- rxModelVars("params(b, a);\nlocf(a);\n ret=a+b")

  expect_equal(as.character(tmp$interp["a"]), "locf")
  expect_equal(as.character(tmp$interp["b"]), "default")

  expect_error(rxModelVars("params(b, a);\nlocf(a);\nnocb(a);\n ret=a+b"))

  tmp <- rxModelVars("params(b, a);\nlinear(a);\n ret=a+b")

  expect_equal(as.character(tmp$interp["a"]), "linear")
  expect_equal(as.character(tmp$interp["b"]), "default")

  tmp <- rxModelVars("params(b, a);\nnocb(a);\n ret=a+b")
  expect_equal(as.character(tmp$interp["a"]), "nocb")
  expect_equal(as.character(tmp$interp["b"]), "default")

  tmp <- rxModelVars("params(b, a);\nmidpoint(a);\n ret=a+b")
  expect_equal(as.character(tmp$interp["a"]), "midpoint")
  expect_equal(as.character(tmp$interp["b"]), "default")


})
