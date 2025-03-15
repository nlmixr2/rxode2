test_that("rxUiDeparse()", {

  rxUiDeparse(rxControl(), "a")
  expect_equal(rxUiDeparse(rxControl(), "a"),
               str2lang("a <- rxControl()"))

  expect_equal(rxUiDeparse(rxControl(covsInterpolation="linear", method="dop853",
                                    naInterpolation="nocb", keepInterpolation="nocb", sigmaXform="variance",
                                    omegaXform="variance", returnType="data.frame", sumType="fsum", prodType="logify"), "a"),
               str2lang("a <- rxControl(method = \"dop853\", covsInterpolation = \"linear\", returnType = \"data.frame\", sigmaXform = \"variance\", sumType = \"fsum\" naInterpolation = \"nocb\", keepInterpolation = \"nocb\")"))

})
