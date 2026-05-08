rxTest({
  .expectStrCmpParams <- function(obj, expected) {
    expect_equal(names(obj$strCmpParams), names(expected))
    expect_equal(obj$strCmpParams, expected)
    for (.nm in names(expected)) {
      expect_s3_class(obj$strCmpParams[[.nm]], "factor")
      expect_equal(levels(obj$strCmpParams[[.nm]]), levels(expected[[.nm]]))
      expect_equal(as.character(obj$strCmpParams[[.nm]]),
                   as.character(expected[[.nm]]))
    }
  }

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

  test_that("modelvars track exhaustive string comparison covariates", {
    skip_on_cran()
    rxClean()

    modTxt <- "
x = 1
if (covB == \"same\" ||
    covA == \"alpha\" ||
    \"beta\" == covA ||
    covA != \"alpha\" ||
    \"same\" != covB ||
    covC == \"shared\" ||
    \"shared\" == covA ||
    covB == \"same\" ||
    ID == \"drop\" ||
    \"drop2\" != id ||
    covC != \"shared\" ||
    covB == \"gamma\" ||
    \"alpha\" == covB ||
    covA == \"beta\" ||
    covC == \"omega\") {
  x = 0
}
"

    expected <- list(
      covB = factor(c("same", "gamma", "alpha"),
                    levels = c("same", "gamma", "alpha")),
      covA = factor(c("alpha", "beta", "shared"),
                    levels = c("alpha", "beta", "shared")),
      covC = factor(c("shared", "omega"),
                    levels = c("shared", "omega"))
    )

    parsed <- rxGetModel(modTxt)
    compiled <- rxode2(modTxt)

    .expectStrCmpParams(parsed, expected)
    .expectStrCmpParams(rxModelVars(compiled), expected)

    parsedCompare <- parsed[setdiff(names(parsed), "timeId")]
    compiledCompare <- rxModelVars(compiled)
    compiledCompare <- compiledCompare[setdiff(names(compiledCompare), "timeId")]
    expect_equal(parsedCompare, compiledCompare)
  })

  test_that("blank modelvars has same size", {
    blank <- rxModelVars("")
    full <-  rxModelVars("y = 1")
    expect_equal(length(rxModelVars(blank)), length(full))
    expect_length(blank$strCmpParams, 0)
    expect_equal(names(blank$strCmpParams), character(0))
    expect_equal(tail(names(full), 5),
                 c("lhsOrd", "splitBolus", "strCmpParams", "timeId", "md5"))
  })
})
