test_that("piping with ini can update labels", {
  mod <- function() {
    ini({
      a <- 1; label("foo")
      addSd <- 2
    })
    model({
      b <- a
      b ~ add(addSd)
    })
  }
  ui <- rxode2(mod)
  expect_equal(ui$iniDf$label[ui$iniDf$name == "a"], "foo")
  newLabelUi <- ini(ui, a = label("bar"))
  expect_equal(newLabelUi$iniDf$label[newLabelUi$iniDf$name == "a"], "bar")
})
