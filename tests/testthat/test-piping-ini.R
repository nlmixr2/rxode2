test_that("piping with ini can update labels (rxode2/issues#351)", {
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

test_that("piping with ini gives an error pointing the user to use label for character rhs (rxode2/issues#351)", {
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
  expect_error(
    ini(ui, a = "bar"),
    regexp = "To assign a new label, use 'a <- label(\"bar\")'",
    fixed = TRUE
  )
})
