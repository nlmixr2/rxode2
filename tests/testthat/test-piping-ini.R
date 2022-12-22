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


test_that(".iniSimplifyFixUnfix", {
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("fix")),
    as.name("fix")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("fixed")),
    as.name("fix")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("FIX")),
    as.name("fix")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("FIXED")),
    as.name("fix")
  )

  expect_equal(
    .iniSimplifyFixUnfix(str2lang("unfix")),
    as.name("unfix")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("unfixed")),
    as.name("unfix")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("UNFIX")),
    as.name("unfix")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("UNFIXED")),
    as.name("unfix")
  )

  expect_equal(
    .iniSimplifyFixUnfix(str2lang("FIXED(a)")),
    str2lang("fix(a)")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("c <- FIXED(a+b)")),
    str2lang("c <- fix(a + b)")
  )
  expect_equal(
    .iniSimplifyFixUnfix(str2lang("c <- UNFIXED(a+b)")),
    str2lang("c <- unfix(a + b)")
  )
})

test_that("piping with ini can update reorder parameters (rxode2/issues#352)", {
  mod <- function() {
    ini({
      a <- 1
      b <- 2
      c <- 3
      addSd <- 2
    })
    model({
      b <- a + b*log(c)
      b ~ add(addSd)
    })
  }
  ui <- rxode2(mod)
  # No modification
  expect_equal(ui$iniDf$name, c("a", "b", "c", "addSd"))
  # b to the top
  expect_equal(ini(ui, b <- after())$iniDf$name, c("b", "a", "c", "addSd"))
  # b to the bottom by number
  expect_equal(ini(ui, b <- after(Inf))$iniDf$name, c("a", "c", "addSd", "b"))
  # b to the bottom by name
  expect_equal(ini(ui, b <- after(addSd))$iniDf$name, c("a", "c", "addSd", "b"))
  # b after c
  expect_equal(ini(ui, b <- after(c))$iniDf$name, c("a", "c", "b", "addSd"))
  # b to b, warn and no change
  expect_warning(
    expect_equal(ini(ui, b <- after(b))$iniDf$name, c("a", "b", "c", "addSd")),
    regexp = "Parameter 'b' set to be moved after itself, no change made"
  )
})
