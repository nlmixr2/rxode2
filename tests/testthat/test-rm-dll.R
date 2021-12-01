rxode2Test(
  {
    context("Removing dlls")
    ode <- rxode2({
      b <- -1
      d / dt(X) <- a * X + Y * Z
      d / dt(Y) <- b * (Y - Z)
      d / dt(Z) <- -X * Y + c * Y - Z
    })

    dll <- rxDll(ode)

    test_that("dll exists", {
      expect_true(file.exists(dll))
      expect_true(file.exists(gsub("[.][^.]*$", ".c", dll)))
    })

    ode$delete()

    test_that("dll is removed", {
      expect_false(file.exists(dll))
      expect_false(file.exists(gsub("[.][^.]*$", ".c", dll)))
    })

    ode$load()

    test_that("dll exists #2", {
      expect_true(file.exists(dll))
      expect_true(file.exists(gsub("[.][^.]*$", ".c", dll)))
    })
  },
  silent = TRUE,
  test = "lvl2"
)
