rxode2Test(
  {
    context("NA tests")

    test_that("can assign NA", {
      mod1 <- rxode2("a=NA;\nb=2;\nc=a+b")
      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_true(all(is.na(ret$a)))
      expect_true(all(is.na(ret$b)))
    })


    test_that("logic NA", {
      mod1 <- rxode2("a=NA;\nif(is.na(a)){a=3}b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")

      expect_equal(ret$a, as.double(3))
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=NA;\nif(!is.na(a)){a=4} else { a=3 }; b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_equal(ret$c, 3 + 2 + exp(3))
    })


    context("Inf tests")

    test_that("can assign Inf", {
      mod1 <- rxode2("a=Inf;\nb=2;\nc=a+b")
      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_true(all(is.infinite(ret$a)))
      expect_true(all(ret$a > 0))
      expect_true(all(ret$b > 0))
    })

    test_that("can assign -Inf", {
      mod1 <- rxode2("a=-Inf;\nb=2;\nc=a+b")
      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_true(all(is.infinite(ret$a)))
      expect_true(all(ret$a < 0))
      expect_true(all(ret$b < 0))
    })

    test_that("logic Inf", {
      mod1 <- rxode2("a=Inf;\nif(is.infinite(a)){a=3}b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")

      expect_equal(ret$a, as.double(3))
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=Inf;\nif(!is.finite(a)){a=3}b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")

      expect_equal(ret$a, as.double(3))
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=Inf;\nif(!is.infinite(a)){a=4} else { a=3 }; b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=Inf;\nif(is.finite(a)){a=4} else { a=3 }; b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_equal(ret$c, 3 + 2 + exp(3))
    })


    test_that("logic -Inf", {
      mod1 <- rxode2("a=-Inf;\nif(is.infinite(a)){a=3}b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")

      expect_equal(ret$a, as.double(3))
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=-Inf;\nif(!is.finite(a)){a=3}b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")

      expect_equal(ret$a, as.double(3))
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=-Inf;\nif(!is.infinite(a)){a=4} else { a=3 }; b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=-Inf;\nif(is.finite(a)){a=4} else { a=3 }; b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_equal(ret$c, 3 + 2 + exp(3))
    })

    context("NaN tests")

    test_that("can assign NaN", {
      mod1 <- rxode2("a=NaN;\nb=2;\nc=a+b")
      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_true(all(is.nan(ret$a)))
      expect_true(all(is.nan(ret$b)))
    })


    test_that("logic NaN", {
      mod1 <- rxode2("a=NaN;\nif(is.nan(a)){a=3}b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")

      expect_equal(ret$a, as.double(3))
      expect_equal(ret$c, 3 + 2 + exp(3))

      mod1 <- rxode2("a=NaN;\nif(!is.nan(a)){a=4} else { a=3 }; b=2;\nc=a+b+exp(3)")

      ret <- rxSolve(mod1, et(0), returnType = "data.frame")
      expect_equal(ret$c, 3 + 2 + exp(3))
    })
  },
  test = "lvl2"
)
