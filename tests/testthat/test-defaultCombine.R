rxTest({
  test_that("defaultCombine works correctly", {
    # By default the function works with camel case, not snake case

    # Test when no arguments are provided
    expect_error(defaultCombine(), "no arguments provided")

    # Test when a single character string of length 1 is provided
    expect_equal(defaultCombine("a"), "a")
    expect_equal(camelCombine("a"), "a")
    expect_equal(snakeCombine("a"), "a")
    expect_equal(dotCombine("a"), "a")
    expect_equal(blankCombine("a"), "a")

    # Test when a single character vector of length 2 or more is provided
    expect_equal(defaultCombine(c("a", "b")), "aB")
    expect_equal(camelCombine(c("a", "b")), "aB")
    expect_equal(snakeCombine(c("a", "b")), "a_b")
    expect_equal(dotCombine(c("a", "b")), "a.b")
    expect_equal(blankCombine(c("a", "b")), "ab")

    # Test when a single list is provided
    expect_equal(defaultCombine(list("a", "b")), "aB")
    expect_equal(camelCombine(list("a", "b")), "aB")
    expect_equal(dotCombine(list("a", "b")), "a.b")
    expect_equal(snakeCombine(list("a", "b")), "a_b")
    expect_equal(blankCombine(list("a", "b")), "ab")

    # Test when multiple arguments are provided
    expect_equal(defaultCombine("a", "b"), "aB")
    expect_equal(camelCombine("a", "b"), "aB")
    expect_equal(snakeCombine("a", "b"), "a_b")
    expect_equal(dotCombine("a", "b"), "a.b")
    expect_equal(blankCombine("a", "b"), "ab")

    # Test invalid arguments
    expect_error(defaultCombine(1), "invalid argument")
    expect_error(snakeCombine(1), "invalid argument")
    expect_error(dotCombine(1), "invalid argument")
    expect_error(blankCombine(1), "invalid argument")
    expect_error(camelCombine(1), "invalid argument")

    # now test changing the default combine changes the method
    setCombineType("snake")
    expect_equal(defaultCombine("a", "b"), "a_b")
    setCombineType("dot")
    expect_equal(defaultCombine("a", "b"), "a.b")
    setCombineType("blank")
    expect_equal(defaultCombine("a", "b"), "ab")
    # camel case should be called last to restore the default for this
    # package
    setCombineType("camel")
    expect_equal(defaultCombine("a", "b"), "aB")
  })

  test_that("test combinePaste2", {
    # Test when no arguments are provided
    expect_error(combinePaste2(), "no arguments provided")

    # Test when a single character string of length 1 is provided
    expect_equal(combinePaste2("a"), "a")

    # Test when a single character vector of length 2 or more is provided
    expect_equal(combinePaste2(c("a", "b"), combineType = "default"), c("a", "b"))
    expect_equal(combinePaste2(c("a", "b"), combineType = "camel"), c("a", "b"))
    expect_equal(combinePaste2(c("a", "b"), combineType = "snake"), c("a", "b"))
    expect_equal(combinePaste2(c("a", "b"), combineType = "dot"), c("a", "b"))
    expect_equal(combinePaste2(c("a", "b"), combineType = "blank"), c("a", "b"))

    # Test when a single list is provided
    expect_error(combinePaste2(list("a", "b")), "Assertion on 'a' failed: Must be of type 'character', not 'list'.")

    # Test when multiple arguments are provided
    expect_equal(combinePaste2("a", "b", combineType = "default"), "aB")
    expect_equal(combinePaste2("a", "b", combineType = "camel"), "aB")
    expect_equal(combinePaste2("a", "b", combineType = "snake"), "a_b")
    expect_equal(combinePaste2("a", "b", combineType = "dot"), "a.b")
    expect_equal(combinePaste2("a", "b", combineType = "blank"), "ab")

    # Test when inputs are different sizes; 1 and many on either side
    expect_equal(combinePaste2("a", c("b", "d"), combineType = "default"), c("aB", "aD"))
    expect_equal(combinePaste2("a", c("b", "d"), combineType = "camel"), c("aB", "aD"))
    expect_equal(combinePaste2("a", c("b", "d"), combineType = "snake"), c("a_b", "a_d"))
    expect_equal(combinePaste2("a", c("b", "d"), combineType = "dot"), c("a.b", "a.d"))
    expect_equal(combinePaste2("a", c("b", "d"), combineType = "blank"), c("ab", "ad"))

    expect_equal(combinePaste2(c("a", "b"), "d", combineType = "default"), c("aD", "bD"))
    expect_equal(combinePaste2(c("a", "b"), "d", combineType = "camel"), c("aD", "bD"))
    expect_equal(combinePaste2(c("a", "b"), "d", combineType = "snake"), c("a_d", "b_d"))
    expect_equal(combinePaste2(c("a", "b"), "d", combineType = "dot"), c("a.d", "b.d"))
    expect_equal(combinePaste2(c("a", "b"), "d", combineType = "blank"), c("ad", "bd"))

    # Same size inputs should paste correctly according to the combineType
    expect_equal(combinePaste2(c("a", "b"), c("c", "d"), combineType = "default"), c("aC", "bD"))
    expect_equal(combinePaste2(c("a", "b"), c("c", "d"), combineType = "camel"), c("aC", "bD"))
    expect_equal(combinePaste2(c("a", "b"), c("c", "d"), combineType = "snake"), c("a_c", "b_d"))
    expect_equal(combinePaste2(c("a", "b"), c("c", "d"), combineType = "dot"), c("a.c", "b.d"))
    expect_equal(combinePaste2(c("a", "b"), c("c", "d"), combineType = "blank"), c("ac", "bd"))

    # Unequal sizes where the first or second is not length one should error
    expect_error(
      combinePaste2(c("a", "b"), c("c", "d", "e"), combineType = "default"),
      "combinePaste2 needs arguments that are the same size or one of the arguments to be a single string"
    )

    # Test invalid arguments
    expect_error(combinePaste2(1))

    expect_error(combinePaste2("a", 1))

    # now test changing the default combine changes the method
    setCombineType("snake")
    expect_equal(combinePaste2("a", "b"), "a_b")
    setCombineType("camel")
    expect_equal(combinePaste2("a", "b"), "aB")
  })

  test_that(".getCombineTypeFromRoption", {
    # test options that are not formed as expected

    expect_equal(.getCombineTypeFromRoption("a"), "default")

    expect_equal(.getCombineTypeFromRoption(42), "default")

    if (requireNamespace("withr", quietly = TRUE)) {
      withr::with_options(list(rxode2.etaCombineType = 42), {
        expect_equal(.getCombineTypeFromRoption("rxode2.etaCombineType"), "default")
      })
    }
  })

  test_that(".defaultCombine2", {
    # Test if v1 and v2 are longer than one and the same length, the output makes sense
    expect_equal(.defaultCombine2(c("a", "b"), "b"), "aBB")
    expect_equal(.defaultCombine2("b", c("a", "b")), "bAB")

    # Test the case that the second argument is a empty string
    expect_equal(.defaultCombine2("a", ""), "a")
  })
})
