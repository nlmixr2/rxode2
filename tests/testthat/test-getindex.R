rxodeTest(
  {
    require(rxode2)
    context("Get Index")
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

    test_that("Index access the right compartment", {
      expect_equal(rxState(rigid, "y1"), 1)
      expect_equal(rxState(rigid, "y2"), 2)
      expect_equal(rxState(rigid, "y3"), 3)
      expect_error(rxState(rigid, "matt"), "cannot locate compartment")
      expect_equal(rigid$get.index("y1"), 1)
      expect_equal(rigid$get.index("y2"), 2)
      expect_equal(rigid$get.index("y3"), 3)
      expect_error(rigid$get.index("matt"), "cannot locate compartment")
    })
  },
  silent = TRUE,
  test = "lvl2"
)
