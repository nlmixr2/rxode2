rxTest({
  test_that("a covariate named value gets an informative error (#1386)", {
    d <- data.frame(ID = 1L, TIME = 1:4, value = c(1, 2, 3, 4))
    m <- function() {
      ini({ a <- 1 })
      model({ y <- a * value })
    }
    expect_error(rxSolve(m, d), "'value' is read as the 'amt' alias")
    s <- rxSolve(m, transform(d, AMT = 0), returnType = "data.frame")
    expect_equal(s$y, d$value)
  })
})
