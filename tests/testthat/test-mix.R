rxTest({
  test_that("mix() in normal rxode2 model", {
    expect_error(rxModelVars("a = mix(a)"))
    expect_error(rxModelVars("a = mix(a, b)"))
    expect_error(rxModelVars("a = mix(a, b, c)"), NA)
    expect_error(rxModelVars("a = mix(a, b, c, d)"))
  })

  test_that("mix() in ui model", {



    ## f <- function() {
    ##   ini({
    ##     p1 <- 0.1
    ##     p2 <- 0.2
    ##     p3 <- 0.1
    ##   })
    ##   model({
    ##     a <- mix(a1, p1,a2, p2, a3)
    ##   })
    ## }

  })

})
