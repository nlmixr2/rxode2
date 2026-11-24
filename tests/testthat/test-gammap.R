rxTest({
  test_that("incomplete gamma", {
    gp <- rxode2({
      x <- gammap(a, z)
    })

    ## Numbers taken from pracma
    expect_equal(lowergamma(2, 1.5), 0.4421746)
    expect_equal(uppergamma(2, 1.5), 0.5578254)
    expect_equal(gammap(2, 1.5), 0.4421746)
    expect_equal(lowergamma(3, 1.5), 0.3823063, tolerance = 1e-6)
    expect_equal(uppergamma(3, 1.5), 1.6176937, tolerance = 1e-6)
    expect_equal(uppergamma(3, 1.5), 1.6176937, tolerance = 1e-6)
    ## note -1.5 doesn't work with boost, but works with pracma...
    expect_equal(gammaq(2, 1.5), 0.5578254, tolerance = 1e-6)

    expect_equal(
      rxFromSE("Derivative(gammap(x,y),y)"),
      "gammapDer(x,y)"
    )
  })

  test_that("gammapInva()/gammaqInva() invert in the shape, as in models", {
    .x <- c(0.5, 1, 3)
    .p <- c(0.3, 0.7, 0.5)
    ## a with gammap(a, x) = p (they returned gammapInv()/gammaqInv() before)
    expect_equal(gammap(gammapInva(.x, .p), .x), .p, tolerance = 1e-10)
    expect_equal(gammaq(gammaqInva(.x, .p), .x), .p, tolerance = 1e-10)
    .m <- rxode2({
      ga <- gammapInva(x, p)
      gq <- gammaqInva(x, p)
    })
    .s <- rxSolve(.m, data.frame(x = .x, p = .p), et(0), returnType = "data.frame")
    expect_equal(gammapInva(.x, .p), .s$ga)
    expect_equal(gammaqInva(.x, .p), .s$gq)
  })

  test_that("gamma inverse distinguishes repeated shapes and probabilities", {
    shape <- rep(c(0.5, 0.50001, 0.5, 0.50001, 0.5, 2), 20L)
    probability <- rep(c(0.25, 0.25, 0.75, 0.25, 0.25, 0.75), 20L)
    expect_equal(gammapInv(shape, probability), qgamma(probability, shape),
                 tolerance = 1e-12)
  })
})
