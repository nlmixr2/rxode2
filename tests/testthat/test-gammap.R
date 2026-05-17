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
})
