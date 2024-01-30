test_that("v1 rate constants", {
  p1 <- rxDerived(v1 = 8, k = 0.5, digits = 3)
  expect_equal(
    p1,
    structure(list(
      vc = 8, kel = 0.5, vss = 8, cl = 4, t12alpha = 1.39,
      alpha = 0.5, A = 0.125, fracA = 1
    ),
    class = "data.frame", row.names = c(NA, -1L)
    )
  )

  p1 <- rxDerived(v1 = 5, k = 0.7, k12 = 0.5, k21 = 0.05, digits = 3)

  expect_equal(
    p1,
    structure(list(
      vc = 5, kel = 0.7, k12 = 0.5, k21 = 0.05, vp = 50,
      vss = 55, cl = 3.5, q = 2.5, t12alpha = 0.568, t12beta = 24.2,
      alpha = 1.22, beta = 0.0287, A = 0.196, B = 0.00358, fracA = 0.982,
      fracB = 0.0179
    ),
    class = "data.frame",
    row.names = c(NA, -1L)
    )
  )

  p1 <- rxDerived(v1 = 10, k = 0.3, k12 = 0.2, k13 = 0.1, k21 = 0.02, k31 = 0.001, digits = 3)

  expect_equal(p1, structure(list(
    vc = 10, kel = 0.3, k12 = 0.2, k21 = 0.02, k13 = 0.1,
    k31 = 0.001, vp = 100, vp2 = 1000, vss = 1110, cl = 3, q = 2,
    q2 = 1, t12alpha = 1.14, t12beta = 52.2, t12gamma = 931,
    alpha = 0.607, beta = 0.0133, gamma = 0.000745, A = 0.0988,
    B = 0.00111, C = 6.47e-05, fracA = 0.988, fracB = 0.0111,
    fracC = 0.000647
  ),
  class = "data.frame",
  row.names = c(NA, -1L)
  ))
})

test_that("Volumes and clearances", {
  p1 <- rxDerived(v1 = 8.0, cl = 4.0, digits = 3)

  expect_equal(p1, structure(list(
    vc = 8, kel = 0.5, vss = 8, cl = 4, t12alpha = 1.39,
    alpha = 0.5, A = 0.125, fracA = 1
  ),
  class = "data.frame",
  row.names = c(NA, -1L)
  ))

  p1 <- rxDerived(v1 = 5.0, v2 = 50, cl = 3.5, q = 2.5, digits = 3)

  expect_equal(p1, structure(list(
    vc = 5, kel = 0.7, k12 = 0.5, k21 = 0.05, vp = 50,
    vss = 55, cl = 3.5, q = 2.5, t12alpha = 0.568, t12beta = 24.2,
    alpha = 1.22, beta = 0.0287, A = 0.196, B = 0.00358, fracA = 0.982,
    fracB = 0.0179
  ),
  class = "data.frame",
  row.names = c(NA, -1L)
  ))

  p1 <- rxDerived(
    v1 = 10, v2 = 100, v3 = 1000,
    cl = 3, q = 2, q2 = 1, digits = 3
  )

  expect_equal(p1, structure(list(
    vc = 10, kel = 0.3, k12 = 0.2, k21 = 0.02, k13 = 0.1,
    k31 = 0.001, vp = 100, vp2 = 1000, vss = 1110, cl = 3, q = 2,
    q2 = 1, t12alpha = 1.14, t12beta = 52.2, t12gamma = 931,
    alpha = 0.607, beta = 0.0133, gamma = 0.000745, A = 0.0988,
    B = 0.00111, C = 6.47e-05, fracA = 0.988, fracB = 0.0111,
    fracC = 0.000647
  ),
  class = "data.frame",
  row.names = c(NA, -1L)
  ))
})
