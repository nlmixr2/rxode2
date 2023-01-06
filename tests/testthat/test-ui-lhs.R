test_that("lhs ui accessors", {
  
  oneCmtAllo <- function() {
    ini({
      lka <- log(0.1); label("Absorption rate (Ka)")
      lcl <- log(0.2); label("Clearance (CL)")
      lvc <- log(1); label("Central volume of distribution (V)")
      cppropSd <- c(0, 0.5)
      allo_cl <- fix(0.75)
      allo_vc <- fix(1)
      etalcl ~ 0.1
    })
    model({
      ka <- exp(lka)
      cl <- exp(lcl + allo_cl * log(WEIGHT_BL/100) + etalcl)
      vc <- exp(lvc + allo_vc * log(WEIGHT_BL/100))
      cp <- 1000 * linCmt()
      cp ~ prop(cppropSd)
    })
  }

  f <- rxode2(oneCmtAllo)


  expect_equal(f$lhsVar,
               c(cl = "etalcl", ka = "lka", cl = "lcl", cl = "allo_cl", vc = "lvc", vc = "allo_vc", cl = "WEIGHT_BL", vc = "WEIGHT_BL"))

  expect_equal(f$varLhs,
               c(etalcl = "cl", lka = "ka", lcl = "cl", allo_cl = "cl", lvc = "vc", allo_vc = "vc", WEIGHT_BL = "cl", WEIGHT_BL = "vc"))


  expect_equal(f$etaLhs, c(etalcl = "cl"))
  expect_equal(f$lhsEta, c(cl = "etalcl"))

  expect_equal(f$thetaLhs, c(lka = "ka", lcl = "cl", allo_cl = "cl", lvc = "vc", allo_vc = "vc"))
  expect_equal(f$lhsTheta, c(ka = "lka", cl = "lcl", cl = "allo_cl", vc = "lvc", vc = "allo_vc"))

  expect_equal(f$covLhs,
               c(WEIGHT_BL = "cl", WEIGHT_BL = "vc"))
  
  expect_equal(f$lhsCov,
               c(cl = "WEIGHT_BL", vc = "WEIGHT_BL"))
  
})
