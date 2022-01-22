# lhs for different compartments make sense

et <- et() %>%
  et(amt = 3, addl = 5, ii = 8, cmt = "depot") %>%
  et(seq(0, 48, length.out = 200), cmt = "depot") %>%
  et(seq(0, 48, length.out = 200), cmt = "centr") %>%
  et(seq(0, 48, length.out = 200), cmt = "peri")

ode.2c.ka <- rxode2({
  C2 <- centr / V
  C3 <- peri / V2
  d / dt(depot) <- -KA * depot
  d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
  d / dt(peri) <- Q * C2 - Q * C3
  C4 <- CMT
  if (CMT == 1) {
    prd <- depot
  }
  if (CMT == 2) {
    prd <- centr
  }
  if (CMT == 3) {
    prd <- peri
  }
})

tmp <- etTrans(et, ode.2c.ka)

test_that("multi-compartment solves", {
  s <- rxSolve(ode.2c.ka, params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), et)
  expect_equal(sort(unique(s$C4)), c(1, 2, 3))
  expect_equal(s[s$C4 == 1, "prd"], s[s$C4 == 1, "depot"])
  expect_equal(s[s$C4 == 2, "prd"], s[s$C4 == 2, "centr"])
  expect_equal(s[s$C4 == 3, "prd"], s[s$C4 == 3, "peri"])
})

## Now change cmt to an un-ordered factor.
et <- as.data.frame(et)
et$cmt <- factor(et$cmt, c("peri", "depot", "centr"), c("peri", "depot", "centr"))

tmp <- etTrans(et, ode.2c.ka)

test_that("multi-compartment solves", {
  s <- rxSolve(ode.2c.ka, params = c(V = 40, CL = 18, V2 = 297, Q = 10, KA = 0.3), et)
  expect_equal(sort(unique(s$C4)), c(1, 2, 3))
  expect_equal(s[s$C4 == 1, "prd"], s[s$C4 == 1, "depot"])
  expect_equal(s[s$C4 == 2, "prd"], s[s$C4 == 2, "centr"])
  expect_equal(s[s$C4 == 3, "prd"], s[s$C4 == 3, "peri"])
})
