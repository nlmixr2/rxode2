rxTest({
  # Test Transit compartment model

  dat1 <- structure(list(time = c(0, 0.0503, 0.1005, 0.1508, 0.201, 0.2513, 0.3015, 0.3518, 0.402, 0.4523, 0.5025, 0.5528, 0.603, 0.6533, 0.7035, 8.9447, 8.995, 9.0452, 9.0955, 9.1457, 9.196, 9.2462, 9.2965, 9.3467, 9.397, 9.4472, 9.4975, 9.5477, 9.598, 9.6482, 9.6985), k = c(0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814), ktr = c(57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027), depot = c(0, 0, 0, 0.0044, 0.1348, 1.096, 3.9792, 8.5762, 13.1524, 16.2823, 17.8004, 18.2531, 18.1919, 17.9355, 17.6201, 0.7694, 0.7548, 0.7405, 0.7265, 0.7128, 0.6993, 0.6861, 0.6731, 0.6604, 0.6479, 0.6356, 0.6236, 0.6118, 0.6002, 0.5889, 0.5777), cen = c(0, 0, 0, 0, 8e-04, 0.0102, 0.0546, 0.1709, 0.3748, 0.6489, 0.9611, 1.285, 1.6058, 1.9171, 2.217, 2.4915, 2.4586, 2.426, 2.3939, 2.362, 2.3306, 2.2994, 2.2686, 2.2382, 2.2081, 2.1783, 2.1488, 2.1197, 2.091, 2.0625, 2.0344)), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 179L, 180L, 181L, 182L, 183L, 184L, 185L, 186L, 187L, 188L, 189L, 190L, 191L, 192L, 193L, 194L), class = "data.frame")

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
ktr = (n+1)/mtt
## note that lgammafn is the same as lgamma in R.
d/dt(depot) = exp(log(bio*podo(depot))+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-lgammafn(n+1))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  et <- eventTable()
  et$add.sampling(seq(0, 10, length.out = 200))
  et$add.dosing(20, start.time = 0, evid=7)

  expect_error(rxSolve(mod, et, transitAbs = TRUE))

  transit <- rxSolve(mod, et)

  test_that("Transit absorption works", {
    expect_equal(
      round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4),
      dat1
    )
  })

  test_that("Transit absorption is not specified, but still works.", {
    transit <- rxSolve(mod, et)
    expect_equal(
      round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4),
      dat1
    )
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
ktr = (n+1)/mtt
## note that lgamma1p is the same as lgamma(1+p) in R.
d/dt(depot) = exp(log(bio*podo(depot))+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-lgamma1p(n))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  test_that("Transit absorption using lagmma1p works.", {
    transit <- rxSolve(mod, et)
    expect_equal(
      round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4),
      dat1
    )
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
ktr = (n+1)/mtt
d/dt(depot) = exp(log(bio*podo(depot))+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-lgamma(n+1))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  test_that("Transit absorption using C's lgamma function.", {
    transit <- rxSolve(mod, et)
    expect_equal(round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4), dat1)
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
ktr = (n+1)/mtt
d/dt(depot) = exp(log(bio*podo())+log(ktr)+n*log(ktr*t)-ktr*t-lfactorial(n))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  test_that("Transit absorption using lfactorial", {
    transit <- rxSolve(mod, et)
    expect_equal(round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4), dat1)
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
ktr = (n+1)/mtt
d/dt(depot) = exp(log(bio*podo(depot))+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-log(factorial(n)))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  test_that("Transit absorption using log(factorial)", {
    transit <- rxSolve(mod, et)
    expect_equal(round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4), dat1)
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
n = 20.1
k = cl/vc
## note that lgammafn is the same as lgamma in R.
d/dt(depot) = transit(n, mtt)-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  # test dosing to central compartment
  etC <- eventTable()
  etC$add.sampling(seq(0, 10, length.out = 200))
  etC$add.dosing(20, start.time = 0, evid=1, cmt="cen")

  tmp <- rxSolve(mod, etC)
  expect_false(any(is.na(etC$cen)))

  test_that("Transit absorption is function that can take 2 arguments", {
    transit2 <- rxSolve(mod, et)
    expect_equal(
      round(as.data.frame(transit)[, names(transit) != "ktr"], 4),
      round(as.data.frame(transit2), 4)
    )
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
d/dt(depot) = transit(n, mtt, bio)-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  test_that("Transit absorption is function that can take 3 arguments", {
    transit2 <- rxSolve(mod, et)
    expect_equal(
      round(as.data.frame(transit)[, names(transit) != "ktr"], 4),
      round(as.data.frame(transit2), 4)
    )
  })

  mod <- rxode2("
## Table 3 from Savic 2007
cl = 17.2 # (L/hr)
vc = 45.1 # L
ka = 0.38 # 1/hr
mtt = 0.37 # hr
bio=1
n = 20.1
k = cl/vc
d/dt(depot) = transit(n, mtt, 1)-ka*depot
d/dt(cen) = ka*depot-k*cen
")

  test_that("Transit absorption can take numeric arguments", {
    transit2 <- rxSolve(mod, et)
    expect_equal(
      round(as.data.frame(transit)[, names(transit) != "ktr"], 4),
      round(as.data.frame(transit2), 4)
    )
  })

  test_that("transit can be a variable or ODE", {
    mod <- rxode2("
d/dt(transit) = -3
")

    expect_s3_class(mod, "rxode2")

    mod <- rxode2("
transit = matt + fun
")
    expect_s3_class(mod, "rxode2")
  })

  test_that("transit compartment works well with dual absorption (#804, #819)", {

    mod <- function() {
      ini({
        ## Table 3 from Savic 2007
        cl  <- 17.2 # (L/hr)
        vc  <- 45.1 # L
        ka  <- 0.38 # 1/hr
        mtt <- 1.37 # hr
        f2 <-0.5    # Fraction of 1st Order portion
        n   <- 20.101
      })
      model({
        k           <- cl/vc
        bio <- 1-f2
        d/dt(depot1) <- transit(n,mtt,bio)-ka*depot1
        d/dt(depot2) <- -ka*depot2
        f(depot2) <-f2
        d/dt(cen)   <- ka*depot1 + ka*depot2-k*cen
      })
    }

    ev1 <- et(0, 7, length.out=200) %>%
      et(amt=20, cmt='depot1', evid=7) %>%
      et(amt=20, cmt='depot2', evid=1)

    case1 <- rxSolve(mod, ev1)

    expect_true(max(case1$depot1) > 7.7)

  })
})
