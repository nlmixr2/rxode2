# Test Transit compartment model

dat1 <- structure(list(time = c(0, 0.0503, 0.1005, 0.1508, 0.201, 0.2513, 0.3015, 0.3518, 0.402, 0.4523, 0.5025, 0.5528, 0.603, 0.6533, 0.7035, 8.9447, 8.995, 9.0452, 9.0955, 9.1457, 9.196, 9.2462, 9.2965, 9.3467, 9.397, 9.4472, 9.4975, 9.5477, 9.598, 9.6482, 9.6985), k = c(0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814), ktr = c(57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027), depot = c(0, 0, 0, 0.0044, 0.1348, 1.096, 3.9792, 8.5762, 13.1524, 16.2823, 17.8004, 18.2531, 18.1919, 17.9355, 17.6201, 0.7694, 0.7548, 0.7405, 0.7265, 0.7128, 0.6993, 0.6861, 0.6731, 0.6604, 0.6479, 0.6356, 0.6236, 0.6118, 0.6002, 0.5889, 0.5777), cen = c(0, 0, 0, 0, 8e-04, 0.0102, 0.0546, 0.1709, 0.3748, 0.6489, 0.9611, 1.285, 1.6058, 1.9171, 2.217, 2.4915, 2.4586, 2.426, 2.3939, 2.362, 2.3306, 2.2994, 2.2686, 2.2382, 2.2081, 2.1783, 2.1488, 2.1197, 2.091, 2.0625, 2.0344)), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 179L, 180L, 181L, 182L, 183L, 184L, 185L, 186L, 187L, 188L, 189L, 190L, 191L, 192L, 193L, 194L), class = "data.frame")

dat2 <- structure(list(time = c(0, 0.0503, 0.1005, 0.1508, 0.201, 0.2513, 0.3015, 0.3518, 0.402, 0.4523, 0.5025, 0.5528, 0.603, 0.6533, 0.7035, 8.9447, 8.995, 9.0452, 9.0955, 9.1457, 9.196, 9.2462, 9.2965, 9.3467, 9.397, 9.4472, 9.4975, 9.5477, 9.598, 9.6482, 9.6985), k = c(0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814, 0.3814), ktr = c(57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027, 57.027), depot = c(20, 19.6217, 19.2506, 18.8865, 18.5292, 18.1788, 17.8349, 17.4976, 17.1666, 16.842, 16.5234, 16.2109, 15.9043, 15.6034, 15.3083, 0.6681, 0.6555, 0.6431, 0.6309, 0.619, 0.6073, 0.5958, 0.5845, 0.5735, 0.5626, 0.552, 0.5416, 0.5313, 0.5213, 0.5114, 0.5017), cen = c(0, 0.3747, 0.7351, 1.0818, 1.4151, 1.7354, 2.043, 2.3383, 2.6217, 2.8935, 3.1541, 3.4038, 3.6429, 3.8717, 4.0905, 2.2571, 2.2268, 2.1968, 2.1671, 2.1378, 2.1088, 2.0802, 2.0518, 2.0238, 1.9962, 1.9688, 1.9418, 1.9151, 1.8887, 1.8626, 1.8368)), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 179L, 180L, 181L, 182L, 183L, 184L, 185L, 186L, 187L, 188L, 189L, 190L, 191L, 192L, 193L, 194L), class = "data.frame")

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
d/dt(depot) = exp(log(bio*podo)+log(ktr)+n*log(ktr*t)-ktr*t-lgammafn(n+1))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

et <- eventTable()
et$add.sampling(seq(0, 10, length.out = 200))
et$add.dosing(20, start.time = 0)

transit <- rxSolve(mod, et, transitAbs = TRUE)

test_that("Transit absorption works", {
  expect_equal(
    round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4),
    dat1
  )
})

test_that("Transit absorption is not specified, but still works.", {
  expect_warning(transit <- rxSolve(mod, et))
  expect_equal(
    round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4),
    dat1
  )
  expect_warning(rxSolve(mod, et))
})

## It gives different results than the last different results...? Is it important?  What does it mean?
test_that("Transit absorption is turned off, and gives other results", {
  no_transit <- rxSolve(mod, et, transitAbs = FALSE)
  expect_equal(
    round(as.data.frame(no_transit[c(1:15, seq(194 - 15, 194)), ]), 4),
    dat2
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
d/dt(depot) = exp(log(bio*podo)+log(ktr)+n*log(ktr*t)-ktr*t-lgamma1p(n))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

test_that("Transit absorption using lagmma1p works.", {
  expect_warning(transit <- rxSolve(mod, et))
  expect_equal(
    round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4),
    dat1
  )
  expect_warning(rxSolve(mod, et))
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
d/dt(depot) = exp(log(bio*podo)+log(ktr)+n*log(ktr*t)-ktr*t-lgamma(n+1))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

test_that("Transit absorption using C's lgamma function.", {
  expect_warning(transit <- rxSolve(mod, et))
  expect_equal(round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4), dat1)
  expect_warning(rxSolve(mod, et))
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
d/dt(depot) = exp(log(bio*podo)+log(ktr)+n*log(ktr*t)-ktr*t-lfactorial(n))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

test_that("Transit absorption using lfactorial", {
  expect_warning(transit <- rxSolve(mod, et))
  expect_equal(round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4), dat1)
  expect_warning(rxSolve(mod, et))
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
d/dt(depot) = exp(log(bio*podo)+log(ktr)+n*log(ktr*t)-ktr*t-log(factorial(n)))-ka*depot
d/dt(cen) = ka*depot-k*cen
")

test_that("Transit absorption using log(factorial)", {
  expect_warning(transit <- rxSolve(mod, et))
  expect_equal(round(as.data.frame(transit[c(1:15, seq(194 - 15, 194)), ]), 4), dat1)
  expect_warning(rxSolve(mod, et))
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

test_that("Transit absorption is function that can take 2 arguments", {
  expect_warning(transit2 <- rxSolve(mod, et))
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
  expect_warning(transit2 <- rxSolve(mod, et))
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
  expect_warning(transit2 <- rxSolve(mod, et))
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
