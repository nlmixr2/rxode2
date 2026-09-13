test_that("a correlated occasion block is no longer refused outright", {

  ## the whole point: `| occ` etas may now covary
  .f <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      add.sd <- 0.7
      eta.ka ~ 0.6
      iov.cl + iov.v ~ c(0.1,
                         0.01, 0.2) | occ
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + iov.cl)
      v <- exp(tv + iov.v)
      d/dt(depot) <- -ka * depot
      d/dt(cent) <- ka * depot - (cl / v) * cent
      cent ~ add(add.sd)
    })
  }
  .ui <- suppressWarnings(rxode2::rxUiDecompress(.f()))

  ## two levels, and the occasion block keeps its covariance
  expect_equal(sort(names(.ui$omega)), c("id", "occ"))
  expect_equal(unname(unclass(.ui$omega$occ)[1, 2]), 0.01)
})

test_that("nested simulation gives each parameter its own variance", {

  ## The omega a nesting level draws from is built by `lotriSep()`
  ## stamping the level's block once per nesting unit, so it is laid out
  ## occasion-major with the parameters INSIDE each stamp.
  ## `rxExpandNestingRep()` indexed it parameter-major, which transposed
  ## the two: every parameter drew the variance belonging to whichever
  ## one sat at that position in the block.  With a single parameter per
  ## level the transposition is invisible, which is why it survived.
  skip_on_cran()
  .mod <- rxode2({
    ka <- exp(0.45 + eta.ka)
    cl <- exp(1 + a)
    v <- exp(3.45 + b)
    q <- exp(0.1 + cc)
    d/dt(depot) <- -ka * depot
    d/dt(cent) <- ka * depot - (cl / v) * cent - q * cent
  })
  .ev <- et(amt = 100, ii = 24, until = 72)
  .ev <- et(.ev, 0:96)
  .ev <- et(.ev, id = 1:3000)
  .ev$occ <- 1 + (.ev$time >= 32) + (.ev$time >= 64)

  ## three deliberately well separated variances, each the same in every
  ## occasion
  .om <- lotri::lotri(lotri::lotri(eta.ka ~ 0.6) | id(nu = 1e6),
                      lotri::lotri(a ~ 0.01,
                                   b ~ 1,
                                   cc ~ 100) | occ(nu = 1e6))

  withr::with_seed(3, {
    .s <- suppressWarnings(rxSolve(.mod, .ev, omega = .om, sigma = NULL,
                                   nDisplayProgress = 1e6))
  })
  .p <- .s$params

  for (.k in 1:3) {
    expect_equal(var(.p[[paste0("a(occ==", .k, ")")]]), 0.01, tolerance = 0.1)
    expect_equal(var(.p[[paste0("b(occ==", .k, ")")]]), 1, tolerance = 0.1)
    expect_equal(var(.p[[paste0("cc(occ==", .k, ")")]]), 100, tolerance = 0.1)
  }
})

test_that("nested simulation carries a correlated occasion block", {

  ## the point of the whole exercise: IOV parameters may covary, and the
  ## covariance has to land WITHIN an occasion, not between occasions
  skip_on_cran()
  .mod <- rxode2({
    ka <- exp(0.45 + eta.ka)
    cl <- exp(1 + iov.cl)
    v <- exp(3.45 + iov.v)
    d/dt(depot) <- -ka * depot
    d/dt(cent) <- ka * depot - (cl / v) * cent
  })
  .ev <- et(amt = 100, ii = 24, until = 72)
  .ev <- et(.ev, 0:96)
  .ev <- et(.ev, id = 1:3000)
  .ev$occ <- 1 + (.ev$time >= 48)

  .om <- lotri::lotri(lotri::lotri(eta.ka ~ 0.6) | id(nu = 1e6),
                      lotri::lotri(iov.cl + iov.v ~ c(0.1,
                                                      0.06, 0.2)) |
                        occ(nu = 1e6))

  withr::with_seed(5, {
    .s <- suppressWarnings(rxSolve(.mod, .ev, omega = .om, sigma = NULL,
                                   nDisplayProgress = 1e6))
  })
  .p <- .s$params

  for (.k in 1:2) {
    .cl <- .p[[paste0("iov.cl(occ==", .k, ")")]]
    .v <- .p[[paste0("iov.v(occ==", .k, ")")]]
    expect_equal(var(.cl), 0.1, tolerance = 0.15)
    expect_equal(var(.v), 0.2, tolerance = 0.15)
    ## the specified covariance, within the occasion
    expect_equal(cov(.cl, .v), 0.06, tolerance = 0.25)
  }

  ## and the occasions stay independent of each other
  expect_equal(cov(.p[["iov.cl(occ==1)"]], .p[["iov.cl(occ==2)"]]), 0,
               tolerance = 0.02)
  expect_equal(cov(.p[["iov.cl(occ==1)"]], .p[["iov.v(occ==2)"]]), 0,
               tolerance = 0.02)
})

test_that("several nesting levels each keep their own variances", {

  ## The reindex has to leave each level's slice of the parameter vector
  ## where it was and only reorder WITHIN it, so more than one level at a
  ## time is the case that would catch a bookkeeping slip.  The existing
  ## multi-level test (`test-occ.R`) uses equal variances within each
  ## level, which cannot see a transposition.
  skip_on_cran()
  .mod <- rxode2({
    cl <- exp(1 + eta.cl + occ.cl + eye.cl)
    v <- exp(3.45 + eta.v + occ.v + eye.v)
    d/dt(cent) <- -(cl / v) * cent
  })
  .ev <- et(amt = 100)
  .ev <- et(.ev, 0:24)
  .ev <- et(.ev, id = 1:2500)
  .ev$occ <- 1 + (.ev$time >= 12)
  .ev$eye <- 1 + (.ev$time %% 2 == 1)

  .om <- lotri::lotri(
    lotri::lotri(eta.cl ~ 0.5, eta.v ~ 0.9) | id(nu = 1e6),
    lotri::lotri(occ.cl ~ 0.01, occ.v ~ 4) | occ(nu = 1e6),
    lotri::lotri(eye.cl ~ 0.02, eye.v ~ 9) | eye(nu = 1e6))

  withr::with_seed(21, {
    .s <- suppressWarnings(rxSolve(.mod, .ev, omega = .om, sigma = NULL,
                                   nDisplayProgress = 1e6))
  })
  .p <- .s$params

  for (.k in 1:2) {
    expect_equal(var(.p[[paste0("occ.cl(occ==", .k, ")")]]), 0.01,
                 tolerance = 0.15)
    expect_equal(var(.p[[paste0("occ.v(occ==", .k, ")")]]), 4,
                 tolerance = 0.15)
    expect_equal(var(.p[[paste0("eye.cl(eye==", .k, ")")]]), 0.02,
                 tolerance = 0.2)
    expect_equal(var(.p[[paste0("eye.v(eye==", .k, ")")]]), 9,
                 tolerance = 0.15)
  }
  ## and the id level is untouched by the levels below it
  expect_equal(var(.p$eta.cl), 0.5, tolerance = 0.15)
  expect_equal(var(.p$eta.v), 0.9, tolerance = 0.15)
})
