test_that("a repeated omega block is one level of variability, not several", {

  ## `lotri`'s `same()` records a repeated block in the `condition`
  ## column as `<level>:same:<master>`.  That suffix names the element a
  ## row mirrors; it is NOT a different level of variability, and code
  ## that reads `condition` as a level has to strip it.
  .f <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      tq <- 1
      tp <- 1
      add.sd <- 0.7
      eta.ka ~ 0.6
      iov.cl1 + iov.v1 ~ c(0.1,
                           0.01, 0.2)
      iov.cl2 + iov.v2 ~ same()
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + iov.cl1)
      v <- exp(tv + iov.v1)
      q <- exp(tq + iov.cl2)
      p <- exp(tp + iov.v2)
      d/dt(depot) <- -ka * depot
      d/dt(cent) <- ka * depot - (cl / v) * cent - q * cent - p * cent
      cent ~ add(add.sd)
    })
  }
  .ui <- rxode2::rxUiDecompress(.f())

  ## every eta is at the id level, so nothing may be read as IOV
  expect_equal(unique(lotri::lotriBaseCondition(
    .ui$iniDf$condition[!is.na(.ui$iniDf$neta1)])), "id")
  expect_error(assertRxUiIovNoCor(.ui), NA)
  expect_error(assertRxUiRandomOnIdOnly(.ui), NA)

  ## and the model has ONE level, not one per mirrored element
  expect_equal(.ui$props$group,
               list(id = c("eta.ka", "iov.cl1", "iov.v1",
                           "iov.cl2", "iov.v2")))

  ## the omega comes back whole, with the repetition intact
  .om <- .ui$omega
  expect_equal(dim(.om), c(5L, 5L))
  expect_equal(attr(.om, "lotriSame"), c(0L, 0L, 0L, 2L, 2L))
  expect_equal(unclass(.om)[4:5, 4:5], unclass(.om)[2:3, 2:3],
               ignore_attr = TRUE)
})

test_that("omegaSameMap reports which etas repeat an earlier block", {

  .f <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      tq <- 1
      add.sd <- 0.7
      a + b ~ c(1,
                0.1, 2)
      c1 + d1 ~ same()
    })
    model({
      ka <- exp(tka + a)
      cl <- exp(tcl + b)
      v <- exp(tv + c1)
      q <- exp(tq + d1)
      d/dt(depot) <- -ka * depot
      d/dt(cent) <- ka * depot - (cl / v) * cent - q * cent
      cent ~ add(add.sd)
    })
  }
  .ui <- rxode2::rxUiDecompress(.f())

  ## 0 for a master eta, otherwise the eta it mirrors
  expect_equal(.ui$omegaSameMap, c(0L, 0L, 1L, 2L))

  ## a model with no repetition reports nothing, so callers can treat
  ## NULL as "everything is estimated"
  .g <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      add.sd <- 0.7
      a + b ~ c(1,
                0.1, 2)
    })
    model({
      ka <- exp(tka + a)
      cl <- exp(tcl + b)
      d/dt(depot) <- -ka * depot
      d/dt(cent) <- ka * depot - cl * cent
      cent ~ add(add.sd)
    })
  }
  expect_null(rxode2::rxUiDecompress(.g())$omegaSameMap)
})

test_that("rxSymInvCholCreate shares a repeated block's parameters", {

  ## For a block diagonal omega whose blocks are identical, inv(omega)
  ## and chol(inv(omega)) are block diagonal with identical factors, so
  ## a repeated block can reuse its master's slice of the parameter
  ## vector.  The derivative w.r.t. a shared parameter is then the block
  ## diagonal sum of the master's and the copy's contributions, which is
  ## exactly the chain rule -- no C++ change is needed.
  .m <- unclass(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  }))

  .u <- rxSymInvCholCreate(.m, diag.xform = "sqrt", create.env = FALSE)
  .s <- rxSymInvCholCreate(.m, diag.xform = "sqrt", create.env = FALSE,
                           same = c(0L, 0L, 1L, 2L))

  ## the repeated block costs no parameters of its own
  expect_equal(.u$fn(NULL, -2L), 6)
  expect_equal(.s$fn(NULL, -2L), 3)
  expect_equal(length(.s$ini), 3L)
  expect_equal(as.double(.s$ini), as.double(.u$ini)[1:3])

  .t3 <- as.double(.s$ini)
  .t6 <- as.double(.u$ini)

  ## the same matrix either way
  expect_equal(.s$fn(.t3, 0L), .u$fn(.t6, 0L))
  expect_equal(.s$fn(.t3, -1L), .u$fn(.t6, -1L))

  ## and the derivative sums both blocks
  for (.k in 1:3) {
    expect_equal(.s$fn(.t3, as.integer(.k)),
                 .u$fn(.t6, as.integer(.k)) +
                   .u$fn(.t6, as.integer(.k + 3L)))
  }

  ## which-thetas-are-diagonal is positional over the parameter vector,
  ## so it must shrink with it
  expect_length(.s$fn(NULL, NULL), 3L)
  expect_length(.u$fn(NULL, NULL), 6L)
})

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

rxTest({

  test_that("rxRename() follows a same() marker to the new name", {
    # The linkage is recorded BY NAME in the `condition` column, which is
    # what makes it survive renumbering -- but it means a rename has to be
    # followed too.  Left alone the marker points at a name that no longer
    # exists and `$omega` refuses to assemble.  nonmem2rx hits this on
    # every SAME model: it renames the etas to their NONMEM labels.
    .f <- function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45
        add.sd <- c(0, 0.7)
        a1 + a2 ~ c(0.1,
                    0.03, 0.2)
        b1 + b2 ~ same()
      })
      model({
        ka <- exp(tka + a1)
        cl <- exp(tcl + a2)
        v <- exp(tv + b1 + 0 * b2)
        linCmt() ~ add(add.sd)
      })
    }
    .ui <- suppressWarnings(rxode2(.f))
    .r <- suppressWarnings(suppressMessages(
      rxRename(.ui, IIV_CL = a1, IIV_V = a2)))
    .i <- .r$iniDf
    .cnd <- function(x) .i$condition[.i$name == x]
    expect_equal(.cnd("b1"), "id:same:IIV_CL")
    expect_equal(.cnd("b2"), "id:same:IIV_V")
    expect_equal(.cnd("(b1,b2)"), "id:same:IIV_CL:IIV_V")
    # the repetition still holds, and the omega assembles at all
    expect_equal(.r$omegaSameMap, c(0L, 0L, 1L, 2L))
    .om <- .r$omega
    expect_equal(unname(.om["b1", "b1"]), unname(.om["IIV_CL", "IIV_CL"]))
    expect_equal(unname(.om["b1", "b2"]), unname(.om["IIV_CL", "IIV_V"]))
    # renaming the COPY leaves the marker pointing at the untouched master
    .r2 <- suppressWarnings(suppressMessages(rxRename(.ui, IOV_CL = b1)))
    .i2 <- .r2$iniDf
    expect_equal(.i2$condition[.i2$name == "IOV_CL"], "id:same:a1")
    expect_false(inherits(try(.r2$omega, silent = TRUE), "try-error"))
  })

})
