rxTest({

  .rx <- loadNamespace("rxode2")

  ## The `prior` column only exists when the installed 'lotri' supports
  ## prior distributions, so the real-syntax tests are gated on it.  The
  ## rest fabricate the column by hand, the way test-assert-priors.R does,
  ## so they still run against an older 'lotri'.
  .hasPriorSupport <- function() {
    exists("lotriPriorDists", envir=asNamespace("lotri"), inherits=FALSE)
  }

  .withPrior <- function(ui, name, prior) {
    ui <- rxUiDecompress(ui)
    .ini <- ui$iniDf
    if (!any(names(.ini) == "prior")) .ini$prior <- NA_character_
    .ini$prior[match(name, .ini$name)] <- prior
    assign("iniDf", .ini, envir=ui)
    ui
  }

  .base <- function() {
    rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl + eta.v ~ c(0.3,
                           0.01, 0.1)
        eta.ka ~ 0.6
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    })
  }

  test_that("a model with no priors has no spec", {
    expect_null(.rx$.rxPriorSimSpec(.base(), list()))
  })

  test_that("a normal prior becomes a one entry thetaMat", {
    .u <- .withPrior(.base(), "tka", "dnorm(0.45, 0.1)")
    .s <- .rx$.rxPriorSimSpec(.u, list())

    expect_equal(dimnames(.s$thetaMat), list("tka", "tka"))
    ## the variance is the square of the sd the prior gives
    expect_equal(unname(.s$thetaMat[1, 1]), 0.01)
    expect_equal(.s$theta$est, 0.45)
    expect_equal(length(.s$omegaNu), 0L)
  })

  test_that("a multivariate normal prior becomes a block of the thetaMat", {
    .u <- .withPrior(.base(), c("tcl", "tv"),
                     "multiNormal(c(1, 3.45), lotri(tcl + tv ~ c(0.02, 0.001, 0.03)))")
    .s <- .rx$.rxPriorSimSpec(.u, list())

    expect_equal(dimnames(.s$thetaMat), list(c("tcl", "tv"), c("tcl", "tv")))
    expect_equal(unname(.s$thetaMat),
                 matrix(c(0.02, 0.001, 0.001, 0.03), 2, 2))
  })

  test_that("independent priors give a block diagonal thetaMat", {
    .u <- .withPrior(.base(), "tka", "dnorm(0.45, 0.1)")
    .u <- .withPrior(.u, c("tcl", "tv"),
                     "multiNormal(c(1, 3.45), lotri(tcl + tv ~ c(0.02, 0.001, 0.03)))")
    .s <- .rx$.rxPriorSimSpec(.u, list())

    expect_equal(dimnames(.s$thetaMat)[[1]], c("tka", "tcl", "tv"))
    ## the two priors say nothing about each other, so the off diagonal
    ## between them is zero
    expect_equal(unname(.s$thetaMat[1, 2:3]), c(0, 0))
    expect_equal(unname(.s$thetaMat[2, 3]), 0.001)
  })

  test_that("the prior mean has to be the initial estimate", {
    ## prior simulation samples around what the model says the parameter
    ## is, so a prior centered anywhere else is an error rather than a
    ## silently different simulation
    .u <- .withPrior(.base(), "tka", "dnorm(0, 2)")
    expect_error(.rx$.rxPriorSimSpec(.u, list()), "is not the initial estimate")

    .u <- .withPrior(.base(), c("tcl", "tv"),
                     "multiNormal(c(9, 3.45), lotri(tcl + tv ~ c(0.02, 0.001, 0.03)))")
    expect_error(.rx$.rxPriorSimSpec(.u, list()), "is not the initial estimate")
  })

  test_that("each omega block keeps its own degrees of freedom", {
    .u <- .withPrior(.base(), "eta.cl", "invWishart(20)")
    .u <- .withPrior(.u, "eta.ka", "invWishart(4)")
    .s <- .rx$.rxPriorSimSpec(.u, list())

    expect_equal(length(.s$omegaNu), 2L)
    expect_equal(.s$omegaNu[[1]]$names, c("eta.cl", "eta.v"))
    expect_equal(.s$omegaNu[[1]]$nu, 20)
    expect_equal(.s$omegaNu[[2]]$names, "eta.ka")
    expect_equal(.s$omegaNu[[2]]$nu, 4)
  })

  test_that("an improper inverse Wishart on a block is an error", {
    ## a 2x2 block needs more than 1 degree of freedom; 'lotri' checks
    ## this when the prior is written, but a piped model can dodge it
    .u <- .withPrior(.base(), "eta.cl", "invWishart(1)")
    expect_error(.rx$.rxPriorSimSpec(.u, list()), "degrees of freedom")
  })

  test_that("a distribution that cannot be simulated from is an error", {
    ## a prior must never be silently ignored
    .u <- .withPrior(.base(), "tka", "dgamma(2, 1)")
    expect_error(.rx$.rxPriorSimSpec(.u, list()),
                 "only normal and multivariate normal")
  })

  test_that("a normal prior on the omega values reaches the spec", {
    ## a NONMEM TNPRI, which lands on the omega row.  `eta.ka` is 0.6, and
    ## an omega element prior is centered on the omega value the same way
    ## a theta one is centered on its estimate
    .u <- .withPrior(.base(), "eta.ka", "dnorm(0.6, 0.1)")
    .s <- .rx$.rxPriorSimSpec(.u, list())

    expect_equal(dimnames(.s$thetaMat)[[1]], "om.eta.ka")
    expect_equal(unname(.s$thetaMat[1, 1]), 0.01)
    expect_equal(.s$omegaEl$name, "om.eta.ka")

    ## and the joint form, which lands wherever the block starts
    .u <- .withPrior(.base(), "tcl",
                     "multiNormal(c(1, 0.6), lotri(tcl + om.eta.ka ~ c(0.02, 0.001, 0.03)))")
    .s <- .rx$.rxPriorSimSpec(.u, list())

    expect_equal(dimnames(.s$thetaMat)[[1]], c("tcl", "om.eta.ka"))
    expect_equal(.s$omegaEl$name, "om.eta.ka")
  })

  test_that("an omega element prior is centered on the omega value", {
    ## the value the draw is added to is the omega, not zero
    .u <- .withPrior(.base(), "eta.ka", "dnorm(0, 0.1)")
    expect_error(.rx$.rxPriorSimSpec(.u, list()), "is not the initial estimate")
  })

  test_that("a joint block spans the thetas and the omega elements", {
    ## a TNPRI variance matrix covers both, with covariances between them
    .u <- .withPrior(.base(), "tcl",
                     "multiNormal(c(1, 0.6), lotri(tcl + om.eta.ka ~ c(0.02, 0.001, 0.03)))")
    .th <- .rx$.rxPriorThetaMat(.u)

    expect_equal(dimnames(.th$thetaMat)[[1]], c("tcl", "om.eta.ka"))
    expect_equal(unname(.th$thetaMat["tcl", "om.eta.ka"]), 0.001)
    ## and the omega member is mapped back to where it lives in the omega
    expect_equal(.th$omegaEl$name, "om.eta.ka")
    expect_equal(.th$omegaEl$neta1, 3L)
    expect_equal(.th$omegaEl$neta2, 3L)
  })

  test_that("a chunked solve with priors is an error", {
    ## the chunked path pre-draws its parameters and strips the omega
    ## from each chunk, so the prior would never be drawn from
    .u <- .withPrior(.base(), "tka", "dnorm(0.45, 0.1)")
    expect_error(.rx$.rxPriorSimSpec(.u, list(chunkSize=1e5)), "chunked solve")
    expect_error(.rx$.rxPriorSimSpec(.u, list(file="out.parquet")),
                 "chunked solve")
  })

  test_that("a nested or occasion model with priors is an error", {
    skip_if_not(.hasPriorSupport())

    .u <- rxode2(function() {
      ini({
        tcl <- 1
        add.sd <- 0.7
        eta.cl ~ 0.3 | id
        eta.o ~ 0.1 | occ
        prior(eta.cl) ~ invWishart(4)
      })
      model({
        ka <- 1
        cl <- exp(tcl + eta.cl + eta.o)
        v <- 1
        linCmt() ~ add(add.sd)
      })
    })

    expect_error(.rx$.rxPriorSimSpec(.u, list()), "nested/occasion")
  })

  test_that("the ini({}) syntax reaches the spec end to end", {
    skip_if_not(.hasPriorSupport())

    .u <- rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl + eta.v ~ c(0.3,
                           0.01, 0.1)
        eta.ka ~ 0.6
        add.sd <- 0.7
        tka ~ 0.01
        prior(eta.cl, eta.v) ~ invWishart(20)
        prior(eta.ka) ~ invWishart(4)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    })

    .s <- .rx$.rxPriorSimSpec(.u, list())

    ## `tka ~ 0.01` is the shorthand: mean is the estimate, and the
    ## number is the variance
    expect_equal(dimnames(.s$thetaMat), list("tka", "tka"))
    expect_equal(unname(.s$thetaMat[1, 1]), 0.01)
    expect_equal(vapply(.s$omegaNu, function(x) x$nu, double(1)), c(20, 4))
  })

  test_that("thetaMat columns are matched to omega entries by every spelling", {
    ## A thetaMat from a real covariance step carries the omega entries
    ## next to the thetas, and every producer spells them differently.
    .om <- matrix(0, 3, 3,
                  dimnames=list(c("eta.cl", "eta.v", "eta.ka"),
                                c("eta.cl", "eta.v", "eta.ka")))

    ## nlmixr2est: `om.<eta>` and `cov.<eta1>.<eta2>` (.foceiOmegaCovNames)
    .e <- .rx$.rxJointElFromNames(.om, c("tka", "om.eta.cl",
                                         "cov.eta.cl.eta.v", "om.eta.v"))
    expect_equal(rownames(.e),
                 c("om.eta.cl", "cov.eta.cl.eta.v", "om.eta.v"))
    expect_equal(unname(.e[, "neta1"]), c(1L, 2L, 2L))
    expect_equal(unname(.e[, "neta2"]), c(1L, 1L, 2L))

    ## nonmem2rx: the bare eta name, and `omega<i>.<j>`/`omega.<i>.<j>`
    .e <- .rx$.rxJointElFromNames(.om, c("t.CL", "eta.cl", "omega1.2",
                                         "eta.v", "omega.3.1"))
    expect_equal(rownames(.e),
                 c("eta.cl", "omega1.2", "eta.v", "omega.3.1"))
    ## either order addresses the same entry, since the matrix is symmetric
    expect_equal(unname(.e["omega.3.1", ]), c(3L, 1L))

    ## a thetaMat with nothing to match is not an omega source
    expect_null(.rx$.rxJointElFromNames(.om, c("tka", "tcl")))

    ## the bare eta name already means an SD under the separation
    ## strategy, so it can be excluded
    .e <- .rx$.rxJointElFromNames(.om, c("eta.cl", "om.eta.v"),
                                  bareName=FALSE)
    expect_equal(rownames(.e), "om.eta.v")

    ## two spellings of one entry is ambiguous rather than first-wins
    expect_error(.rx$.rxJointElFromNames(.om, c("om.eta.cl", "eta.cl")),
                 "more than one")
  })

  test_that("sigma entries are matched the same way", {
    .sg <- matrix(0, 2, 2, dimnames=list(c("eps1", "eps2"),
                                         c("eps1", "eps2")))
    .e <- .rx$.rxJointElFromNames(.sg, c("eps1", "sigma1.2", "eps2"),
                                  what="sigma")
    expect_equal(rownames(.e), c("eps1", "sigma1.2", "eps2"))
    expect_equal(unname(.e["sigma1.2", ]), c(2L, 1L))
  })
})
