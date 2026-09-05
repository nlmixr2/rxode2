rxTest({
  # Mu-referencing the parameters of a declared distribution.
  #
  # rxEtaDistExpand() writes them as bare thetas inside an inverse-CDF call, so
  # every one of them comes out NON-mu-referenced -- the case saem and the FOCEi
  # family handle worst.  Bauer's NONMEM streams put all five on MU_5..MU_9 with
  # a zero-variance helper eta; this does the same thing, with a non-degenerate
  # variance (see the test below for why).

  .muMod <- function() {
    .f <- function() {
      ini({
        lclm  <- log(5)
        lclrv <- log(0.09)
        lv1m  <- log(4.7)
        lv1rv <- log(0.09)
        tq <- 0.9
        tv2 <- 4.2
        eta.cl + eta.v1 ~ c(1,
                            0.5, 1)
        dist(eta.cl) ~ dgamma(shape=1/exp(lclrv), rate=1/(exp(lclrv)*exp(lclm)))
        dist(eta.v1) ~ dgamma(shape=1/exp(lv1rv), rate=1/(exp(lv1rv)*exp(lv1m)))
        eta.q ~ 0.1
        prop.sd <- 0.1
      })
      model({
        cl <- eta.cl
        v <- eta.v1
        q <- exp(tq + eta.q)
        v2 <- exp(tv2)
        linCmt() ~ prop(prop.sd)
      })
    }
    .f()
  }

  .noDist <- function() {
    .f <- function() {
      ini({
        tcl <- 1
        tv <- 3
        eta.cl ~ 0.1
        add.sd <- 1
      })
      model({
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        linCmt() ~ add(add.sd)
      })
    }
    .f()
  }

  test_that("rxEtaDistMuRef() mu-references every declared-distribution parameter", {
    .u <- rxEtaDistMuRef(.muMod(), variance=0.1)
    .want <- c("lclm", "lclrv", "lv1m", "lv1rv", "rxCor.eta.v1.eta.cl")

    # All five, including the copula correlation.  The correlation only ever
    # appears on a generated tanh() line, so it is the one that gets silently
    # left behind if the generated-line detection is too narrow -- and it is
    # exactly NONMEM's MU_9.
    expect_setequal(intersect(.u$muRefTable$theta, .want), .want)

    .h <- .u$iniDf[grepl("^eta\\.mu\\.", .u$iniDf$name), ]
    expect_equal(nrow(.h), length(.want))
    expect_true(all(.h$fix))
    expect_true(all(.h$est == 0.1))

    # The model's own structural parameters keep the mu reference they already
    # had, and gain no helper.
    expect_true("tq" %in% .u$muRefTable$theta)
    expect_false(any(grepl("eta\\.mu\\.tq", .u$iniDf$name)))
  })

  test_that("rxEtaDistMuRef() refuses a degenerate helper variance", {
    # nlmixr2's mu-theta M-step is weighted by omega^-1, so a ~0 helper
    # variance pins the parameter at its ini() value instead of freeing it.
    # NONMEM's EM updates such a parameter by direct maximization, which is why
    # its "$OMEGA (0.0 FIXED)" idiom works there and must not be copied here.
    expect_error(rxEtaDistMuRef(.muMod(), variance=1e-9),
                 "meaningfully above zero")
  })

  test_that("rxEtaDistMuRef() needs something declared to work on", {
    expect_error(rxEtaDistMuRef(.noDist()), "dist\\(\\)")
  })
})
