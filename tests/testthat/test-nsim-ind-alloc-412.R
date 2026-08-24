rxTest({
  # rxSolve_datSetupHmax() sized the per-individual solve pool (inds_global)
  # as nsub*nPopPar instead of the nsub*nsim (== nPopPar) it actually needs.
  # For a large study that either exhausted memory or overflowed the int and
  # reported "nothing to solve" -- which is what made addNpde()/vpcSim() fail
  # on a real data set (nlmixr2/nlmixr2#412).  33000 subjects x nsim 2 puts
  # the old size (33000 * 66000) past INT_MAX while the true pool is 66000.
  test_that("a many-subject nsim solve does not over-size the individual pool", {
    m <- rxode2({
      ka <- 1
      cl <- 1
      v <- 20
      cp <- linCmt()
      sim <- cp + eta.cl
    })
    .n <- 33000L
    .d <- data.frame(ID=rep(seq_len(.n), each=2L),
                     TIME=rep(c(0, 4), .n),
                     AMT=rep(c(100, 0), .n),
                     EVID=rep(c(1L, 0L), .n))
    s <- rxSolve(m, .d, omega=lotri::lotri(eta.cl ~ 0.1), nsim=2,
                 returnType="data.frame")
    expect_equal(nrow(s), 2L * .n)
    expect_equal(sort(unique(s$sim.id)), 1:2)
    expect_true(all(is.finite(s$sim)))
  })

  test_that("a params data.frame of nsub*nsim rows solves as nsim replicates", {
    # exercises the nid != nPopPar branch that was over-sizing the pool
    m <- rxode2({
      cl <- exp(tcl)
      v <- exp(tv)
      ka <- exp(tka)
      cp <- linCmt()
      sim <- cp
    })
    .d <- data.frame(ID=rep(1:4, each=3L),
                     TIME=rep(c(0, 1, 4), 4),
                     AMT=rep(c(100, 0, 0), 4),
                     EVID=rep(c(1L, 0L, 0L), 4))
    .p1 <- data.frame(tka=seq(0.4, 0.7, length.out=4),
                      tcl=1, tv=3.5)
    .p3 <- do.call(rbind, lapply(1:3, function(.i) {
      .p1$tcl <- 1 + 0.1 * .i
      .p1
    }))
    s3 <- rxSolve(m, .p3, .d, returnType="data.frame")
    expect_equal(sort(unique(s3$sim.id)), 1:3)
    for (.i in 1:3) {
      .p <- .p1
      .p$tcl <- 1 + 0.1 * .i
      s1 <- rxSolve(m, .p, .d, returnType="data.frame")
      expect_equal(s3$sim[s3$sim.id == .i], s1$sim)
    }
  })

  test_that("an empty data set still refuses to solve", {
    m <- rxode2({
      cl <- 1
      v <- 20
      ka <- 1
      cp <- linCmt()
    })
    .d <- data.frame(ID=integer(0), TIME=numeric(0), AMT=numeric(0),
                     EVID=integer(0))
    expect_error(rxSolve(m, .d))
  })
})
