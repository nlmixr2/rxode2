rxTest({
  # NONMEM anchors from ferx-core (FeRx-NLME/ferx-core@872826c6,
  # nonmem_anchor/reset_init_snapshot_*): A_0(1) = 10*WT, CL = 5, V = 50; at an
  # evid=3/4 reset cmt(0) reads the reset record's covariates (rxode2#1587)
  .resetCovMod <- function() {
    ini({
      tcl <- 5
      tv <- 50
      tbase <- 10
      tocc <- 3
    })
    model({
      occf <- 1
      if (OCC == 2) occf <- tocc
      central(0) <- tbase * WT * occf
      d / dt(central) <- -tcl / tv * central
      cp <- central / tv
    })
  }

  .resetCovData <- function(time, evid, wt, amt = 0, occ = 1) {
    data.frame(
      ID = 1,
      TIME = time,
      EVID = evid,
      AMT = amt,
      CMT = 1,
      WT = wt,
      OCC = occ
    )
  }

  .resetCovSolve <- function(d) {
    .r <- rxSolve(.resetCovMod, d, returnType = "data.frame", addDosing = FALSE)
    .r$cp
  }

  .t0 <- c(0, 1, 2, 4, 6)
  .e0 <- c(0, 0, 0, 1, 0)
  .a0 <- c(0, 0, 0, 100, 0)
  .p0 <- c(14, 12.667723840, 11.462230521, 9.3208243882)

  test_that("evid=3 reset re-seeds cmt(0) with the reset record's covariates (A)", {
    d <- .resetCovData(
      c(.t0, 8, 9, 10, 12, 16),
      c(.e0, 3, 0, 0, 0, 0),
      c(rep(70, 5), rep(140, 5)),
      c(.a0, rep(0, 5))
    )
    expect_equal(
      .resetCovSolve(d),
      c(.p0, 25.335447691, 22.924461062, 18.768961267, 12.581210973),
      tolerance = 1e-5
    )
  })

  test_that("evid=3 reset with flat covariates is unchanged (B)", {
    d <- .resetCovData(
      c(.t0, 8, 9, 10, 12, 16),
      c(.e0, 3, 0, 0, 0, 0),
      70,
      c(.a0, rep(0, 5))
    )
    expect_equal(
      .resetCovSolve(d),
      c(.p0, 12.667723846, 11.462230531, 9.3844806337, 6.2906054867),
      tolerance = 1e-5
    )
  })

  test_that("evid=3 reset uses its own record, not the next one (C)", {
    d <- .resetCovData(
      c(.t0, 8, 9, 10, 12, 16),
      c(.e0, 3, 0, 0, 0, 0),
      c(rep(70, 5), 140, rep(200, 4)),
      c(.a0, rep(0, 5))
    )
    expect_equal(
      .resetCovSolve(d),
      c(.p0, 25.335447691, 22.924461062, 18.768961267, 12.581210973),
      tolerance = 1e-5
    )
  })

  test_that("each evid=3 reset uses its own record's covariates (E)", {
    d <- .resetCovData(
      c(.t0, 8, 9, 10, 12, 13, 14, 16),
      c(.e0, 3, 0, 0, 3, 0, 0, 0),
      c(rep(70, 5), rep(140, 3), rep(200, 4)),
      c(.a0, rep(0, 7))
    )
    expect_equal(
      .resetCovSolve(d),
      c(
        .p0,
        25.335447691,
        22.924461062,
        36.193496708,
        32.749230100,
        26.812801823
      ),
      tolerance = 1e-5
    )
  })

  test_that("evid=3 reset uses its own record, not a co-timed dose's (G)", {
    d <- .resetCovData(
      c(.t0, 8, 8, 9, 10, 12, 16),
      c(.e0, 3, 1, 0, 0, 0, 0),
      c(rep(70, 5), 140, rep(90, 5)),
      c(.a0, 0, 100, rep(0, 4))
    )
    expect_equal(
      .resetCovSolve(d),
      c(.p0, 27.145122526, 24.561922567, 20.109601358, 13.479868900),
      tolerance = 1e-5
    )
  })

  test_that("evid=3 reset uses the occasion on its own record (J)", {
    d <- .resetCovData(
      c(0, 2, 4, 6, 8, 9, 12, 16),
      c(0, 0, 1, 0, 3, 0, 0, 0),
      70,
      c(0, 0, 100, rep(0, 5)),
      c(rep(1, 4), rep(2, 4))
    )
    expect_equal(
      .resetCovSolve(d),
      c(
        14,
        11.462230536,
        9.3208243980,
        38.003171537,
        28.153441913,
        18.871816468
      ),
      tolerance = 1e-5
    )
  })
})
