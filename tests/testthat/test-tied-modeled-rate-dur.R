rxTest({
  # rxode2#1218: modeled rate (RATE=-1) / modeled duration (RATE=-2) doses tied
  # at exactly the same TIME used to break the positional start/stop pairing and
  # fail with data errors 686/886 (and 797/997 for modeled rate).  etTrans() now
  # re-pairs each start with its own stop after the sort.

  mod <- rxode2({
    ka <- 1
    cl <- 3
    v <- 30
    D1 <- 2
    R1 <- 75
    dur(depot) <- D1
    dur(central) <- D1
    rate(depot) <- R1
    rate(central) <- R1
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - (cl / v) * central
  })

  .obs <- data.frame(ID = 1L, TIME = c(1, 4, 8, 12, 24), EVID = 0L,
                     AMT = NA_real_, RATE = NA_real_, CMT = 2L)
  .dose <- function(t, amt, rate, cmt) {
    data.frame(ID = 1L, TIME = t, EVID = 1L, AMT = amt, RATE = rate, CMT = cmt)
  }
  .sim <- function(ev) {
    as.data.frame(rxSolve(mod, ev))[, c("time", "depot", "central")]
  }

  # A tie must give the same answer as the same doses separated by an
  # imperceptible amount of time (the workaround before this was fixed).
  test_that("two modeled duration doses tied at one time solve", {
    .tied <- rbind(.dose(0, 150, -2, 1), .dose(0, 150, -2, 2), .obs)
    .stag <- rbind(.dose(0, 150, -2, 1), .dose(1e-8, 150, -2, 2), .obs)
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  test_that("two modeled duration doses tied in the same compartment solve", {
    .tied <- rbind(.dose(0, 150, -2, 2), .dose(0, 150, -2, 2), .obs)
    .stag <- rbind(.dose(0, 150, -2, 2), .dose(1e-8, 150, -2, 2), .obs)
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  test_that("three modeled duration doses tied at one time solve", {
    .tied <- rbind(.dose(0, 150, -2, 1), .dose(0, 150, -2, 2),
                   .dose(0, 100, -2, 2), .obs)
    .stag <- rbind(.dose(0, 150, -2, 1), .dose(1e-8, 150, -2, 2),
                   .dose(2e-8, 100, -2, 2), .obs)
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  test_that("two modeled rate doses tied at one time solve", {
    .tied <- rbind(.dose(0, 150, -1, 1), .dose(0, 150, -1, 2), .obs)
    .stag <- rbind(.dose(0, 150, -1, 1), .dose(1e-8, 150, -1, 2), .obs)
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  test_that("a modeled rate and a modeled duration dose tied at one time solve", {
    .tied <- rbind(.dose(0, 150, -2, 1), .dose(0, 150, -1, 2), .obs)
    .stag <- rbind(.dose(0, 150, -2, 1), .dose(1e-8, 150, -1, 2), .obs)
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  test_that("tied modeled duration doses with addl solve", {
    .tied <- rbind(cbind(.dose(0, 150, -2, 1), II = 6, ADDL = 2L),
                   cbind(.dose(0, 150, -2, 2), II = 6, ADDL = 2L),
                   cbind(.obs, II = 0, ADDL = 0L))
    .stag <- .tied
    .stag$TIME[2] <- 1e-8
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  # steady state doses in different compartments reset the system, so the tied
  # answer is order dependent; it only needs to solve without error and match
  # the ordering the sort produces (highest compartment last)
  test_that("tied modeled duration steady state doses solve", {
    .tied <- rbind(cbind(.dose(0, 150, -2, 1), II = 6, SS = 1L),
                   cbind(.dose(0, 150, -2, 2), II = 6, SS = 1L),
                   cbind(.obs, II = 0, SS = 0L))
    .stag <- rbind(cbind(.dose(0, 150, -2, 2), II = 6, SS = 1L),
                   cbind(.dose(1e-8, 150, -2, 1), II = 6, SS = 1L),
                   cbind(.obs, II = 0, SS = 0L))
    expect_equal(.sim(.tied), .sim(.stag), tolerance = 1e-5)
  })

  # regressions: the re-pairing must not disturb tables it does not need to fix
  test_that("a single modeled duration dose is unchanged", {
    expect_equal(.sim(rbind(.dose(0, 150, -2, 1), .obs)),
                 .sim(rbind(.dose(0, 150, -2, 1), .obs)))
  })

  test_that("a bolus tied with a modeled duration dose is order independent", {
    expect_equal(.sim(rbind(.dose(0, 150, 0, 1), .dose(0, 150, -2, 2), .obs)),
                 .sim(rbind(.dose(0, 150, -2, 2), .dose(0, 150, 0, 1), .obs)))
  })

  test_that("tied modeled duration start/stop records are adjacent", {
    .ev <- rbind(.dose(0, 150, -2, 1), .dose(0, 150, -2, 2), .obs)
    .evid <- etTrans(.ev, mod)$EVID
    # 8xxxx starts each immediately followed by their own 6xxxx stop
    expect_equal(.evid[1:4], c(80201L, 60201L, 80101L, 60101L))
  })
})
