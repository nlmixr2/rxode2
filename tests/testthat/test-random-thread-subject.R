rxTest({
  # The random-number functions a model calls take no subject argument; they
  # read the subject the calling thread is evaluating.  Draws in the
  # right-hand side (which give 0), in lhs, in an initial condition, the
  # per-subject draws, simeps() and simeta() must come out the same at any
  # number of cores, and as pinned.
  .rngModel <- rxode2({
    ka <- exp(tka + eta.ka)
    zi <- rinorm(0, 1)
    bi <- ribeta(2, 5)
    gi <- rigamma(3, 2)
    ui <- riunif(0, 1)
    depot(0) <- 10 * ui
    d/dt(depot) <- -ka * depot + rxnorm(0, 1)
    d/dt(center) <- ka * depot - 0.2 * center
    e <- center + err
    j <- 0
    while (e < 0) {
      j <- j + 1
      if (j > 100) break
      simeps()
      e <- center + err
    }
    y <- e + rxnorm(0, 0.1) + rxunif(0, 0.01) + rxbeta(2, 2) + rxgamma(2, 1)
  })

  .rngEtaModel <- rxode2({
    wt <- 70 * exp(eta.wt)
    i <- 0
    while ((wt < 60) || (wt > 80)) {
      i <- i + 1
      if (i > 100) break
      simeta()
      wt <- 70 * exp(eta.wt)
    }
    u <- rxunif(0, 1)
  })

  .rngSim <- function(cores) {
    rxWithSeed(
      1234,
      rxSolve(
        .rngModel,
        et(amt = 100, cmt = "depot") |> et(seq(0, 12, by = 2)),
        params = c(tka = log(1.2)),
        omega = lotri::lotri(eta.ka ~ 0.1),
        sigma = lotri::lotri(err ~ 4),
        nSub = 12,
        cores = cores,
        returnType = "data.frame"
      )
    )
  }

  .rngEtaSim <- function(cores) {
    rxWithSeed(
      1234,
      rxSolve(
        .rngEtaModel,
        et(1:3),
        omega = lotri::lotri(eta.wt ~ 0.3^2),
        nSub = 12,
        cores = cores,
        returnType = "data.frame"
      )
    )
  }

  test_that("model random draws read the subject each thread solves", {
    .s1 <- .rngSim(1L)
    expect_identical(.rngSim(4L), .s1)
    # the right-hand side draws nothing: depot is the exact decay from 10 ui + 100
    .d <- .s1[.s1$time > 0, ]
    expect_equal(.d$depot, (10 * .d$ui + 100) * exp(-.d$ka * .d$time), tolerance = 1e-5)
    .cols <- c("depot", "center", "zi", "bi", "gi", "ui", "e", "y")
    expect_equal(
      c(colSums(.s1[, .cols]), y1 = .s1$y[1], yLast = .s1$y[nrow(.s1)], j = sum(.s1$j)),
      c(
        depot = 1407.28206298879,
        center = 2611.37011819875,
        zi = 16.9627054533026,
        bi = 22.329059161767,
        gi = 108.976733503254,
        ui = 32.9049242048453,
        e = 2626.2620972838,
        y = 2841.70146549072,
        y1 = 3.20708403676612,
        yLast = 15.5320660464507,
        j = 11
      ),
      tolerance = 1e-10
    )
    # each subject keeps its own per-subject draws through its records
    for (.v in c("zi", "bi", "gi", "ui")) {
      expect_equal(as.vector(tapply(.s1[[.v]], .s1$sim.id, function(x) length(unique(x)))), rep(1L, 12), label = .v)
    }
    expect_equal(length(unique(.s1$zi)), 12L)
  })

  test_that("simeta() resamples for the subject each thread solves", {
    .e1 <- .rngEtaSim(1L)
    expect_identical(.rngEtaSim(4L), .e1)
    expect_true(all(.e1$wt >= 60 & .e1$wt <= 80))
    expect_equal(c(wt = sum(.e1$wt), u = sum(.e1$u)), c(wt = 2606.25180489536, u = 18.4739381949803), tolerance = 1e-10)
  })
})
