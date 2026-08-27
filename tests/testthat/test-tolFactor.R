rxTest({
  .mod <- rxode2({
    d/dt(intestine) <- -a * intestine
    d/dt(blood) <- a * intestine - b * blood
  })

  .et <- eventTable()
  .et$add.sampling(seq(0, 10, length.out = 10))
  .et$add.dosing(dose = 1, strt.time = 0)

  # Build a 4-subject event table (id column required for nsub > 1)
  .ev <- do.call(rbind, lapply(1:4, function(i) {
    d <- as.data.frame(.et)
    d$id <- i
    d
  }))

  .p <- data.frame(id = 1:4, a = 6, b = seq(0.4, 0.9, length.out = 4))

  test_that("tolFactor=NULL (default) solves without error", {
    expect_error(rxSolve(.mod, .p, .ev, tolFactor = NULL), NA)
  })

  test_that("tolFactor scalar >= 1 solves without error", {
    expect_error(rxSolve(.mod, .p, .ev, tolFactor = 10), NA)
  })

  test_that("tolFactor vector (one per subject) solves without error", {
    expect_error(rxSolve(.mod, .p, .ev, tolFactor = c(1, 2, 5, 10)), NA)
  })

  test_that("tolFactor named vector matched by subject ID solves without error", {
    expect_error(rxSolve(.mod, .p, .ev, tolFactor = c("2" = 5, "4" = 10)), NA)
  })

  test_that("tolFactor < 1 is rejected", {
    expect_error(rxSolve(.mod, .p, .ev, tolFactor = 0.5))
  })

  test_that("$tolFactor returns per-subject factors used in the solve", {
    .out <- rxSolve(.mod, .p, .ev, tolFactor = c(1, 2, 5, 10))
    .tf <- .out$tolFactor
    expect_length(.tf, 4)
    expect_equal(as.numeric(.tf), c(1, 2, 5, 10))
  })

  test_that("named tolFactor: matched subjects get factor, unmatched stay at 1", {
    .out <- rxSolve(.mod, .p, .ev, tolFactor = c("2" = 5, "4" = 10))
    .got <- as.numeric(.out$tolFactor)
    expect_equal(.got[1], 1)
    expect_equal(.got[2], 5)
    expect_equal(.got[3], 1)
    expect_equal(.got[4], 10)
  })

  test_that("one subject's tolerance factor does not leak onto the next subject", {
    # Solved sequentially on one thread, subject 1 first.  The per-thread
    # tolerance arrays used to be scaled in place, so subject 1's loosening was
    # still in them when subjects 2-4 were solved; they are now rebuilt from the
    # solve's base tolerances every time.
    .cols <- c("intestine", "blood")
    .base <- as.data.frame(rxSolve(.mod, .p, .ev, tolFactor = NULL, cores = 1))
    .loose <- as.data.frame(rxSolve(.mod, .p, .ev, tolFactor = c(1e8, 1, 1, 1),
                                    cores = 1))
    expect_equal(.base[.base$id != 1, .cols], .loose[.loose$id != 1, .cols])
    # ...and subject 1 itself really was solved at the looser tolerance, so the
    # comparison above is not vacuous
    expect_false(isTRUE(all.equal(.base[.base$id == 1, .cols],
                                  .loose[.loose$id == 1, .cols])))
  })

  test_that("tolFactor=1 for all subjects does not change results vs NULL", {
    .base <- rxSolve(.mod, .p, .ev, tolFactor = NULL)
    .tf1  <- rxSolve(.mod, .p, .ev, tolFactor = rep(1, 4))
    expect_equal(
      as.data.frame(.base)[, c("id", "time", "intestine", "blood")],
      as.data.frame(.tf1)[, c("id", "time", "intestine", "blood")]
    )
  })
})
