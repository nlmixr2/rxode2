rxTest({
  test_that("rxFlag works", {
    
    mod2 <- rxode2({
      a <- 6
      b <- 0.6
      d / dt(intestine) <- -a * intestine
      lag(intestine) <- 2
      d / dt(blood) <- a * intestine - b * blood
      printf("%d\n", rxFlag)
      flag <- rxFlag
    }, fullPrint=FALSE)
    
    obs <- units::set_units(seq(0, 10, by = 1 / 24), "days")

    et <- eventTable(time.units = "days")
    et$add.sampling(obs)
    et$add.dosing(
      dose = 2 / 24, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    sink("flag.csv")
    cat("flag\n")
    solve2 <- solve(mod2, et)
    sink()
    f <- read.csv("flag.csv")
    unlink("flag.csv")

    expect_true(all(solve2$flag == 11))
    expect_true(all(f$flag == 1))

    mod3 <- rxode2({
      a <- 6
      b <- 0.6
      d / dt(intestine) <- -a * intestine
      lag(intestine) <- 2
      d / dt(blood) <- a * intestine - b * blood
      printf("%d\n", rxFlag)
      flag <- rxFlag
    }, fullPrint=TRUE)

    sink("flag.csv")
    cat("flag\n")
    solve3 <- solve(mod3, et)
    sink()
    f3 <- read.csv("flag.csv")
    unlink("flag.csv")

    expect_true(all(solve2$flag == 11))
    expect_equal(sort(unique(f3$flag)), c(1, 5, 11))

  })
})
