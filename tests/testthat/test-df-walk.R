rxTest({
  # The output-record walk (rxDfWalkInit/rxDfWalkStep/rxDfWalkDull, slots
  # 105-107) is what rxode2_df() lays out its rows with.  Walked through the
  # pointer table as a downstream package walks it, it has to give rxSolve()'s
  # own rows and dose columns.
  .walkMod <- rxode2({
    ka <- 1.2
    cl <- 3
    v <- 30
    d / dt(depot) <- -ka * depot
    d / dt(center) <- ka * depot - cl / v * center
    cp <- center / v
  })

  .walkEvents <- list(
    bolus = et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 4)),
    addl = et(amt = 100, cmt = "depot", ii = 12, addl = 2) |> et(seq(0, 48, by = 6)),
    rate = et(amt = 100, rate = 50, cmt = "center") |> et(seq(0, 12, by = 2)),
    dur = et(amt = 100, dur = 3, cmt = "center") |> et(seq(0, 12, by = 2)),
    ss = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(seq(0, 12, by = 3)),
    evid2 = et(amt = 100, cmt = "depot") |> et(seq(0, 12, by = 4)) |> et(time = 5, evid = 2),
    reset = et(amt = 100, cmt = "depot") |> et(seq(0, 12, by = 4)) |>
      et(time = 13, evid = 3) |> et(amt = 50, time = 14, cmt = "depot") |> et(c(16, 20)),
    twoId = et(amt = 100, cmt = "depot", id = 1:2) |> et(seq(0, 12, by = 4), id = 1:2)
  )

  # addDosing/subsetNonmem -> rxSolve()'s code (rxSolve_ in rxData.cpp)
  .walkModes <- list(
    list(addDosing = TRUE, subsetNonmem = TRUE, code = 3L),
    list(addDosing = TRUE, subsetNonmem = FALSE, code = 2L),
    list(addDosing = NA, subsetNonmem = TRUE, code = 1L),
    list(addDosing = FALSE, subsetNonmem = TRUE, code = 0L)
  )

  .walkRows <- function(ev, code) {
    on.exit(rxSolveFree(), add = TRUE)
    rxSolve(.walkMod, ev, .setupOnly = 1L)
    .Call("_rxTestDfWalkSlots", .rxode2ptrs(), code, PACKAGE = "rxode2")
  }

  for (.ev in names(.walkEvents)) {
    for (.m in .walkModes) {
      test_that(paste0("the walk gives rxSolve()'s rows: ", .ev, ", addDosing=", .m$addDosing,
                       ", subsetNonmem=", .m$subsetNonmem), {
        .ref <- as.data.frame(rxSolve(.walkMod, .walkEvents[[.ev]],
          addDosing = .m$addDosing, subsetNonmem = .m$subsetNonmem,
          returnType = "data.frame"
        ))
        .w <- .walkRows(.walkEvents[[.ev]], .m$code)
        expect_equal(length(.w$time), nrow(.ref))
        expect_equal(.w$time, .ref$time)
        if (!is.null(.ref$resetno)) {
          expect_equal(as.integer(.w$resetno), as.integer(.ref$resetno))
        }
        if (!is.null(.ref$evid)) {
          expect_equal(as.integer(.w$evid), as.integer(.ref$evid))
        }
        for (.c in c("cmt", "ss")) {
          if (!is.null(.ref[[.c]])) {
            expect_equal(as.integer(.w[[.c]]), as.integer(.ref[[.c]]), label = .c)
          }
        }
        for (.c in c("amt", "rate", "dur", "ii")) {
          if (!is.null(.ref[[.c]])) {
            expect_equal(.w[[.c]], .ref[[.c]], label = .c)
          }
        }
        if (.m$code >= 2L) {
          # a dull (all-empty) column is the one rxSolve() drops
          .dull <- setNames(.w$dull == 1L, c("rate", "dur", "ss", "ii"))
          for (.c in names(.dull)) {
            expect_equal(.dull[[.c]], is.null(.ref[[.c]]), label = paste("dull", .c))
          }
        }
      })
    }
  }
  test_that("addDosing = TRUE keeps a string lhs and a factor covariate typed", {
    # the dose columns were allocated after the covariates but filled before
    # time, so every later column was written three positions off: a factor
    # column (string lhs, factor covariate) crashed the solve
    .m <- rxode2({
      ka <- 1.2
      cl <- 3
      v <- 30
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      grp <- "low"
      if (cp > 1) grp <- "high"
      sexEff <- SEX
    })
    .d <- data.frame(
      id = 1, time = c(0, 1, 2, 4, 8), evid = c(1, 0, 0, 0, 0),
      amt = c(100, 0, 0, 0, 0), cmt = c(1, 2, 2, 2, 2),
      SEX = factor(c("F", "M", "F", "M", "F"))
    )
    .obs <- as.data.frame(rxSolve(.m, .d, addDosing = FALSE))
    for (.sn in c(TRUE, FALSE)) {
      .all <- as.data.frame(rxSolve(.m, .d, addDosing = TRUE, subsetNonmem = .sn))
      expect_s3_class(.all$grp, "factor")
      expect_identical(levels(.all$grp), levels(.obs$grp))
      .w <- .all$evid == 0
      expect_identical(.all$grp[.w], .obs$grp)
      expect_equal(.all$cp[.w], .obs$cp)
      expect_equal(.all$time[.w], .obs$time)
      expect_identical(.all$SEX[.w], .obs$SEX)
    }
  })
})
