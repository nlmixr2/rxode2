rxTest({
  # A homogeneous event table stores one representative subject per group, so
  # the residual draw used to be sized from that single subject (issue #1341).
  .m1341 <- rxode2({
    d/dt(depot)  <- -ka * depot
    d/dt(center) <-  ka * depot - cl / v * center
    cp <- center / v
    y  <- cp + err
  })
  .p1341 <- c(ka = 1.5, cl = 2.7, v = 31.5)
  .s1341 <- lotri(err ~ 0.25)
  .t1341 <- c(0.25, 1, 4, 12, 24)

  # Solve and report both the output and the number of rows actually drawn for
  # the residual.  The drawn matrix is checked directly because the solve reads
  # it with an index clamped at the last row: too few rows repeat silently, and
  # too many are silently ignored, so the simulated values alone cannot see
  # either.
  .solve1341 <- function(ev, seed = 1, ...) {
    .r <- withr::with_seed(seed, {
      suppressWarnings(rxSolve(.m1341, ev, .p1341, sigma = .s1341, ...))
    })
    list(df = as.data.frame(.r),
         sigmaRows = nrow(attr(class(.r), ".rxode2.env")$.sigma))
  }

  test_that("sigma is simulated for every subject that comes from et(id=) (#1341)", {
    .b <- .solve1341(et(amt = 320) |> et(.t1341) |> et(id = 1:6),
                     addDosing = FALSE)
    expect_equal(nrow(.b$df), 6L * length(.t1341))
    # one draw per observation row, not one subject's worth recycled
    expect_equal(length(unique(.b$df$y - .b$df$cp)), nrow(.b$df))
    expect_equal(.b$sigmaRows, nrow(.b$df))

    # ... and it matches the nSub= path exactly
    .a <- .solve1341(et(amt = 320) |> et(.t1341), nSub = 6, addDosing = FALSE)
    expect_equal(.b$df$y - .b$df$cp, .a$df$y - .a$df$cp)
  })

  # The row count `curObs` reads differs per `addDosing` -- `nobs2` (evid=0
  # only) for NULL, `nobs` (observations, so evid=2 as well) for FALSE and
  # `nall` (every record) for TRUE/NA -- so all four have to be expanded.  What
  # the fix guarantees is that n identical subjects draw exactly n times what
  # one of them draws, which catches an under- and an over-sized draw alike.
  test_that("every addDosing branch draws n times the single-subject count (#1341)", {
    .shapes <- list(
      plain = et(amt = 320) |> et(.t1341),
      evid2 = et(amt = 320) |> et(.t1341) |> et(time = 2, evid = 2),
      addl  = et(time = 0, amt = 320, addl = 2, ii = 12) |> et(.t1341),
      ss    = et(time = 0, amt = 320, ii = 12, ss = 1) |> et(.t1341),
      # no record at time 0, so etTrans adds an evid=9 ini record per subject;
      # those take no residual draw
      evid9 = et(c(1, 2, 4, 8))
    )
    for (.nm in names(.shapes)) {
      for (.ad in list(NULL, FALSE, TRUE, NA)) {
        .lbl <- paste0(.nm, "/addDosing=", if (is.null(.ad)) "NULL" else .ad)
        .one <- .solve1341(.shapes[[.nm]] |> et(id = 1L), addDosing = .ad)
        .many <- .solve1341(.shapes[[.nm]] |> et(id = 1:4), addDosing = .ad)
        expect_equal(.many$sigmaRows, 4L * .one$sigmaRows, label = .lbl)
        expect_equal(nrow(.many$df), 4L * nrow(.one$df), label = .lbl)
        expect_equal(length(unique(.many$df$y - .many$df$cp)), nrow(.many$df),
                     label = .lbl)
      }
    }
  })

  test_that("sigma expands per group when the groups differ (#1341)", {
    .ev <- rbind(et(amt = 320) |> et(.t1341) |> et(id = 1:3),
                 et(amt = 100) |> et(.t1341) |> et(id = 4:5))
    .b <- .solve1341(.ev, seed = 4, addDosing = FALSE)
    expect_equal(length(unique(.b$df$y - .b$df$cp)), nrow(.b$df))
    expect_equal(.b$sigmaRows, nrow(.b$df))
  })

  test_that("the expansion holds across studies as well (#1341)", {
    .b <- .solve1341(et(amt = 320) |> et(.t1341) |> et(id = 1:4),
                     seed = 9, nStud = 3, addDosing = FALSE)
    expect_equal(nrow(.b$df), 3L * 4L * length(.t1341))
    expect_equal(.b$sigmaRows, nrow(.b$df))
    expect_equal(length(unique(.b$df$y - .b$df$cp)), nrow(.b$df))
  })

  test_that("omega is drawn per subject from et(id=) as well (#1341)", {
    # omega was never part of the bug; pin that, so a later change to the
    # expanded counts cannot quietly take the between-subject draws with it
    .mo <- rxode2({
      cli <- cl * exp(eta.cl)
      d/dt(depot)  <- -ka * depot
      d/dt(center) <-  ka * depot - cli / v * center
      cp <- center / v
      y  <- cp + err
    })
    .b <- withr::with_seed(8, {
      suppressWarnings(rxSolve(.mo, et(amt = 320) |> et(.t1341) |> et(id = 1:6),
                               .p1341, sigma = .s1341,
                               omega = lotri(eta.cl ~ 0.3), addDosing = FALSE))
    })
    .df <- as.data.frame(.b)
    # one cli per subject, six distinct subjects
    expect_equal(length(unique(.df$cli)), 6L)
    # ... and the residual is still one draw per row
    expect_equal(length(unique(.df$y - .df$cp)), nrow(.df))
    expect_equal(nrow(attr(class(.b), ".rxode2.env")$.sigma), nrow(.df))
  })

  test_that("a non-homogeneous multi-subject data set still draws one sigma per row (#1341)", {
    .ev <- rbind(data.frame(id = 1, time = c(0, 1, 2, 3), amt = c(320, NA, NA, NA),
                            evid = c(1, 0, 0, 0)),
                 data.frame(id = 2, time = c(0, 1, 2, 3, 8), amt = c(100, NA, NA, NA, NA),
                            evid = c(1, 0, 0, 0, 0)))
    .b <- .solve1341(.ev, seed = 3, addDosing = FALSE)
    expect_equal(length(unique(.b$df$y - .b$df$cp)), nrow(.b$df))
    expect_equal(.b$sigmaRows, nrow(.b$df))
  })
})
