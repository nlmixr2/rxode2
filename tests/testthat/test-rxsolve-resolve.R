rxTest({
  # Re-solving a solved object is the inner loop of MAP estimation in posologyr
  # (levenc/posologyr#70); it must reuse the compiled model and translate the
  # events once.
  .m <- rxode2({
    cl <- tcl * exp(eta.cl) * (wt / 70)
    v <- tv * exp(eta.v)
    d/dt(centr) <- -cl / v * centr
    alag(centr) <- lagt
    cp <- centr / v
  })
  .p <- c(tcl = 4, tv = 30, eta.cl = 0, eta.v = 0, lagt = 0.1)
  .d <- data.frame(
    ID = 1,
    TIME = c(0, 8, 11.9, 12, 23.9),
    DV = c(NA, NA, 4, NA, 3),
    AMT = c(1000, 1000, NA, 1000, NA),
    DUR = c(1, 1, NA, 1, NA),
    EVID = c(1, 1, 0, 1, 0),
    wt = c(80, 80, 80, 82, 82)
  )
  .p2 <- .p
  .p2[c("eta.cl", "eta.v")] <- c(0.2, -0.1)
  .d2 <- .d
  .d2$wt <- 75

  test_that("rxSolve(<rxSolve>, params, events) does not re-parse the model", {
    .s <- rxSolve(.m, .p, .d)
    .expected <- as.data.frame(rxSolve(.m, .p2, .d2))
    local_mocked_bindings(.rxTransCharacter = function(...) stop("model re-parsed"))
    .r <- rxSolve(.s, .p2, .d2)
    expect_equal(as.data.frame(.r), .expected)
    .r <- rxSolve(.s, c(.p2, wt = 70), et(amt = 1000, dur = 1, ii = 8, addl = 2) |> et(1:24))
    expect_s3_class(.r, "rxSolve")
  })

  test_that("re-solving keeps working after the model DLL is unloaded", {
    .m2 <- rxode2("d/dt(centr) = -kel * centr")
    .e1 <- et(amt = 100) |> et(1:4)
    .e2 <- et(amt = 200) |> et(1:4)
    .s <- rxSolve(.m2, c(kel = 0.2), .e1)
    rxDynUnload(.m2)
    expect_equal(
      as.data.frame(rxSolve(.s, c(kel = 0.3), .e2)),
      as.data.frame(rxSolve(.m2, c(kel = 0.3), .e2))
    )
  })

  test_that("re-solving keeps the event (jump) sensitivities", {
    .m3 <- rxode2({
      alag(centr) <- tau
      f(centr) <- bio
      d/dt(centr) <- -kel * centr
    }, calcSens = c("tau", "bio", "kel"))
    .p3 <- c(tau = 0.5, bio = 0.8, kel = 0.2)
    .s <- rxSolve(.m3, .p3, et(amt = 100) |> et(0:10))
    .e2 <- et(amt = 200) |> et(0:10)
    expect_equal(as.data.frame(rxSolve(.s, .p3, .e2)), as.data.frame(rxSolve(.m3, .p3, .e2)))
  })

  test_that("a data frame solve and a $params update translate the events once", {
    .n <- 0L
    .etTrans <- etTrans
    local_mocked_bindings(etTrans = function(...) {
      .n <<- .n + 1L
      .etTrans(...)
    })
    .s <- rxSolve(.m, .p, .d)
    expect_equal(.n, 1L)
    .n <- 0L
    .s$params <- .p2
    expect_equal(.n, 1L)
    expect_equal(as.data.frame(.s), as.data.frame(rxSolve(.m, .p2, .d)))
  })

  test_that("the small-table sort matches data.table's order", {
    .c1 <- c(2L, NA, 1L, 1L, NA, 2L, 1L, 2L, 1L, 1L, 1L)
    .c2 <- c(1, 2, NaN, 0, 1, NA, -0, 1, NA, NaN, 0)
    .c3 <- c(3L, 1L, 2L, 1L, NA, 2L, 1L, 3L, 1L, 1L, 2L)
    expect_identical(
      .forder3(.c1, .c2, .c3),
      data.table::data.table(c1 = .c1, c2 = .c2, c3 = .c3)[order(c1, c2, c3), which = TRUE]
    )
    .c2b <- c(1, 2, 3, 0, 1, 2, -0, 1, 0, 3, 0)
    expect_true(.forderUseBase(.c1, .c2b, .c3))
    expect_identical(
      .forder3(.c1, .c2b, .c3),
      data.table::data.table(c1 = .c1, c2 = .c2b, c3 = .c3)[order(c1, c2, c3), which = TRUE]
    )
    for (.dec in c(FALSE, TRUE)) {
      expect_identical(
        .forder1(.c1, decreasing = .dec),
        data.table::data.table(c1 = .c1)[order(c1, decreasing = .dec, na.last = TRUE), which = TRUE]
      )
      expect_identical(
        .forder1(.c2, decreasing = .dec),
        data.table::data.table(c1 = .c2)[order(c1, decreasing = .dec, na.last = TRUE), which = TRUE]
      )
    }
  })
})
