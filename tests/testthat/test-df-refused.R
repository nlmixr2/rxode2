rxTest({
  # rxDfSolveRefused() (slot 113): whether rxSolve() refuses a solve rather
  # than returning it, read through the pointer table as a downstream package
  # solving through rxode2 reads it.
  .refMod <- rxode2({
    ka <- 1.2
    cl <- 3
    v <- 30
    d / dt(depot) <- -ka * depot
    d / dt(center) <- ka * depot - cl / v * center
    cp <- center / v
  })
  .refEv <- function(id = 1:2) {
    et(amt = 100, cmt = "depot", id = id) |> et(seq(0, 12, by = 3), id = id)
  }
  .refused <- function(ev, ...) {
    on.exit(rxSolveFree(), add = TRUE)
    rxSolve(.refMod, ev, ..., .setupOnly = 1L)
    .Call("_rxTestDfRefusedSlot", .rxode2ptrs(), PACKAGE = "rxode2")
  }

  test_that("a solve rxSolve() returns is not refused", {
    expect_identical(.refused(.refEv()), 0L)
    expect_identical(.refused(.refEv(1L)), 0L)
  })

  test_that("a model 'vv' cannot integrate is refused, as rxSolve() refuses it", {
    expect_error(rxSolve(.refMod, .refEv(), method = "vv"), "Velocity Verlet")
    expect_identical(.refused(.refEv(), method = "vv"), 2L)
  })

  test_that("the refusal slot is 113, the last", {
    .n <- names(.rxode2ptrs())
    expect_identical(length(.n), 114L)
    expect_identical(.n[114], "rxode2rxDfSolveRefused")
  })
})
