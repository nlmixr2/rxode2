rxTest({
  # An adaptive explicit RK step that keeps failing the error test down to
  # dop853's minimum step (0.1*|h| <= |t|*eps) fails the subject's solve, as
  # dop853 does; before, the step was force-accepted and the solve went on.
  .sol <- function(m, meth, ...) {
    .w <- character()
    .s <- withCallingHandlers(
      rxSolve(
        m,
        data.frame(ID = 1:2, TIME = 2, EVID = 0, AMT = 0),
        method = meth,
        atol = 1e-8,
        rtol = 1e-8,
        maxsteps = 100000,
        addDosing = FALSE,
        ...
      ),
      warning = function(w) {
        .w <<- c(.w, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(x = .s$x, w = .w)
  }
  test_that("dop853s fails where dop853 reports step size too small", {
    .m <- rxode2("d/dt(x) <- 1/(abs(t-1)+1e-300)") # non-integrable singularity at t = 1
    .d <- .sol(.m, "dop853")
    .s <- .sol(.m, "dop853s")
    expect_true(all(is.na(.d$x)))
    expect_true(all(is.na(.s$x)))
    expect_true(any(grepl("could not solve the ODEs", .s$w)))
  })
  test_that("dop853s still solves what dop853 solves", {
    .m <- rxode2("d/dt(x) <- 0.5/sqrt(abs(t-1)+1e-300)") # integrable singularity, x(2) = 2
    expect_equal(.sol(.m, "dop853s")$x, rep(2, 2), tolerance = 1e-6)
    expect_equal(.sol(.m, "dop853")$x, rep(2, 2), tolerance = 1e-6)
  })
})
