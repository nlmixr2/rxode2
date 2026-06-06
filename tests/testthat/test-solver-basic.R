rxTest({
  .mod <- rxode2::rxode2({
    d/dt(y) <- -y
  })
  .et <- rxode2::eventTable()
  .et$add.sampling(seq(0, 5, length.out = 10))

  # method, hmin (NULL = omit), tolerance
  .solvers <- list(
    list(method = "ab",           hmin = 0.0001, tol = 0.05),
    list(method = "abm",          hmin = 0.0001, tol = 0.05),
    list(method = "bs",           hmin = NULL,   tol = 0.05),
    list(method = "ck54",         hmin = NULL,   tol = 0.05),
    list(method = "dop5",         hmin = NULL,   tol = 0.05),
    list(method = "dop54",        hmin = NULL,   tol = 0.05),
    list(method = "dop87",        hmin = NULL,   tol = 0.05),
    list(method = "gauss6",       hmin = 0.01,   tol = 0.05),
    list(method = "geng5",        hmin = 0.01,   tol = 0.05),
    list(method = "iem",          hmin = NULL,   tol = 0.05),
    list(method = "iiic6",        hmin = 0.01,   tol = 0.05),
    list(method = "radauiia5",    hmin = 0.01,   tol = 0.05),
    list(method = "rk43",         hmin = NULL,   tol = 0.05),
    list(method = "f32",          hmin = NULL,   tol = 0.05),
    list(method = "f78",          hmin = NULL,   tol = 0.05),
    list(method = "ros4",         hmin = NULL,   tol = 0.05),
    list(method = "ros43",        hmin = NULL,   tol = 0.05),
    list(method = "ros6",         hmin = 0.01,   tol = 0.05),
    list(method = "sdirk43",      hmin = NULL,   tol = 0.05),
    list(method = "vern65",       hmin = NULL,   tol = 0.05),
    list(method = "vern76",       hmin = NULL,   tol = 0.05),
    list(method = "vern98",       hmin = NULL,   tol = 0.05),
    list(method = "backwardEuler", hmin = 0.01,  tol = 0.2)
  )

  for (.cfg in .solvers) {
    local({
      .method <- .cfg$method
      .tol    <- .cfg$tol
      .hmin   <- .cfg$hmin
      test_that(paste(.method, "integrates 1-compartment model correctly"), {
        .args <- list(.mod, params = c(), events = .et,
                      inits = c(y = 1), method = .method)
        if (!is.null(.hmin)) .args$hmin <- .hmin
        out <- do.call(rxode2::rxSolve, .args)
        expect_s3_class(out, "rxSolve")
        expect_equal(out$y, exp(-out$time), tolerance = .tol)
      })
    })
  }
})
