rxTest({
  # C API for the event ("jump") sensitivity shape (issue #1169) and the CMT
  # covariate writer (issue #1172).  These entry points exist so a downstream
  # package (nlmixr2est's FOCEi) can swap between peer models inside one shared
  # solve pool from C++.  Here they are driven through their .Call wrappers,
  # which call exactly the same C functions the pointer table exports.

  .mod1 <- "
    ka <- exp(tka + eta_ka)
    cl <- exp(tcl)
    v  <- exp(tv)
    alag(depot) <- exp(tlag + eta_lag)
    f(depot)    <- expit(tf)
    d/dt(depot)   <- -ka * depot
    d/dt(central) <-  ka * depot - cl / v * central
    cp <- central / v
  "

  test_that("the rxode2 function-pointer table is complete and named", {
    p <- .rxode2ptrs()
    expect_false(any(vapply(p, is.null, logical(1))))
    expect_true(all(nzchar(names(p))))
    expect_false(anyDuplicated(names(p)) > 0L)
    # the entry points added for issues #1169 / #1172
    expect_true(all(c("rxode2setIndCmt", "rxode2EventSensShapeSize",
                      "rxode2EventSensShapeSave", "rxode2EventSensShapeRestore",
                      "rxode2EventSensLoadFull", "rxode2EventSensGetDims",
                      "rxode2EventSensSetDims", "rxode2EventSensSetActive",
                      "rxode2EventSensDeactivate") %in% names(p)))
  })

  test_that("event-sensitivity dims round trip and deactivate to zero", {
    on.exit(rxEventSensDeactivate(), add = TRUE)
    .Call(`_rxode2_eventSensSetDims`, 1L, 3L, 4L, 5L, 6L, 1L)
    expect_equal(.rxGetEventSensDims(),
                 c(active = 1L, nState = 3L, nParam = 4L, nParam2 = 5L,
                   nParam3 = 6L, useCalcJac = 1L))
    rxEventSensDeactivate()
    expect_equal(.rxGetEventSensDims(),
                 c(active = 0L, nState = 0L, nParam = 0L, nParam2 = 0L,
                   nParam3 = 0L, useCalcJac = 0L))
  })

  test_that("rxEventSensLoadModel installs all six dims", {
    on.exit(rxEventSensDeactivate(), add = TRUE)
    m <- rxode2(.mod1, calcSens = c("eta_ka", "eta_lag"), eventSens = "jump")
    expect_true(rxEventSensLoadModel(m))
    d <- .rxGetEventSensDims()
    expect_equal(unname(d[["active"]]), 1L)
    expect_equal(unname(d[["nState"]]), 2L)   # depot + central
    expect_equal(unname(d[["nParam"]]), 2L)   # eta_ka + eta_lag
    # an fd model must not activate the jumps
    mfd <- rxode2(.mod1, calcSens = c("eta_ka", "eta_lag"), eventSens = "fd")
    expect_false(rxEventSensLoadModel(mfd))
  })

  test_that("shape save/restore round trips the whole shape, not just the dims", {
    on.exit(rxEventSensDeactivate(), add = TRUE)
    m <- rxode2(.mod1, calcSens = c("eta_ka", "eta_lag"), eventSens = "jump")
    rxEventSensLoadModel(m)
    saved <- .rxEventSensShapeSave()
    expect_true(is.raw(saved))
    expect_gt(length(saved), 0L)

    # clobber the shape the way a peer model's install would
    rxEventSensDeactivate()
    .Call(`_rxode2_eventSensSetDims`, 1L, 99L, 99L, 99L, 99L, 1L)
    expect_equal(unname(.rxGetEventSensDims()[["nState"]]), 99L)

    .rxEventSensShapeRestore(saved)
    expect_equal(.rxGetEventSensDims(),
                 c(active = 1L, nState = 2L, nParam = 2L, nParam2 = 0L,
                   nParam3 = 0L, useCalcJac = 0L))

    expect_error(.rxEventSensShapeRestore(as.raw(1:4)))
  })

  test_that("a solve is unchanged by a save / clobber / restore bracket", {
    # The strong check: the function pointers, not only the dims, come back --
    # a restored shape must reproduce the reference sensitivities exactly.
    on.exit(rxEventSensDeactivate(), add = TRUE)
    ev <- et(amt = 100, cmt = "depot") |> et(c(1, 4, 8, 12, 24))
    p <- c(tka = 0.45, tcl = 1, tv = 3.45, tlag = -0.7, tf = 1,
           eta_ka = 0, eta_lag = 0)
    m <- rxode2(.mod1, calcSens = c("eta_ka", "eta_lag"), eventSens = "jump")
    ref <- as.data.frame(rxSolve(m, ev, params = p, atol = 1e-10, rtol = 1e-10))

    rxEventSensLoadModel(m)
    saved <- .rxEventSensShapeSave()
    rxEventSensDeactivate()
    .rxEventSensShapeRestore(saved)

    got <- as.data.frame(rxSolve(m, ev, params = p, atol = 1e-10, rtol = 1e-10))
    expect_equal(got, ref)
  })
})
