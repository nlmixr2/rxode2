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

  # depot with a modeled alag() AND rate(), plus second-order sensitivities, so
  # the shape carries dLagEs/dRateEs/d2LagEs/d2RateEs and a nonzero nParam2 --
  # a first-order alag/F model alone would not notice a dropped rate pointer.
  .mod2 <- "
    ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
    alag(depot) <- exp(tlag)
    rate(depot) <- exp(trate)
    d/dt(depot)   = -ka * depot
    d/dt(central) =  ka * depot - cl / v * central
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

  # Slots whose LABEL does not describe the function they hold.  The labels are
  # frozen -- a released downstream package may have snapshotted this name vector
  # at build time -- so they are exempted here rather than corrected.  See the
  # comment on the `retN` block in src/init.c.
  .rxFrozenMislabeled <- c(8L, 9L)

  test_that("every table slot lands where iniRxodePtrs0() reads it", {
    # The table is positional: src/init.c fills slot N and iniRxodePtrs0() in
    # rxode2ptr.h reads slot N into a specific function pointer.  If the two ever
    # disagree a downstream package silently gets the wrong function, which no
    # other test would catch.
    h <- system.file("include", "rxode2ptr.h", package = "rxode2")
    skip_if(!nzchar(h) || !file.exists(h))
    lines <- grep("R_ExternalPtrAddrFn\\(VECTOR_ELT\\(p, [0-9]+\\)\\)",
                  readLines(h), value = TRUE)
    idx <- as.integer(sub(".*VECTOR_ELT\\(p, ([0-9]+)\\).*", "\\1", lines))
    var <- trimws(sub("^\\s*([A-Za-z0-9_]+)\\s*=.*", "\\1", lines))
    p <- .rxode2ptrs()
    # every slot is read exactly once, and none is out of range
    expect_equal(sort(idx), seq_along(p) - 1L)
    # names carry an inconsistent rxode2/rx prefix, so compare on the normalized stem
    .stem <- function(x) {
      sub("^rx", "", tolower(gsub("[^A-Za-z0-9]", "", gsub("rxode2", "", x))))
    }
    .keep <- !(idx %in% .rxFrozenMislabeled)
    expect_equal(.stem(var[.keep]), .stem(names(p)[idx[.keep] + 1L]))
  })

  test_that("the function-pointer table's existing names never change", {
    # A released downstream package may compare the names it snapshotted at build
    # time against the live table and refuse to load on any difference, and it
    # cannot be patched retroactively.  So existing labels are frozen even when
    # wrong (see .rxFrozenMislabeled): append new slots, never rename or reorder.
    expect_equal(names(.rxode2ptrs())[seq_len(10L)],
                 c("rxode2rxRmvnSEXP", "rxode2rxParProgress", "rxode2getRxSolve_",
                   "rxode2indSolve", "rxode2getTime", "rxode2isRstudio",
                   "rxode2iniSubjectE", "rxode2sortIds",
                   # slots 8 and 9: frozen, deliberately mislabeled
                   "getSolvingOptionsInd", "rxode2getUpdateInis"))
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

  test_that("toggling active preserves the rest of the shape", {
    # rxode2EventSensSetActive() is the cheap gate: it must flip `active` and
    # leave the dims (and the function pointers) in place, unlike Deactivate().
    on.exit(rxEventSensDeactivate(), add = TRUE)
    m <- rxode2(.mod1, calcSens = c("eta_ka", "eta_lag"), eventSens = "jump")
    rxEventSensLoadModel(m)
    before <- .rxGetEventSensDims()
    saved <- .rxEventSensShapeSave()
    .Call(`_rxode2_eventSensSetDims`, 0L, before[["nState"]], before[["nParam"]],
          before[["nParam2"]], before[["nParam3"]], before[["useCalcJac"]])
    off <- .rxGetEventSensDims()
    expect_equal(unname(off[["active"]]), 0L)
    expect_equal(off[-1L], before[-1L])   # only `active` moved
    # and the pointers were untouched: restoring the saved shape is a no-op here
    .rxEventSensShapeRestore(saved)
    expect_equal(.rxGetEventSensDims(), before)
  })

  test_that("rxEventSensLoadModel carries nParam2 for a second-order model", {
    on.exit(rxEventSensDeactivate(), add = TRUE)
    m <- rxode2(.mod2, calcSens = c("trate", "tlag"),
                calcSens2 = c("trate", "tlag"), eventSens = "jump")
    expect_true(rxEventSensLoadModel(m))
    d <- .rxGetEventSensDims()
    expect_equal(unname(d[["nParam"]]), 2L)
    expect_gt(d[["nParam2"]], 0L)   # would be 0 if the dim were dropped
  })

  test_that("eventSensLoadFull carries nParam3 and useCalcJac", {
    # rxode2EventSensLoad() (the older entry point) takes only four dims; the
    # regression this guards is LoadFull silently dropping the two added ones.
    on.exit(rxEventSensDeactivate(), add = TRUE)
    m <- rxode2(.mod1, calcSens = c("eta_ka", "eta_lag"), eventSens = "jump")
    .Call(`_rxode2_eventSensLoadFull`, rxModelVars(m)$trans,
          1L, 2L, 3L, 4L, 5L, 1L)
    expect_equal(.rxGetEventSensDims(),
                 c(active = 1L, nState = 2L, nParam = 3L, nParam2 = 4L,
                   nParam3 = 5L, useCalcJac = 1L))
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

    # a wrong-sized buffer, and a same-sized one that rxode2 did not stamp, are
    # both rejected rather than installed as live function pointers
    expect_error(.rxEventSensShapeRestore(as.raw(1:4)))
    expect_error(.rxEventSensShapeRestore(raw(length(saved))))
    corrupt <- saved
    corrupt[1:4] <- as.raw(c(0, 0, 0, 0))
    expect_error(.rxEventSensShapeRestore(corrupt))
    # the rejected restores left the installed shape alone
    expect_equal(.rxGetEventSensDims(),
                 c(active = 1L, nState = 2L, nParam = 2L, nParam2 = 0L,
                   nParam3 = 0L, useCalcJac = 0L))
  })

  test_that("save/restore preserves modeled rate() and second-order pointers", {
    # The .mod1 round trip below only exercises alag()/f().  This one carries
    # rate() and calcSens2 as well, so dropping dRateEs/d2RateEs/d2LagEs from the
    # saved shape would change the sensitivities instead of passing silently.
    on.exit(rxEventSensDeactivate(), add = TRUE)
    ev <- et(amt = 100, cmt = "depot") |> et(c(1, 4, 8, 12, 24))
    p <- c(tka = 0.45, tcl = 1, tv = 3.45, tlag = -0.7, trate = 3)
    m <- rxode2(.mod2, calcSens = c("trate", "tlag"),
                calcSens2 = c("trate", "tlag"), eventSens = "jump")
    ref <- as.data.frame(rxSolve(m, ev, params = p, atol = 1e-10, rtol = 1e-10))
    expect_true(any(grepl("rx__sens_", names(ref))))

    rxEventSensLoadModel(m)
    saved <- .rxEventSensShapeSave()
    rxEventSensDeactivate()
    .rxEventSensShapeRestore(saved)

    got <- as.data.frame(rxSolve(m, ev, params = p, atol = 1e-10, rtol = 1e-10))
    expect_equal(got, ref)
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
