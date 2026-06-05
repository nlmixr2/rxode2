rxTest({
  test_that("rxPrune captures evid_ inside if clause", {
    m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 24) {
        evid_(t + 12, 1, 50, 1, 0, 0, 0, 0)
      }
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")

    expect_false(grepl("evid_", pruned))
    expect_true(grepl("rxCaptureId1", pruned))
    expect_length(caps, 1L)
    expect_equal(caps[[1]]$id, 1L)
    expect_match(caps[[1]]$original, "evid_")
    expect_match(caps[[1]]$condition, "t")     # condition contains t
    expect_equal(caps[[1]]$capVar, "rxCaptureId1")
  })

  test_that("rxPrune captures bolus with correct args inside if clause", {
    m <- rxode2({
      d/dt(A) <- -ka * A
      if (t < 24) {
        bolus(50, 1, 0, 0, 0)
      }
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")

    expect_false(grepl("bolus", pruned))
    expect_true(grepl("rxCaptureId1", pruned))
    expect_length(caps, 1L)
    expect_match(caps[[1]]$original, "bolus")
  })

  test_that("rxPrune captures all adaptive dosing function types", {
    # (evid_, bolus, infuse, infuseDur, reset, replace, multiply, phantom, obs)
    # Each is wrapped in a simple model; correct arg counts per grammar.
    cases <- list(
      list(fn = "evid_",     call = "evid_(t+12, 1, 50, 1, 0, 0, 0, 0)"),
      list(fn = "bolus",     call = "bolus(50, 1, 0, 0, 0)"),
      list(fn = "infuse",    call = "infuse(50, 10, 1, 0, 0, 0)"),
      list(fn = "infuseDur", call = "infuseDur(50, 2, 1, 0, 0, 0)"),
      list(fn = "reset",     call = "reset()"),
      list(fn = "replace",   call = "replace(50, 1)"),
      list(fn = "multiply",  call = "multiply(0.5, 1)"),
      list(fn = "phantom",   call = "phantom(50, 1, 0, 0, 0)"),
      list(fn = "obs",       call = "obs(cp)")
    )
    for (case in cases) {
      model_str <- paste0(
        "d/dt(A) <- -ka * A\ncp <- A/vd\nif (t > 0) {\n  ",
        case$call, "\n}"
      )
      m <- tryCatch(
        suppressMessages(rxode2(model_str)),
        error = function(e) NULL
      )
      if (is.null(m)) next   # skip if parser rejects (e.g. obs)
      pruned <- rxPrune(m)
      caps <- attr(pruned, "capturedEvid")
      expect_false(
        grepl(case$fn, pruned, fixed = TRUE),
        info = paste0(case$fn, " should be removed from pruned string")
      )
      expect_true(
        !is.null(caps) && length(caps) >= 1L,
        info = paste0(case$fn, " should produce at least one capture")
      )
      expect_match(caps[[1]]$original, case$fn,
                   info = paste0(case$fn, " original should be stored"))
    }
  })

  test_that("rxPrune captures unconditional adaptive dosing call with condition '1'", {
    m <- rxode2({
      d/dt(A) <- -ka * A
      bolus(50, 1, 0, 0, 0)
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")

    expect_false(grepl("bolus", pruned))
    expect_length(caps, 1L)
    expect_equal(caps[[1]]$condition, "1")
    expect_true(grepl("rxCaptureId1 <- 1", pruned, fixed = TRUE))
  })

  test_that("rxPrune captures nested if conditions by multiplying them", {
    m <- rxode2({
      d/dt(A) <- -ka * A
      if (t > 0) {
        if (t < 24) {
          bolus(50, 1, 0, 0, 0)
        }
      }
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")

    expect_length(caps, 1L)
    # Both conditions must appear in the combined condition string
    expect_match(caps[[1]]$condition, "t")
    # Conditions are multiplied together (two conditions -> two sets of parens with *)
    expect_match(caps[[1]]$condition, "\\*")
  })

  test_that("rxPrune assigns unique sequential IDs to multiple adaptive dosing calls", {
    m <- rxode2({
      d/dt(A) <- -ka * A
      if (t < 12) {
        bolus(50, 1, 0, 0, 0)
      }
      if (t >= 12) {
        replace(10, 1)
      }
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")

    expect_length(caps, 2L)
    expect_equal(caps[[1]]$id, 1L)
    expect_equal(caps[[2]]$id, 2L)
    expect_true(grepl("rxCaptureId1", pruned))
    expect_true(grepl("rxCaptureId2", pruned))
    expect_match(caps[[1]]$original, "bolus")
    expect_match(caps[[2]]$original, "replace")
  })

  test_that("rxPrune attaches no capturedEvid attribute when no adaptive dosing present", {
    m <- rxode2({
      if (t < 12) cp <- A / vd
      d/dt(A) <- -ka * A
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")
    expect_true(is.null(caps) || length(caps) == 0L)
  })

  test_that(".restoreAdaptiveDosing builds correct if-wrapped restore lines", {
    caps <- list(
      list(id = 1L, original = "bolus(50, 1, 0, 0, 0)", condition = "(t<24)",
           capVar = "rxCaptureId1"),
      list(id = 2L, original = "reset()", condition = "1",
           capVar = "rxCaptureId2")
    )
    lines <- rxode2:::.restoreAdaptiveDosing(caps)
    expect_equal(lines[[1]], "if (rxCaptureId1) { bolus(50, 1, 0, 0, 0) }")
    expect_equal(lines[[2]], "if (rxCaptureId2) { reset() }")
  })

  test_that(".rxLoadPrune stores captures and restore lines in rxS env", {
    rxReq("symengine")
    m <- rxode2({
      d/dt(A) <- -ka * A
      if (t < 24) {
        bolus(50, 1, 0, 0, 0)
      }
    })
    env <- suppressMessages(rxode2:::.rxLoadPrune(m, doConst = TRUE))
    expect_true(!is.null(env$..capturedEvid))
    expect_length(env$..capturedEvid, 1L)
    expect_true(!is.null(env$..restoreLines))
    expect_match(env$..restoreLines[[1]], "bolus")
    expect_match(env$..restoreLines[[1]], "rxCaptureId1")
  })

  test_that("rxPrune captures adaptive dosing in else branch (negated condition)", {
    m <- rxode2({
      if (t < 12) {
        x <- 1
      } else {
        bolus(50, 1, 0, 0, 0)
      }
      d/dt(A) <- -ka * A
    })
    pruned <- rxPrune(m)
    caps <- attr(pruned, "capturedEvid")

    expect_length(caps, 1L)
    # The else branch condition is negated: 1-(t<12) form
    expect_match(caps[[1]]$condition, "1-")
  })
})
