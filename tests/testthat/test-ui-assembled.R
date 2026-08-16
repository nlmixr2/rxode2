## ui-assembly hooks: functions called with a freshly assembled ui BEFORE it is
## compressed.  This is the only point at which a package can attach parse-time
## state to a model -- `rxUdfUi()` has no field to return it, and after
## `rxUiCompress()` the ui is a list whose `rxUiDecompress()` yields a fresh
## environment on every call, so a later in-place assignment is invisible.

.assembledMod <- function() {
  ini({ tcl <- -2 })
  model({
    cl <- exp(tcl)
    d/dt(depot) <- -cl * depot
    cp <- depot
  })
}

test_that("rxRegisterUiAssembled validates its arguments", {
  expect_error(rxRegisterUiAssembled(1, function(ui) NULL), "single string")
  expect_error(rxRegisterUiAssembled(c("a", "b"), function(ui) NULL), "single string")
  expect_error(rxRegisterUiAssembled("nm", "notAFunction"), "must be a function")
})

test_that("an assembled hook sees the ui as a mutable environment, and the change sticks", {
  on.exit(rxRemoveUiAssembled("test:assembled"), add = TRUE)
  seen <- new.env(parent = emptyenv())
  rxRegisterUiAssembled("test:assembled", function(ui) {
    seen$isEnv <- is.environment(ui)
    assign("testAssembledSlot", 42L, envir = ui)
    assign("sticky", unique(c(ui$sticky, "testAssembledSlot")), envir = ui)
  })

  ui <- rxode2(.assembledMod)
  ## the hook ran, and it ran while the ui was still an environment
  expect_true(isTRUE(seen$isEnv))
  ## the assignment survived compression and is visible to the caller
  uiEnv <- rxUiDecompress(ui)
  expect_true(exists("testAssembledSlot", envir = uiEnv, inherits = FALSE))
  expect_equal(get("testAssembledSlot", envir = uiEnv, inherits = FALSE), 42L)
})

test_that("an assembled-hook slot marked sticky survives saveRDS and piping", {
  on.exit(rxRemoveUiAssembled("test:assembled"), add = TRUE)
  rxRegisterUiAssembled("test:assembled", function(ui) {
    assign("testAssembledSlot", list(a = 1), envir = ui)
    assign("sticky", unique(c(ui$sticky, "testAssembledSlot")), envir = ui)
  })
  ui <- rxode2(.assembledMod)

  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(ui, f)
  ui2 <- rxUiDecompress(readRDS(f))
  expect_equal(get("testAssembledSlot", envir = ui2, inherits = FALSE), list(a = 1))

  ui3 <- rxUiDecompress(ui |> model(cp <- depot * 1))
  expect_equal(get("testAssembledSlot", envir = ui3, inherits = FALSE), list(a = 1))
})

test_that("a failing assembled hook warns but does not break the model build", {
  on.exit(rxRemoveUiAssembled("test:bad"), add = TRUE)
  rxRegisterUiAssembled("test:bad", function(ui) stop("deliberate"))
  expect_warning(ui <- rxode2(.assembledMod), "test:bad")
  expect_true(inherits(ui, "rxUi"))
})

test_that("rxRemoveUiAssembled unregisters, and removing an absent hook is a no-op", {
  rxRegisterUiAssembled("test:gone", function(ui) stop("should not run"))
  expect_null(rxRemoveUiAssembled("test:gone"))
  ## the removed hook must not fire: it would surface as a hook-failure warning
  ## (rxode2() itself emits routine parse messages, so only warnings are checked)
  expect_no_warning(ui <- suppressMessages(rxode2(.assembledMod)))
  expect_true(inherits(ui, "rxUi"))
  expect_null(rxRemoveUiAssembled("test:neverRegistered"))
})

test_that("a ui-prep hook may take (ui) or (ui, solveModel)", {
  on.exit({
    rxRemoveUiPrep("test:prep1")
    rxRemoveUiPrep("test:prep2")
  }, add = TRUE)
  seen <- new.env(parent = emptyenv())
  ## one-argument hook: the long-standing signature, still supported
  rxRegisterUiPrep("test:prep1", function(ui) seen$one <- TRUE)
  ## two-argument hook: also receives the model actually being solved
  rxRegisterUiPrep("test:prep2", function(ui, solveModel) {
    seen$two <- TRUE
    seen$pars <- tryCatch(rxModelVars(solveModel)$params, error = function(e) NULL)
  })

  ui <- rxode2(.assembledMod)
  ev <- et(amt = 100)
  ev <- et(ev, seq(1, 8, by = 1))
  invisible(rxSolve(ui, ev))

  expect_true(isTRUE(seen$one))
  expect_true(isTRUE(seen$two))
  ## the solve model's parameter vector is what the gpars layout uses
  expect_true(is.character(seen$pars))
  expect_true("tcl" %in% seen$pars)
})
