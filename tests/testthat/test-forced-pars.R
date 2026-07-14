## Per-ui forced parameters: a named-numeric slot carried on the ui whose values
## override params/data/inits on EVERY solve (injected into every gpars column at
## solve setup, the par-loader injection point).  This is the plugin-free forcing
## mechanism that lets a model/fit carry externally-owned values (e.g. trained
## neural-network weights) and stay self-contained.

## slow-decay model: cl = exp(tcl)*WT with tcl small, so cp stays well away from 0
## across the observed window (a robust, non-saturating response to forcing WT).
.forcedMod <- function() {
  ini({ tcl <- -2 })
  model({
    cl <- exp(tcl) * WT
    d/dt(depot) <- -cl * depot
    cp <- depot
  })
}

.forcedEv <- function() {
  ev <- et(amt = 100)
  ev <- et(ev, seq(1, 8, by = 1))
  ev$WT <- 1
  ev
}

test_that("rxForcedPars getter/setter round-trips and validates", {
  ui <- rxode2(.forcedMod)
  expect_null(rxForcedPars(ui))
  rxForcedPars(ui) <- c(WT = 2)
  expect_equal(rxForcedPars(ui), c(WT = 2))
  ## unnamed -> error
  expect_error(`rxForcedPars<-`(ui, 3), "fully-named")
  ## clear
  rxForcedPars(ui) <- NULL
  expect_null(rxForcedPars(ui))
})

test_that("forcedPars overrides the data covariate on rxSolve; unset -> unchanged", {
  ui <- rxode2(.forcedMod)
  ev <- .forcedEv()

  s0 <- rxSolve(ui, ev)

  ## force WT = 2 -> cl doubles -> faster decay -> strictly lower cp at every t>0
  rxForcedPars(ui) <- c(WT = 2)
  s1 <- rxSolve(ui, ev)
  expect_true(all(s1$cp < s0$cp))

  ## the injected value is captured on the solved object too
  expect_equal(unname(rxInjectedPars(s1)["WT"]), 2)

  ## unset -> identical to the un-forced solve
  rxForcedPars(ui) <- NULL
  s2 <- rxSolve(ui, ev)
  expect_equal(s2$cp, s0$cp, tolerance = 1e-8)
})

test_that("forcedPars is hidden from the printed model and survives piping", {
  ui <- rxode2(.forcedMod)
  rxForcedPars(ui) <- c(WT = 2)

  ## hidden: stored on the ui env, not the printed meta block
  .txt <- paste(capture.output(print(ui)), collapse = "\n")
  expect_false(grepl("forcedPars", .txt))

  ## survives a model-block change (a "significant" pipe) via the sticky mechanism
  ui2 <- ui |> model(cp <- depot * 1)
  expect_equal(rxForcedPars(ui2), c(WT = 2))
  ## still hidden after piping
  .txt2 <- paste(capture.output(print(ui2)), collapse = "\n")
  expect_false(grepl("forcedPars", .txt2))

  ## and still applied on the piped model's solve
  ev <- .forcedEv()
  s0 <- rxSolve(rxode2(.forcedMod), ev)
  s2 <- rxSolve(ui2, ev)
  expect_true(all(s2$cp < s0$cp))
})

test_that("forcedPars names that are not model params are ignored", {
  ui <- rxode2(.forcedMod)
  ev <- .forcedEv()
  s0 <- rxSolve(ui, ev)
  ## a bogus name alongside a real one: bogus ignored, WT applied
  rxForcedPars(ui) <- c(nonParam = 99, WT = 2)
  s1 <- rxSolve(ui, ev)
  expect_true(all(s1$cp < s0$cp))
})
