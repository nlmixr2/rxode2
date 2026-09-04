rxTest({

  # Infusions pushed from inside the model with evid_().  _rxTranslateOneEvent()
  # has to emit the record that turns the infusion back off: a fixed rate/
  # duration dose needs a -rate record at time + dur, and a modeled rate/duration
  # dose needs its companion "off" record at the same time for updateRate()/
  # updateDur() to fill in.  Each pushed regimen is compared against the same
  # regimen written into the event table.

  .obs <- c(0, 1e-8, seq(0.5, 24, by = 0.5))
  .ref <- rxode2({
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })
  .pars <- c(cl = 1, v = 10)

  .expectSameAsEventTable <- function(mod, ev, pars = .pars, tolerance = 1e-5) {
    got <- rxSolve(mod, pars, et(.obs))
    want <- rxSolve(.ref, .pars, ev |> et(.obs))
    expect_equal(got$time, want$time)
    expect_equal(got$cp, want$cp, tolerance = tolerance)
  }

  ## A steady-state dose into a compartment with a modeled alag() (rxode2#1349)
  ## has to be expanded into an UNLAGGED steady-state record plus the lagged
  ## records that actually dose the subject.  Only the event table did that, so
  ## the same regimen pushed from inside the model solved to something else.
  .obsLag <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
  .refLag <- rxode2({
    d/dt(central) <- -cl / v * central
    alag(central) <- 3
    cp <- central / v
  })
  .expectSameAsEventTableLag <- function(mod, ev, ref = .refLag,
                                         tolerance = 1e-5) {
    got <- rxSolve(mod, .pars, et(.obsLag))
    want <- rxSolve(ref, .pars, ev |> et(.obsLag))
    expect_equal(got$time, want$time)
    ## the t=0 row differs by construction: a dose pushed at the current time
    ## is applied after that record's own observation
    expect_equal(got$cp[-1], want$cp[-1], tolerance = tolerance)
  }

  test_that("a pushed steady-state dose into an alag() compartment matches the event table (#1349)", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 1)
      }
    })
    .expectSameAsEventTableLag(
      mod, et(amt = 100, time = 2, rate = 10, ii = 12, ss = 1))
  })

  test_that("a pushed ss=2 dose into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 2)
      }
    })
    .expectSameAsEventTableLag(
      mod, et(amt = 100, time = 2, rate = 10, ii = 12, ss = 2))
  })

  test_that("a pushed steady-state bolus into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 12, 0, 1)
      }
    })
    .expectSameAsEventTableLag(mod, et(amt = 100, time = 2, ii = 12, ss = 1))
  })

  test_that("a pushed steady-state modeled-rate dose into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 12, 0, 1)
      }
    })
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      rate(central) <- 10
      cp <- central / v
    })
    .expectSameAsEventTableLag(
      mod, et(amt = 100, time = 2, rate = -1, ii = 12, ss = 1), ref = ref)
  })

  test_that("a pushed steady-state modeled-duration dose into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -2, 12, 0, 1)
      }
    })
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      dur(central) <- 10
      cp <- central / v
    })
    .expectSameAsEventTableLag(
      mod, et(amt = 100, time = 2, rate = -2, ii = 12, ss = 1), ref = ref)
  })

  test_that("only the first occurrence of a pushed ss+addl series into an alag() compartment expands", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 2, 1)
      }
    })
    .expectSameAsEventTableLag(
      mod, et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2, ss = 1))
  })

  test_that("a pushed evid=4 steady-state dose into an alag() compartment matches the event table", {
    # the widest expansion: a reset plus the four steady-state-with-lag records
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 10, 12, 0, 1)
      }
    })
    .expectSameAsEventTableLag(
      mod, et(amt = 100, time = 2, rate = 10, evid = 4, ii = 12, ss = 1))
  })

  test_that("ssAtDoseTime=FALSE turns the pushed lagged expansion off too", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 1)
      }
    })
    got <- rxSolve(mod, .pars, et(.obsLag), ssAtDoseTime = FALSE)
    want <- rxSolve(.refLag, .pars,
                    et(amt = 100, time = 2, rate = 10, ii = 12, ss = 1) |>
                      et(.obsLag), ssAtDoseTime = FALSE)
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed phantom dose keeps a modeled rate, as the event table does", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      rate(depot) <- 25
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 7, 100, 1, -1, 0, 0, 0)
      }
    })
    ref <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      rate(depot) <- 25
      cp <- central / v
    })
    pars <- c(ka = 0.5, cl = 1, v = 10)
    got <- rxSolve(mod, pars, et(.obs))
    want <- rxSolve(ref, pars,
                    et(amt = 100, time = 2, rate = -1, evid = 7) |> et(.obs))
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed lagged steady-state bolus splits into every target", {
    # the lagged steady-state expansion yields TWO splittable bolus records
    # (the flg 9 steady-state record and its plain flg 1 companion), so
    # _rxPushDose() has to split each of them, not just the first
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 12, 0, 1)
      }
    })
    # the same model with the dose in the event table instead: splitBolus is a
    # post-pass over already-translated records on both sides, so this is the
    # comparison that has to hold
    mBase <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      alag(central) <- 3
      cp <- central / v
    })
    p <- c(ka = 0.5, cl = 1, v = 10)
    got <- rxSolve(mSplit, p, et(obs))
    want <- rxSolve(mBase, p,
                    et(amt = 100, time = 2, ii = 12, ss = 1, cmt = 1) |> et(obs))
    expect_true(all(is.finite(got$cp)))
    expect_equal(got$depot[-1], want$depot[-1], tolerance = 1e-5)
    expect_equal(got$central[-1], want$central[-1], tolerance = 1e-5)
  })

  ## A negative compartment (turn that compartment off) is understood by the
  ## shared translator, so the two engines agree on it, but evid_() cannot
  ## SPELL it yet: cmt_evid in inst/tran.g accepts only a positive compartment,
  ## and it is shared with bolus()/infuse(), where a negative compartment would
  ## be meaningless.  Reaching it from the model language needs a grammar
  ## change; the translator-level agreement is covered by
  ## test-etTrans-translator.R's negcmt cells.

  test_that("a pushed evid=4 infusion turns off (#1322 follow-up)", {
    # evid=4 is reset + dose, and the translated event only had room for those
    # two records, so the infusion stop was dropped and the infusion ran for the
    # rest of the solve.
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 10, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, evid = 4))
  })

  test_that("a pushed evid=4 bolus is unchanged", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 0, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, evid = 4))
  })

  test_that("a pushed modeled rate (rate=-1) dose solves (#1322 follow-up)", {
    # the modeled on record was pushed without its off record, so
    # handleTurnOnModeledRate() reported data error 997 and the solve failed
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10))
  })

  test_that("a pushed modeled duration (rate=-2) dose solves (#1322 follow-up)", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -2, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, dur = 10))
  })

  test_that("a pushed evid=4 modeled rate dose resets and solves", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, -1, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, evid = 4))
  })

  test_that("pushed modeled rate/duration doses repeat with addl", {
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    modRate <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 12, 2, 0)
      }
    })
    modDur <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -2, 12, 2, 0)
      }
    })
    want <- rxSolve(.ref, .pars,
                    et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2) |> et(obs))
    expect_equal(rxSolve(modRate, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
    expect_equal(rxSolve(modDur, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("a pushed modeled rate dose reaches steady state", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 12, 0, 1)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, ii = 12, ss = 1))
  })

  test_that("a pushed fixed-duration (infuseDur) dose matches the event table", {
    # infuseDur() sets isDur, so rate carries the DURATION and rateI is 2
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuseDur(100, 10, 1, 0, 0, 0)
      }
    })
    got <- rxSolve(mod, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 100, time = 0, dur = 10) |> et(.obs))
    expect_equal(got$cp, want$cp, tolerance = 1e-5)
  })

  test_that("a pushed modeled rate constant infusion (ss=1, ii=0, amt=0) solves", {
    # flg 40 never turns off -- getTime__() skips the infusion-time calculation
    # for it and etTran.cpp emits no off record either, so the pushed path must
    # not add one.  The extra record would be numerically inert, so this checks
    # the trajectory; that the record itself is gone is checked by comparing
    # what the two translators emit, which the fix keeps in step.
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 1, 0, 1, -1, 0, 0, 1)
      }
    })
    got <- rxSolve(mod, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 0, time = 0, rate = 10, ss = 1, ii = 0) |> et(.obs))
    # the push happens during the first evaluation, so the steady state is in
    # place from the next output row onward rather than at time 0 itself
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed modeled infusion works above compartment 100 and with a modeled lag", {
    # cmt100 has to survive into the off record, and the off record's time is
    # written by updateRate() as laggedStart + dur
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 0, 0, 0)
      }
    })
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
    })
    got <- rxSolve(mod, .pars, et(.obs))
    want <- rxSolve(ref, .pars, et(amt = 100, time = 2, rate = 10) |> et(.obs))
    expect_equal(got$cp, want$cp, tolerance = 1e-5)

    # a compartment above 100: the pushed dose and the event-table dose must
    # reach the same state
    .states <- paste0("a", seq_len(120))
    .mod <- paste0("d/dt(", .states, ") <- -0.1*", .states, collapse = "\n")
    modHi <- rxode2(paste0(.mod, "\nrate(a110) <- 10\n",
                           "if (t < 1e-8) { evid_(2, 1, 100, 110, -1, 0, 0, 0) }"))
    refHi <- rxode2(.mod)
    gotHi <- rxSolve(modHi, et(seq(0, 24, by = 1)))
    wantHi <- rxSolve(refHi, et(amt = 100, time = 2, rate = 10, cmt = "a110") |>
                        et(seq(0, 24, by = 1)))
    expect_equal(gotHi$a110, wantHi$a110, tolerance = 1e-5)
  })

  test_that("a pushed evid=4 modeled duration dose resets and solves", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, -2, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, dur = 10, evid = 4))
  })

  test_that("a pushed steady state (ss=2) infusion matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 2)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, ii = 12, ss = 2))
  })

  test_that("pushed fixed-rate doses repeat with addl", {
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 2, 0)
      }
    })
    want <- rxSolve(.ref, .pars,
                    et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2) |> et(obs))
    expect_equal(rxSolve(mod, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("a pushed evid=4 dose repeated with addl resets only once (rxode2#1351/#1352)", {
    # _rxPushDose()'s addl loop used to pass the original evid (4, reset+dose)
    # unmodified to every repeat, so every addl repetition re-reset the
    # compartment -- unlike the data-table addl expansion in etTran.cpp, which
    # resets only on the first evid=4 occurrence (matches NONMEM, see
    # rxode2#1351). Both spellings must now agree.
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 0, 12, 2, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, evid = 4, ii = 12, addl = 2))
  })

  test_that("a pushed evid=4 infusion repeated with addl resets only once", {
    # the infusion form of the test above: the first occurrence translates to
    # three records (reset, on, off) and every repeat to two (on, off)
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 10, 12, 2, 0)
      }
    })
    want <- rxSolve(.ref, .pars,
                    et(amt = 100, time = 2, rate = 10, evid = 4, ii = 12, addl = 2) |>
                      et(obs))
    expect_equal(rxSolve(mod, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("addl does not repeat a pushed reset or 'other' record", {
    # _rxPushDose()'s addl loop used to repeat ANY evid, so a pushed evid=3
    # with addl reset the system once per repetition.  The event table warns
    # and ignores addl for evid 0/2/3 (etTran.cpp), and the shared
    # _rxAddlApplies() now gives the push path the same rule.
    obs <- seq(0, 48, by = 1)
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 1, 100, 1, 0, 0, 0, 0)
        evid_(15, 1, 100, 1, 0, 0, 0, 0)
        evid_(30, 1, 100, 1, 0, 0, 0, 0)
        evid_(10, 3, 0, 1, 0, 12, 2, 0)
      }
    })
    got <- rxSolve(mod, .pars, et(obs))
    oneReset <- rxSolve(.ref, .pars,
                        et(amt = 100, time = 0) |> et(amt = 100, time = 15) |>
                          et(amt = 100, time = 30) |>
                          et(time = 10, evid = 3) |> et(obs))
    threeResets <- rxSolve(.ref, .pars,
                           et(amt = 100, time = 0) |> et(amt = 100, time = 15) |>
                             et(amt = 100, time = 30) |>
                             et(time = 10, evid = 3) |> et(time = 22, evid = 3) |>
                             et(time = 34, evid = 3) |> et(obs))
    # the t=0 row differs by construction: a dose pushed at the current time
    # lands after that record's own observation
    .i <- got$time > 0
    expect_equal(got$cp[.i], oneReset$cp[.i], tolerance = 1e-5)
    expect_false(isTRUE(all.equal(got$cp[.i], threeResets$cp[.i],
                                  tolerance = 1e-5)))
  })

  test_that("a pushed dose carries the same ii as the identical data record", {
    # rep 0 of an addl series keeps ii only when it means something (steady
    # state); this is _rxAddlOccurrence()'s rule, shared with the event table.
    # et() zeroes a meaningless ii before etTrans() ever sees it, so the
    # comparable reference is a raw data.frame.
    obs <- c(0, 1e-8, seq(0.5, 24, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 12, 0, 0)
      }
    })
    raw <- rbind(data.frame(id = 1, time = obs, amt = NA_real_, evid = 0,
                            ii = 0, addl = 0, ss = 0),
                 data.frame(id = 1, time = 2, amt = 100, evid = 1,
                            ii = 12, addl = 0, ss = 0))
    raw <- raw[order(raw$time), ]
    got <- rxSolve(mod, .pars, et(obs), addDosing = TRUE)
    want <- rxSolve(.ref, .pars, raw, addDosing = TRUE)
    expect_equal(sort(names(got)), sort(names(want)))
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a split bolus pushed as evid=4 reserves enough room (#1322 follow-up)", {
    # splitBolus expands one translated event into splitBolusN-1 records, so an
    # evid=4 push writes 1 + (splitBolusN-1) records where the capacity check
    # only reserved ev.n = 2.  Three targets steps n_all_times by 3 and never
    # lands on the one offset that overruns, so this uses FOUR (nRec = 4 against
    # a reservation of 2), which does reach past the end of the block.  Push
    # repeatedly so the EVID_EXTRA_SIZE slack that hid the overrun is used up.
    # The overrun is a couple of elements that malloc bucketing usually absorbs,
    # so this exercises the boundary rather than failing deterministically
    # without the fix -- it is here to give a heap checker something to catch.
    mSplit <- rxode2({
      splitBolus(depot, depot, central, peri)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central - q * central + q * peri
      d/dt(peri) <- q * central - q * peri
      cp <- central / v
      if (t < 1) {
        evid_(t + 6, 4, 50, 1, 0, 6, 9, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central - q * central + q * peri
      d/dt(peri) <- q * central - q * peri
      cp <- central / v
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 72, by = 1))
    # evid=4 resets only on the FIRST addl repetition (t=6, matching NONMEM --
    # see rxode2#1351); write the reference the same way, evid=4 once then
    # plain doses, rather than evid=4 at every repeat.
    eBase <- e |> et(amt = 100, time = 0, cmt = 2) |> et(amt = 100, time = 0, cmt = 3) |>
      et(amt = 50, time = 6, cmt = 1, evid = 4) |>
      et(amt = 50, time = 6, cmt = 2) |>
      et(amt = 50, time = 6, cmt = 3)
    for (.t in seq(12, 60, by = 6)) {
      eBase <- eBase |>
        et(amt = 50, time = .t, cmt = 1) |>
        et(amt = 50, time = .t, cmt = 2) |>
        et(amt = 50, time = .t, cmt = 3)
    }
    p <- c(ka = 0.5, cl = 1, v = 10, q = 0.3)
    rSplit <- rxSolve(mSplit, p, e)
    rBase <- rxSolve(mBase, p, eBase)
    expect_true(all(is.finite(rSplit$cp)))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
    expect_equal(rSplit$peri, rBase$peri, tolerance = 1e-5)
  })

  test_that("a split bolus pushed as evid=1 reserves enough room (#1322 follow-up)", {
    # the same under-reservation as the evid=4 case above, on the commoner
    # spelling: one translated event, splitBolusN-1 records
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1) {
        evid_(t + 6, 1, 50, 1, 0, 6, 9, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 72, by = 1))
    eBase <- e |> et(amt = 100, time = 0, cmt = 2)
    for (.t in seq(6, 60, by = 6)) {
      eBase <- eBase |>
        et(amt = 50, time = .t, cmt = 1) |>
        et(amt = 50, time = .t, cmt = 2)
    }
    p <- c(ka = 0.5, cl = 1, v = 10)
    rSplit <- rxSolve(mSplit, p, e)
    rBase <- rxSolve(mBase, p, eBase)
    expect_true(all(is.finite(rSplit$cp)))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })

  test_that("a pushed infusion into a splitBolus compartment is not split", {
    # _rxShouldSplitTranslatedBolus() requires whI == 0, so only a bolus splits;
    # an infusion into the same compartment stays whole
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuse(100, 10, 1, 0, 0, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    p <- c(ka = 0.5, cl = 1, v = 10)
    obs <- c(0, 1e-8, seq(0.5, 30, by = 0.5))
    rSplit <- rxSolve(mSplit, p, et(obs))
    rBase <- rxSolve(mBase, p, et(amt = 100, time = 0, rate = 10, cmt = 1) |> et(obs))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })

  test_that("pushed fixed-rate and bolus doses are unchanged", {
    modRate <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(modRate, et(amt = 100, time = 2, rate = 10))

    modBolus <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(modBolus, et(amt = 100, time = 2))
  })

  test_that("a pushed steady state constant infusion with a duration errs (#1350)", {
    # flg 40 (ss=1, ii=0, amt=0) never turns off, so pairing it with a duration
    # -- modeled (rate=-2) or fixed -- gives no usable rate and used to
    # silently steady-state the compartment to zero instead of erring the way
    # the same combination already does in the event table.
    modModeledDur <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 1, 0, 1, -2, 0, 0, 1)
      }
    })
    expect_error(rxSolve(modModeledDur, .pars, et(.obs)),
                 "makes no sense")

    modFixedDur <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuseDur(0, 10, 1, 0, 0, 1)
      }
    })
    expect_error(rxSolve(modFixedDur, .pars, et(.obs)),
                 "makes no sense")

    modModeledDurReset <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 4, 0, 1, -2, 0, 0, 1)
      }
    })
    expect_error(rxSolve(modModeledDurReset, .pars, et(.obs)),
                 "makes no sense")
  })

  test_that("a hand-encoded classic internal evid cannot bypass the flg-40 duration guard (#1350)", {
    # evid_()'s "evid" documents evid >= 100 as a supported hand-encoded
    # classic rxode2 form (cmt100*100000 + rateI*10000 + cmt99*100 + flg),
    # passed through _rxTranslateOneEvent() verbatim rather than through
    # _rxTranslateDoseInto() -- so the guard above has to be duplicated for
    # this path or a hand-encoded evid reproduces the same silent zero.
    # cmt99=1: rateI=2 (fixed dur) -> 2*10000 + 1*100 + 40 = 20140
    modFixedDur <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 20140, 0, 1, 0, 0, 0, 0)
      }
    })
    expect_error(rxSolve(modFixedDur, .pars, et(.obs)), "makes no sense")

    # rateI=8 (modeled dur) -> 8*10000 + 1*100 + 40 = 80140
    modModeledDur <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 80140, 0, 1, 0, 0, 0, 0)
      }
    })
    expect_error(rxSolve(modModeledDur, .pars, et(.obs)), "makes no sense")

    # control: rateI=1 (fixed rate) at the same flg=40 must keep working --
    # 1*10000 + 1*100 + 40 = 10140; amt carries the rate for rateI 1/2
    modFixedRate <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 10140, 10, 1, 0, 0, 0, 0)
      }
    })
    got <- rxSolve(modFixedRate, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 0, time = 0, rate = 10, ss = 1, ii = 0) |> et(.obs))
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("infuseDur()'s duration slot rejects the modeled-rate column mistake at flg 40 (#1350)", {
    # infuseDur() reuses _rxPushDose's "rate" slot to carry its "dur" argument
    # (isDur set), so a NONMEM-style column mistake -- writing -1 (modeled
    # RATE's sentinel) into a DURATION -- collapses onto the same rateI=9 as
    # evid_()'s legitimate rate=-1 spelling.  etTran.cpp still rejects that
    # mistake for the event table when flg=40 (the rate/dur distinction is
    # column-based there), so the push path has to track which argument slot
    # produced rateI=9 to reject it too, via the isDur bit.
    modColumnMistake <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        infuseDur(0, -1, 1, 0, 0, 1)
      }
    })
    expect_error(rxSolve(modColumnMistake, .pars, et(.obs)), "makes no sense")

    # control: the identical rateI=9 reached via evid_()'s "rate" argument
    # (isDur unset) is the legitimate spelling and must keep working
    modLegit <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 1, 0, 1, -1, 0, 0, 1)
      }
    })
    got <- rxSolve(modLegit, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 0, time = 0, rate = 10, ss = 1, ii = 0) |> et(.obs))
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed steady state constant infusion by fixed rate is unchanged (#1350)", {
    # the legitimate spelling -- a constant infusion by fixed RATE -- must keep
    # working; only pairing flg 40 with a duration is rejected.  (The modeled
    # rate sibling is already covered by "a pushed modeled rate constant
    # infusion (ss=1, ii=0, amt=0) solves" above.)
    modFixedRate <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuse(0, 10, 1, 0, 0, 1)
      }
    })
    got <- rxSolve(modFixedRate, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 0, time = 0, rate = 10, ss = 1, ii = 0) |> et(.obs))
    # the push happens during the first evaluation, so the steady state is in
    # place from the next output row onward rather than at time 0 itself
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

})
