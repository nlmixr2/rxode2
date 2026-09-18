rxTest({
  # A steady-state constant infusion with a duration is rejected, pushed or
  # hand-encoded, while a fixed-rate one is unchanged (#1350).
  .obs <- c(0, 1e-8, seq(0.5, 24, by = 0.5))
  .ref <- rxode2({
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })
  .pars <- c(cl = 1, v = 10)

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
    expect_error(rxSolve(modModeledDur, .pars, et(.obs)), "makes no sense")

    modFixedDur <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuseDur(0, 10, 1, 0, 0, 1)
      }
    })
    expect_error(rxSolve(modFixedDur, .pars, et(.obs)), "makes no sense")

    modModeledDurReset <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 4, 0, 1, -2, 0, 0, 1)
      }
    })
    expect_error(rxSolve(modModeledDurReset, .pars, et(.obs)), "makes no sense")
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
