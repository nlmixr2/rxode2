rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # evid = 4 is a reset followed by a dose at the same time; the dose's
  # bioavailability dual must use the costate before the reset zeroes it.
  .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- bio"
  .p <- c(ka = 1.2, k = 0.2, bio = 0.8)
  .cs <- names(.p)
  .scen <- list(
    evid4 = et(amt = 100, cmt = "depot") |> et(time = 6, evid = 4, amt = 80, cmt = "depot") |>
      et(c(1, 3, 6, 6.5, 7, 10, 13)),
    # the same regimen written as a reset and a dose
    resetDose = et(amt = 100, cmt = "depot") |> et(time = 6, evid = 3) |>
      et(time = 6, amt = 80, cmt = "depot") |> et(c(1, 3, 6, 6.5, 7, 10, 13))
  )
  # a replace or multiply right after a reset at the same time keeps the reset
  .scenJump <- list(
    resetReplace = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |>
      et(time = 2, evid = 5, amt = 7, cmt = "center") |> et(c(0, 1, 2, 3, 4)),
    resetMultiply = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |>
      et(time = 2, evid = 6, amt = 0.5, cmt = "center") |> et(c(0, 1, 2, 3, 4))
  )
  .check <- function(scen) {
    for (.nm in names(scen)) {
      .ev <- scen[[.nm]]
      .ref <- .adjFd(.txt, .ev, .p)
      # one method per backward sweep, liblsodaadj and cvodesadj
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")) {
        .a <- rxSolveAdjointRk4(.txt, .ev,
          params = .p, calcSens = .cs, method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  }

  test_that("a reset followed by a dose keeps the dose's bioavailability sensitivity", {
    for (.nm in names(.scen)) {
      .ev <- .scen[[.nm]]
      # the post-reset dose's d/d(bio) is genuinely nonzero
      expect_gt(max(abs(.adjFd(.txt, .ev, .p)$rx__sens_depot_BY_bio__[.ev$time[is.na(.ev$evid) | .ev$evid == 0] > 6])), 10)
    }
    .check(.scen)
  })

  test_that("a replace or multiply at a reset's time keeps the reset in every driver", {
    .check(.scenJump)
  })

  test_that("a dose together with a replace or multiply at one time keeps its bioavailability", {
    # Same-time records sort as multiply, replace, then dose.  A replace with 0
    # or a multiply of an empty compartment leaves no f() term on the jump
    # itself.  liblsodaadj keeps one event per segment and cvodesadj cannot
    # yet transpose same-time events, so this covers the rk4s family.
    .scenSame <- list(
      replaceDose = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 5, amt = 0, cmt = "depot") |>
        et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 3, 4, 6)),
      multiplyDose = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0, cmt = "depot") |>
        et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 3, 4, 6)),
      multiplyDoseT0 = et(time = 0, evid = 6, amt = 3, cmt = "depot") |> et(amt = 100, cmt = "depot") |>
        et(c(0, 1, 3))
    )
    for (.nm in names(.scenSame)) {
      .ev <- .scenSame[[.nm]]
      .ref <- .adjFd(.txt, .ev, .p)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s")) {
        .a <- rxSolveAdjointRk4(.txt, .ev,
          params = .p, calcSens = .cs, method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("the liblsodaadj self-check replay agrees with the adjoint for a reset plus a dose", {
    withr::local_envvar(RX_LSADJ_SELFCHECK = "1")
    for (.nm in names(.scen)) {
      .out <- utils::capture.output(
        rxSolveAdjointRk4(.txt, .scen[[.nm]], params = .p, calcSens = .cs, method = "liblsodaadj", cores = 1),
        type = "message"
      )
      .line <- grep("max\\|adjoint - discrete_forward_sens\\|", .out, value = TRUE)
      expect_length(.line, 1L)
      .err <- as.numeric(sub("^.*discrete_forward_sens\\| = ([^ ]+) .*$", "\\1", .line))
      expect_lt(.err, 1e-8, label = .nm)
    }
  })
})
