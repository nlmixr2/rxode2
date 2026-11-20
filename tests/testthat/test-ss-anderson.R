rxTest({
  # rxControl(ssMethod = "anderson"): accelerated steady state (#1762)
  .mm <- function() {
    ini({
      vm <- 10
      km <- 5
      v <- 50
      ka <- 1
    })
    model({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - vm * (centr / v) / (km + centr / v)
      cp <- centr / v
    })
  }

  test_that("an accumulating Michaelis-Menten steady state matches a long plain iteration in far fewer cycles", {
    # 220 per 24 h against a 240 per 24 h elimination capacity
    .ev <- et(amt = 220, ii = 24, ss = 1) |> et(seq(0, 24, by = 4))
    .ref <- rxSolve(
      .mm,
      .ev,
      method = "dop853",
      atol = 1e-10,
      rtol = 1e-10,
      ssAtol = 1e-12,
      ssRtol = 1e-12,
      maxSS = 1e6
    )$cp
    .aa <- rxSolve(.mm, .ev, method = "dop853", ssMethod = "anderson", maxSS = 50)$cp
    expect_equal(.aa, .ref, tolerance = 1e-6)
    # plain iteration is nowhere near after the same number of cycles
    .plain <- rxSolve(.mm, .ev, method = "dop853", maxSS = 50)$cp
    expect_gt(abs(.plain[1] - .ref[1]), 1)
    # liblsoda gives the same steady state
    .lsoda <- rxSolve(.mm, .ev, ssMethod = "anderson", maxSS = 50)$cp
    expect_equal(.lsoda, .ref, tolerance = 1e-5)
  })

  test_that("dosing above the elimination capacity (no steady state) falls back to plain iteration", {
    for (.amt in c(245, 300, 1000)) {
      .ev <- et(amt = .amt, ii = 24, ss = 1) |> et(0)
      for (.rtol in c(1e-6, 1e-3)) {
        .aa <- rxSolve(.mm, .ev, method = "dop853", ssMethod = "anderson", maxSS = 500, ssRtol = .rtol)$cp
        .plain <- rxSolve(.mm, .ev, method = "dop853", maxSS = 500, ssRtol = .rtol)$cp
        expect_equal(.aa, .plain)
      }
    }
  })

  test_that("a fixed dose count and linCmt() steady states are unchanged", {
    .ev <- et(amt = 220, ii = 24, ss = 1) |> et(seq(0, 24, by = 4))
    expect_identical(
      rxSolve(.mm, .ev, ssMethod = "anderson", minSS = 20, maxSS = 20)$cp,
      rxSolve(.mm, .ev, minSS = 20, maxSS = 20)$cp
    )
    .lin <- function() {
      ini({
        lcl <- log(3)
        lv <- log(20)
        lka <- log(1.1)
      })
      model({
        cl <- exp(lcl)
        v <- exp(lv)
        ka <- exp(lka)
        cp <- linCmt()
      })
    }
    .ev <- et(amt = 100, ii = 12, ss = 1) |> et(seq(0, 12, by = 2))
    expect_identical(
      rxSolve(.lin, .ev, ssMethod = "anderson")$cp,
      rxSolve(.lin, .ev)$cp
    )
  })

  test_that("a linear ODE steady state matches plain iteration", {
    .ode <- function() {
      ini({
        cl <- 3
        v <- 20
        ka <- 1.1
      })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(centr) <- ka * depot - cl / v * centr
        cp <- centr / v
      })
    }
    .ev <- et(amt = 100, ii = 12, ss = 1) |> et(seq(0, 12, by = 2))
    expect_equal(
      rxSolve(.ode, .ev, ssMethod = "anderson")$cp,
      rxSolve(.ode, .ev, ssAtol = 1e-12, ssRtol = 1e-12, maxSS = 1e5)$cp,
      tolerance = 1e-6
    )
  })

  test_that("the accelerated steady state is thread invariant", {
    .ev <- et(amt = 220, ii = 24, ss = 1, id = 1:8) |> et(seq(0, 24, by = 4))
    .p <- data.frame(id = 1:8, vm = seq(9.5, 11, length.out = 8))
    .s1 <- rxSolve(.mm, .ev, .p, ssMethod = "anderson", cores = 1)
    .s2 <- rxSolve(.mm, .ev, .p, ssMethod = "anderson", cores = 2)
    expect_identical(.s1$cp, .s2$cp)
  })

  test_that("forward sensitivities of an accelerated steady state are those of the fixed point", {
    .mText <- "d/dt(depot) = -ka*depot\nd/dt(centr) = ka*depot - vm*(centr/v)/(km + centr/v)"
    .model <- rxS(rxGetModel(.mText), TRUE, promoteLinSens = FALSE)
    .st <- rxStateOde(.model)
    invisible(.rxJacobian(.model, c(.st, "vm")))
    .sens <- .rxSens(.model, "vm")
    .fwd <- rxode2(paste(c(.mText, .sens), collapse = "\n"))
    .ev <- et(amt = 200, ii = 24, ss = 1) |> et(0)
    .p <- c(vm = 10, km = 5, v = 50, ka = 1)
    .solve <- function(p, ...) {
      as.data.frame(rxSolve(
        .fwd,
        p,
        .ev,
        method = "dop853",
        atol = 1e-11,
        rtol = 1e-11,
        ssAtol = 1e-10,
        ssRtol = 1e-10,
        ...
      ))
    }
    .aa <- .solve(.p, ssMethod = "anderson")
    .ref <- .solve(.p, maxSS = 1e6)
    expect_equal(.aa$centr, .ref$centr, tolerance = 1e-7)
    expect_equal(.aa$rx__sens_centr_BY_vm__, .ref$rx__sens_centr_BY_vm__, tolerance = 1e-6)
    .h <- 1e-4
    .fd <- (.solve(base::replace(.p, "vm", 10 + .h), ssMethod = "anderson")$centr -
      .solve(base::replace(.p, "vm", 10 - .h), ssMethod = "anderson")$centr) /
      (2 * .h)
    expect_equal(.aa$rx__sens_centr_BY_vm__, .fd, tolerance = 1e-4)
  })

  test_that("ssMethod is checked", {
    expect_error(rxControl(ssMethod = "newton"))
    expect_equal(rxControl(ssMethod = "anderson")$ssMethod, 1L)
    expect_equal(rxControl()$ssMethod, 0L)
  })
})
