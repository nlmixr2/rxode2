rxTest({
  # Keep the ODE adjoint path under test: the automatic ODE -> linCmt()
  # translation would otherwise collapse these linear models onto linCmt().
  withr::local_options(rxode2.useLinCmt = FALSE)

  # Every kind of statement .rxAdjointExpand() must carry from the original
  # model into the expansion, each with events and parameters to solve it.
  .ev2 <- et(seq(0, 4, by = 0.5))
  .evDose <- et(amt = 100, cmt = "depot") |> et(c(1, 2, 4, 8, 12))
  .adjCarryKinds <- list(
    ini = list(
      txt = "y(0) <- 2\nd/dt(y) <- -k*y",
      ev = .ev2, p = c(k = 0.5)
    ),
    lhs = list(
      txt = "ka2 <- ka*2\nd/dt(depot) <- -ka2*depot\ncp <- center/v\nd/dt(center) <- ka2*depot - k*center\nauc <- cp*2",
      ev = .evDose, p = c(ka = 1.2, v = 25, k = 0.14)
    ),
    dosing = list(
      txt = "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd\nalag(depot) <- lg",
      ev = .evDose, p = c(ka = 1.2, k = 0.14, fd = 0.7, lg = 0.7)
    ),
    rate = list(
      txt = "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nrate(depot) <- r",
      ev = et(amt = 100, cmt = "depot", rate = -1) |> et(c(1, 2, 4, 8, 12)), p = c(ka = 1.2, k = 0.14, r = 40)
    ),
    dur = list(
      txt = "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\ndur(depot) <- dd",
      ev = et(amt = 100, cmt = "depot", rate = -2) |> et(c(1, 2, 4, 8, 12)), p = c(ka = 1.2, k = 0.14, dd = 2.5)
    ),
    ifelse = list(
      txt = "if (t > 2) { kk <- k*2 } else { kk <- k }\nd/dt(y) <- -kk*y\ny(0) <- y0",
      ev = .ev2, p = c(k = 0.5, y0 = 3)
    ),
    default = list(
      txt = "k <- 0.5\nd/dt(y) <- -k*y\ny(0) <- 2",
      ev = .ev2, p = c(k = 1)
    ),
    string = list(
      txt = "d/dt(y) <- -k*y\nif (t > 1) {label <- \"a\"} else {label <- \"b\"}\nz <- y\ny(0) <- 1",
      ev = .ev2, p = c(k = 1)
    ),
    # a string output after a numeric one keeps its column position
    stringLate = list(
      txt = "d/dt(y) <- -k*y\nz <- y*2\nif (t > 1) {label <- \"a\"} else {label <- \"b\"}\nw <- z + 1\ny(0) <- 1",
      ev = .ev2, p = c(k = 1)
    ),
    mtime = list(
      txt = "d/dt(y) <- -k*y\nmtime(t1) <- 1.5\nw <- t1*k\ny(0) <- 1",
      ev = .ev2, p = c(k = 1)
    ),
    param = list(
      txt = "param(zz, k)\nd/dt(y) <- -k*y\nz <- y*zz\ny(0) <- 2",
      ev = .ev2, p = c(k = 0.5, zz = 2)
    ),
    order = list(
      txt = "cmt(y)\nd/dt(x) <- -k*x\nd/dt(y) <- k*x - k2*y\nx(0) <- 4\ndvid(2, 1)",
      ev = .ev2, p = c(k = 0.5, k2 = 0.3)
    ),
    extra = list(
      txt = "d/dt(y) <- -k*y\ncmt(extra)\ny(0) <- 2",
      ev = .ev2, p = c(k = 0.5)
    ),
    jac = list(
      txt = "d/dt(y) <- -k*y\ndf(y)/dy(y) <- -k\ny(0) <- 2",
      ev = .ev2, p = c(k = 0.5)
    ),
    adaptive = list(
      txt = "d/dt(y) <- -k*y\nif (t < 1) {\n bolus(50, 1, 0, 0, 0)\n}",
      ev = .ev2, p = c(k = 1)
    ),
    past = list(
      txt = "d/dt(y) <- -k*delay(y, 1)\ny(0) <- 2\npast(y, 1) <- 3",
      ev = et(c(0.5, 1.5, 2.5, 3.5)), p = c(k = 0.5), dense = TRUE
    )
  )

  # How each rxModelVars() field of the expansion relates to the original's.
  # A field not listed fails the test, so a new field must be classified.
  .adjMvRules <- list(
    identical = c(
      "params", "ini", "normal.state", "normProp", "needSort", "nMtime",
      "extraCmt", "stateExtra", "dvid", "indLin", "slhs", "alag", "udf",
      "interp", "strAssign", "strCmpParams", "splitBolus", "splitInfusion",
      "splitInfusionBolus", "splitBolusInfusion"
    ),
    # the base states come first, the rx__sens_* output slots after them
    prefix = c("state", "state.ignore"),
    byName = c("stateProp", "lhsStr"),
    # extra cmt() states carry an NA name here; stateExtra covers them
    stateOrd = "stateOrd",
    # the rx__adj* lhs come first, the original output columns after them
    lhs = "lhs",
    lhsOrd = "lhsOrd",
    # only the expansion has sensitivity slots
    sens = c("sens", "sensProp"),
    flags = "flags",
    # the expansion supplies its own Jacobian, and only for the stiff methods
    ignored = c("trans", "md5", "model", "version", "timeId", "dfdy")
  )

  test_that("every rxModelVars() field of the original model survives the adjoint expansion", {
    for (.nm in names(.adjCarryKinds)) {
      .k <- .adjCarryKinds[[.nm]]
      .o <- rxModelVars(rxode2(.k$txt))
      .ex <- .rxAdjointExpand(.k$txt, names(.k$p)[1])
      .e <- rxModelVars(rxode2(.ex$text))
      expect_identical(setdiff(names(.o), unlist(.adjMvRules)), character(0), label = .nm)
      for (.f in .adjMvRules$identical) {
        expect_identical(.e[[.f]], .o[[.f]], label = paste(.nm, .f))
      }
      for (.f in .adjMvRules$prefix) {
        expect_identical(.e[[.f]][seq_along(.o[[.f]])], .o[[.f]], label = paste(.nm, .f))
      }
      for (.f in .adjMvRules$byName) {
        expect_identical(.e[[.f]][names(.o[[.f]])], .o[[.f]], label = paste(.nm, .f))
      }
      expect_identical(.e$stateOrd[.o$state], .o$stateOrd[.o$state], label = paste(.nm, "stateOrd"))
      .eLhs <- .e$lhs[!grepl("^(rx__adj|rxCaptureId[0-9]+$)", .e$lhs)]
      expect_identical(.eLhs, .o$lhs, label = paste(.nm, "lhs"))
      expect_identical(
        order(.e$lhsOrd[names(.o$lhsOrd)]), order(.o$lhsOrd),
        label = paste(.nm, "lhsOrd")
      )
      expect_identical(.e$sens, paste0("rx__sens_", .ex$st, "_BY_", names(.k$p)[1], "__"), label = paste(.nm, "sens"))
      # pkTime only governs how `t` is read under rxControl(nonmem = TRUE), and
      # the capture/past lines the expansion emits are classified differently
      .fl <- setdiff(names(.o$flags), "pkTime")
      expect_identical(.e$flags[.fl], .o$flags[.fl], label = paste(.nm, "flags"))
    }
  })

  test_that("the adjoint solve reproduces rxSolve() for every carried statement kind", {
    for (.nm in names(.adjCarryKinds)) {
      .k <- .adjCarryKinds[[.nm]]
      # delay() needs a dense method; the plain model then runs rk4s forward-only
      .base <- if (isTRUE(.k$dense)) "rk4s" else "rk4"
      .a <- rxSolveAdjointRk4(.k$txt, .k$ev, params = .k$p, calcSens = names(.k$p), hmin = 0.01)
      .r <- as.data.frame(rxSolve(rxode2(.k$txt), .k$ev, params = .k$p, method = .base, hmin = 0.01))
      expect_identical(names(.a)[!grepl("^rx__sens_", names(.a))], names(.r), label = .nm)
      expect_identical(grep("^(rx__adj|rxCaptureId)", names(.a), value = TRUE), character(0), label = .nm)
      for (.cn in setdiff(names(.r), "time")) {
        if (is.numeric(.r[[.cn]])) {
          expect_equal(.a[[.cn]], .r[[.cn]], tolerance = 1e-12, label = paste(.nm, .cn))
        } else {
          expect_identical(as.character(.a[[.cn]]), as.character(.r[[.cn]]), label = paste(.nm, .cn))
        }
      }
    }
  })

  test_that("rxSolveAdjointRk4() solves from the model initial condition", {
    .a <- rxSolveAdjointRk4("y(0) <- 1\nd/dt(y) <- -k * y", et(0:2), params = c(k = 1), calcSens = "k")
    expect_equal(.a$y, exp(-(0:2)), tolerance = 1e-6)
    expect_equal(.a$rx__sens_y_BY_k__, -(0:2) * exp(-(0:2)), tolerance = 1e-6)
  })

  # central finite difference of rxSolve() for every state and parameter
  .adjFd <- function(txt, ev, p, method, tol = 1e-11) {
    .m <- rxode2(txt)
    .st <- rxStateOde(.m)
    .ret <- list()
    for (.pn in names(p)) {
      .h <- p[[.pn]] * 1e-6
      .pu <- p
      .pd <- p
      .pu[.pn] <- .pu[.pn] + .h
      .pd[.pn] <- .pd[.pn] - .h
      .u <- as.data.frame(rxSolve(.m, ev, params = .pu, method = method, atol = tol, rtol = tol))
      .d <- as.data.frame(rxSolve(.m, ev, params = .pd, method = method, atol = tol, rtol = tol))
      for (.s in .st) {
        .ret[[paste0("rx__sens_", .s, "_BY_", .pn, "__")]] <- (.u[[.s]] - .d[[.s]]) / (2 * .h)
      }
    }
    .ret
  }

  # closed form of a one-dose model with center(0) = c0, bolus fd*amt landing
  # at lg; the gradient over the parameters in `p`
  .pkClosed <- function(p, tt, fd = "1", lg = "0", c0 = "10") {
    .e <- sprintf(
      "~ %s * 100 * ka / (ka - k) * (exp(-k * (t - %s)) - exp(-ka * (t - %s))) + %s * exp(-k * t)",
      fd, lg, lg, c0
    )
    .eD <- sprintf("~ %s * 100 * exp(-ka * (t - %s))", fd, lg)
    .gC <- attr(do.call(deriv(stats::as.formula(.e), names(p), function.arg = c(names(p), "t")), c(as.list(p), list(t = tt))), "gradient")
    .gD <- attr(do.call(deriv(stats::as.formula(.eD), names(p), function.arg = c(names(p), "t")), c(as.list(p), list(t = tt))), "gradient")
    .ret <- list()
    for (.pn in names(p)) {
      .ret[[paste0("rx__sens_depot_BY_", .pn, "__")]] <- unname(.gD[, .pn])
      .ret[[paste0("rx__sens_center_BY_", .pn, "__")]] <- unname(.gC[, .pn])
    }
    .ret
  }

  test_that("adjoint sensitivities of models with y(0), f() and alag() match the closed form", {
    .tt <- c(1, 2, 4, 8, 12)
    .mods <- list(
      ini = list(
        txt = "y(0) <- 2\nd/dt(y) <- -k*y", ev = et(.tt), p = c(k = 0.5),
        known = list(rx__sens_y_BY_k__ = -.tt * 2 * exp(-0.5 * .tt))
      ),
      f = list(
        txt = "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd\ncenter(0) <- 10",
        ev = .evDose, p = c(ka = 1.2, k = 0.14, fd = 0.7)
      ),
      alag = list(
        txt = "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nalag(depot) <- lg\ncenter(0) <- 10",
        ev = .evDose, p = c(ka = 1.2, k = 0.14, lg = 0.7)
      )
    )
    .mods$f$known <- .pkClosed(.mods$f$p, .tt, fd = "fd")
    .mods$alag$known <- .pkClosed(.mods$alag$p, .tt, lg = "lg")
    # one method per backward sweep
    for (.nm in names(.mods)) {
      .k <- .mods[[.nm]]
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(
          .k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        .a <- .a[.a$time %in% .tt, , drop = FALSE]
        for (.cn in names(.k$known)) {
          expect_lt(max(abs(.a[[.cn]] - .k$known[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  # One method per backward sweep: explicit RK, Rosenbrock, implicit RK, the
  # explicit/Rosenbrock composite, liblsoda, CVODES and Adams-Bashforth.
  .adjIniMethods <- c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")
  .adjIniMods <- list(
    one = list(
      txt = "y(0) <- y0\nd/dt(y) <- -k*y",
      ev = et(c(0, 0.5, 1, 2, 3)), p = c(k = 0.7, y0 = 3)
    ),
    two = list(
      txt = "x(0) <- a*b\nd/dt(x) <- -k*x\nd/dt(z) <- k*x - k2*z",
      ev = et(c(0, 0.5, 1, 2, 3)), p = c(a = 2, b = 1.5, k = 0.7, k2 = 0.3)
    ),
    reset = list(
      txt = "y(0) <- y0\nd/dt(y) <- -k*y",
      ev = et(c(0, 0.5, 1, 2.5, 3)) |> et(time = 2, evid = 3), p = c(k = 0.7, y0 = 3)
    )
  )

  test_that("parameter-dependent initial conditions get their adjoint sensitivity", {
    for (.nm in names(.adjIniMods)) {
      .k <- .adjIniMods[[.nm]]
      .fd <- .adjFd(.k$txt, .k$ev, .k$p, "liblsoda", tol = 1e-13)
      for (.m in .adjIniMethods) {
        # hmin sets the fixed step; it would only constrain the adaptive methods
        .a <- rxSolveAdjointRk4(
          .k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        .a <- .a[.a$time %in% .k$ev$time[is.na(.k$ev$evid) | .k$ev$evid == 0], , drop = FALSE]
        for (.cn in names(.fd)) {
          expect_lt(max(abs(.a[[.cn]] - .fd[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("a subject observed only at its first time reads dX0/dtheta", {
    .txt <- "y(0) <- y0\nd/dt(y) <- -k*y"
    .p <- c(k = 0.7, y0 = 3)
    # one subject, observed only at t0 (once and twice)
    for (.ev in list(et(0), et(c(0, 0)))) {
      for (.m in .adjIniMethods) {
        .a <- rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = c("k", "y0"), method = .m)
        expect_equal(.a$rx__sens_y_BY_y0__, rep(1, nrow(.a)), tolerance = 1e-12, label = .m)
        expect_equal(.a$rx__sens_y_BY_k__, rep(0, nrow(.a)), tolerance = 1e-12, label = .m)
      }
    }
    # three subjects, the second observed only at baseline
    .ev <- rbind(
      data.frame(id = 1L, time = c(0, 1, 2), evid = 0L),
      data.frame(id = 2L, time = 0, evid = 0L),
      data.frame(id = 3L, time = c(0, 0.5, 3), evid = 0L)
    )
    for (.m in .adjIniMethods) {
      .a <- rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = c("k", "y0"), method = .m,
        atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0)
      expect_equal(.a$rx__sens_y_BY_y0__, exp(-0.7 * .a$time), tolerance = 1e-6, label = .m)
    }
  })

  test_that("a subject with records only at its first time keeps its t0 events", {
    # No forward step is taken, so the sensitivity is dX0/dp changed only by
    # the events at t0, which sort as multiply, replace, then dose.
    .row <- function(id, evid, amt, cmt) data.frame(id = id, time = 0, evid = evid, amt = amt, cmt = cmt)
    .txtF <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd\ncenter(0) <- c0"
    .pF <- c(ka = 1.2, k = 0.14, fd = 0.7, c0 = 2)
    .cases <- list(
      dose = list(ev = rbind(.row(1L, 1L, 100, "depot"), .row(1L, 0L, 0, "depot")), fd = 100, c0 = 1),
      twoDoses = list(ev = rbind(.row(1L, 1L, 100, "depot"), .row(1L, 1L, 50, "depot"),
                                 .row(1L, 0L, 0, "depot"), .row(1L, 0L, 0, "depot")), fd = 150, c0 = 1),
      replace = list(ev = rbind(.row(1L, 1L, 100, "depot"), .row(1L, 5L, 30, "center"),
                                .row(1L, 0L, 0, "depot")), fd = 100, c0 = 0)
    )
    for (.nm in names(.cases)) {
      .k <- .cases[[.nm]]
      for (.m in .adjIniMethods) {
        .a <- rxSolveAdjointRk4(.txtF, .k$ev, params = .pF, calcSens = c("fd", "c0"), method = .m)
        .n <- sum(.k$ev$evid == 0L)
        expect_equal(nrow(.a), .n, label = paste(.nm, .m))
        expect_equal(.a$rx__sens_depot_BY_fd__, rep(.k$fd, .n), tolerance = 1e-12, label = paste(.nm, .m))
        expect_equal(.a$rx__sens_center_BY_c0__, rep(.k$c0, .n), tolerance = 1e-12, label = paste(.nm, .m))
        expect_equal(.a$rx__sens_center_BY_fd__, rep(0, .n), tolerance = 1e-12, label = paste(.nm, .m))
      }
    }
    # a multiply at t0 scales the initial-condition sensitivity
    .ev <- rbind(.row(1L, 6L, 3, "center"), .row(1L, 0L, 0, "center"))
    for (.m in .adjIniMethods) {
      .a <- rxSolveAdjointRk4("d/dt(center) <- -k*center\ncenter(0) <- c0", .ev,
        params = c(k = 0.14, c0 = 2), calcSens = c("k", "c0"), method = .m)
      expect_equal(.a$rx__sens_center_BY_c0__, 3, tolerance = 1e-12, label = .m)
      expect_equal(.a$rx__sens_center_BY_k__, 0, tolerance = 1e-12, label = .m)
    }
  })

  test_that("the initial-condition sensitivity matches its closed form", {
    .tt <- c(0, 0.5, 1, 2, 3)
    .a <- rxSolveAdjointRk4("y(0) <- y0\nd/dt(y) <- -k*y", et(.tt), params = c(k = 0.7, y0 = 3), calcSens = c("k", "y0"))
    expect_equal(.a$rx__sens_y_BY_y0__, exp(-0.7 * .tt), tolerance = 1e-6)
    expect_equal(.a$rx__sens_y_BY_k__, -.tt * 3 * exp(-0.7 * .tt), tolerance = 1e-6)
    # no initial condition depends on a calcSens parameter: no extra lhs block
    expect_identical(.rxAdjointExpand("y(0) <- 2\nd/dt(y) <- -k*y", "k")$dx0Off, -1L)
    expect_gt(.rxAdjointExpand("y(0) <- y0\nd/dt(y) <- -k*y", "y0")$dx0Off, 0L)
  })
})
