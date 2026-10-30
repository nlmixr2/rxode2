rxTest({
  # `_rxTestSolveWarnLabels(idLvl, ids, setLvl, maxIds)`: set rx_global's ID
  # factor levels from `idLvl` (unless `setLvl` is FALSE), push one aggregated
  # warning per internal solve id in `ids`, then flush listing `maxIds` subjects.
  .warnLabels <- function(idLvl, ids, setLvl = TRUE, maxIds = 64L) {
    .out <- capture.output(
      .Call("_rxTestSolveWarnLabels", idLvl, ids, setLvl, as.integer(maxIds), PACKAGE = "rxode2")
    )
    grep("warning\\(s\\)", .out, value = TRUE)
  }

  test_that("aggregated solve warnings label subjects by their real id", {
    # rxSetIdLvlFactors() populates rx_global's ID factor table; the aggregated
    # warning flush (rxSolveWarnFlush) then resolves each internal solve id to
    # the real subject id via rxGetIdSim.  See src/solveWarn.cpp /
    # src/par_solve.cpp.
    .line <- .warnLabels(c("101", "202", "303"), c(0L, 2L))
    expect_equal(length(.line), 1L)
    # internal ids 0 and 2 map to the 1st and 3rd levels, printed in id order.
    expect_match(.line, "for subject\\(s\\): 101, 303")
    expect_false(any(grepl("Unknown", .line)))
  })

  test_that("unresolved subject id falls back to the 1-based internal index", {
    # With no id factor table (empty idLvl), no label exists; the flush must
    # print "internal #N" (1-based) rather than a bare "Unknown".
    .line <- .warnLabels(character(0), 4L)
    expect_equal(length(.line), 1L)
    expect_match(.line, "for subject\\(s\\): internal #5")
    expect_false(any(grepl("Unknown", .line)))
  })

  test_that("an id past the end of the levels also falls back", {
    # nsub/nsim are whatever the last solve left; the levels here are not a
    # simulation's worth of subjects, so there is nothing to resolve.
    expect_match(.warnLabels(c("101", "202"), c(1L, 7L)), "for subject\\(s\\): 202, internal #8")
  })

  test_that("past 256 subjects for one message the remainder is a lower bound", {
    # one message keeps at most 256 distinct subjects; the flush lists 64
    .line <- function(n, rest) {
      paste0(
        "[rxTest -- solve warning]: ",
        n,
        " warning(s) for subject(s): ",
        paste0("internal #", 1:64, collapse = ", "),
        ", ... (",
        rest,
        " more)"
      )
    }
    expect_identical(.warnLabels(character(0), 0:299), .line(300, "at least 193"))
    expect_identical(.warnLabels(character(0), 0:255), .line(256, "192"))
    expect_identical(.warnLabels(character(0), 0:99), .line(100, "36"))
    # a repeat of a stored subject is not one more
    expect_identical(.warnLabels(character(0), c(0:255, 0:255)), .line(512, "192"))
    # listing every stored subject still says one more was seen
    expect_identical(
      .warnLabels(character(0), 0:299, maxIds = 300L),
      paste0(
        "[rxTest -- solve warning]: 300 warning(s) for subject(s): ",
        paste0("internal #", 1:256, collapse = ", "),
        ", ... (at least 1 more)"
      )
    )
  })

  test_that("a subject literally named Unknown is still printed as itself", {
    # The fallback must be driven by whether the id resolves, not by string
    # comparison against "Unknown" -- that is a legal subject id.
    .line <- .warnLabels(c("A", "Unknown", "C"), 1L)
    expect_match(.line, "for subject\\(s\\): Unknown")
    expect_false(any(grepl("internal #", .line)))
  })

  test_that("rxSetIdLvlFactors makes rxGetId resolve real subject ids", {
    .warnLabels(c("alpha", "beta", "gamma"), integer(0))
    expect_equal(
      .Call("_rxTestGetIdLabels", c(0L, 1L, 2L), PACKAGE = "rxode2"),
      c("alpha", "beta", "gamma")
    )
    # out-of-range id is reported as "Unknown" by rxGetId itself (the flush is
    # what converts that to the "internal #N" fallback).
    expect_equal(.Call("_rxTestGetIdLabels", 9L, PACKAGE = "rxode2"), "Unknown")
    expect_equal(.Call("_rxTestGetIdLabels", -1L, PACKAGE = "rxode2"), "Unknown")
  })

  test_that("rxSetIdLvlFactors coerces a numeric idLvl and rejects the rest", {
    # An estimation host whose ID column is numeric can pass it as-is.
    .warnLabels(c(11, 22, 33), integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", c(0L, 2L), PACKAGE = "rxode2"), c("11", "33"))
    .warnLabels(4:6, integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", 1L, PACKAGE = "rxode2"), "5")
    .warnLabels(c(TRUE, FALSE), integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", c(0L, 1L), PACKAGE = "rxode2"), c("TRUE", "FALSE"))
    # A type that cannot name subjects must clear the table rather than read
    # through a non-character SEXP; the flush then falls back.
    for (.bad in list(NULL, list("a", "b"), TRUE ~ FALSE)) {
      expect_match(.warnLabels(.bad, 0L), "for subject\\(s\\): internal #1")
    }
  })

  test_that("a solve past the ID levels is labelled by subject and simulation", {
    # The ID levels only ever cover one simulation's worth of subjects, but a
    # multiple-simulation solve runs nsub*nsim of them, laid out
    # simulation-major.  Solve index nsub is subject 1 of simulation 2, not an
    # unknown subject -- it used to print a meaningless "internal #4".
    .f <- function() {
      ini({
        tka <- 0.5
        tcl <- -3.2
        tv <- -1
        eta.cl ~ 0.1
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
      })
    }
    .e <- et(amt = 100) |> et(seq(0, 24, 4)) |> et(id = 1:3)
    withr::with_seed(42, rxSolve(.f, .e, nStud = 2, addDosing = FALSE))
    # rxGetId() alone cannot see past the levels ...
    expect_equal(.Call("_rxTestGetIdLabels", 0:5, PACKAGE = "rxode2"), c("1", "2", "3", rep("Unknown", 3)))
    # ... but the flush resolves them, and says which simulation they came from
    expect_match(.warnLabels(NULL, c(1L, 4L), setLvl = FALSE), "for subject\\(s\\): 2, 2 \\(sim 2\\)")
    # past nsub*nsim there is still nothing to resolve
    expect_match(.warnLabels(NULL, 20L, setLvl = FALSE), "for subject\\(s\\): internal #21")
    # and once a host overwrites the levels itself, nsub/nsim no longer
    # describe them, so the expansion must not fire off the stale pair
    expect_match(.warnLabels(c("1", "2", "3"), 4L), "for subject\\(s\\): internal #5")
  })

  test_that("nSub= replicated subjects are labelled by their sim.id", {
    # `nSub=` replicates a one-subject table as extra *simulations* of the one
    # subject (nsub stays 1, nsim becomes 4), and the output carries only a
    # sim.id column running 1..nsub*nsim -- no id column, since rxode2_df.cpp
    # emits one only when nsub > 1.  So the label a user can match here is the
    # simulation number, and "1 (sim 3)" is the row whose sim.id is 3.
    .f <- function() {
      ini({
        tka <- 0.5
        tcl <- -3.2
        tv <- -1
        eta.cl ~ 0.1
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
      })
    }
    .e <- et(amt = 100) |> et(seq(0, 24, 4))
    withr::with_seed(1, rxSolve(.f, .e, nSub = 4, addDosing = FALSE))
    expect_match(.warnLabels(NULL, c(0L, 2L), setLvl = FALSE), "for subject\\(s\\): 1, 1 \\(sim 3\\)")
    # nSub= and nStud= together just multiply out to nsim = 8
    withr::with_seed(1, rxSolve(.f, .e, nSub = 4, nStud = 2, addDosing = FALSE))
    expect_match(.warnLabels(NULL, c(0L, 5L), setLvl = FALSE), "for subject\\(s\\): 1, 1 \\(sim 6\\)")
    # and past nsub*nsim there is nothing to resolve
    expect_match(.warnLabels(NULL, 8L, setLvl = FALSE), "for subject\\(s\\): internal #9")
  })

  test_that("the id levels are left cleared for the rest of the suite", {
    # These tests write rx_global's ID factor table directly; later test files
    # read it back when a solve builds its output data frame, so hand it back
    # empty.
    .warnLabels(character(0), integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", 0L, PACKAGE = "rxode2"), "Unknown")
  })

  test_that("a summarized warning value gives mean +/- sd (min/max), or itself once", {
    .stat <- function(x) {
      .out <- capture.output(.Call("_rxTestSolveWarnStat", x, PACKAGE = "rxode2"))
      grep("warning\\(s\\)", .out, value = TRUE)
    }
    .x <- c(1, 2, 4, 8)
    expect_identical(
      .stat(.x),
      sprintf(
        "[rxTest -- x = %g +/- %g (%g/%g), -x = %g +/- %g (%g/%g), n = 3]: 4 warning(s)",
        mean(.x),
        sd(.x),
        min(.x),
        max(.x),
        -mean(.x),
        sd(.x),
        -max(.x),
        -min(.x)
      )
    )
    expect_identical(.stat(2.5), "[rxTest -- x = 2.5, -x = -2.5, n = 3]: 1 warning(s)")
    # a large offset does not lose the spread (Welford, not sum of squares)
    .x <- 1e8 + c(1, 2, 3)
    expect_identical(
      .stat(.x),
      sprintf(
        "[rxTest -- x = %g +/- %g (%g/%g), -x = %g +/- %g (%g/%g), n = 3]: 3 warning(s)",
        mean(.x),
        sd(.x),
        min(.x),
        max(.x),
        -mean(.x),
        sd(.x),
        -max(.x),
        -min(.x)
      )
    )
  })

  test_that("a dop853 step-limit warning keys on nmax and summarizes x over subjects", {
    .m <- rxode2({
      d/dt(central) <- -0.1 * central
    })
    .ev <- et(amt = 100) |> et(seq(0, 200, by = 50)) |> et(id = 1:2)
    .out <- capture.output(suppressWarnings(rxSolve(.m, .ev, method = "dop853", maxsteps = 5L)))
    # one line: dop853's own, with no IDID line besides
    .line <- grep("^\\[", .out, value = TRUE)
    expect_length(.line, 1L)
    expect_match(
      .line,
      "^\\[exit of dop853 at x = [-0-9.e+]+ \\+/- [-0-9.e+]+ \\([-0-9.e+]+/[-0-9.e+]+\\), more than nmax = 5 are needed\\]: 2 warning\\(s\\) for subject\\(s\\): 1, 2$"
    )
  })

  test_that("a dop853 step size that is not finite gives one line, with its x", {
    .m <- rxode2("d/dt(y) <- -k * y")
    .d <- data.frame(
      ID = rep(101:104, each = 4),
      TIME = rep(c(0, 0, 1, 2), 4),
      EVID = rep(c(1, 0, 0, 0), 4),
      AMT = rep(c(1, 0, 0, 0), 4),
      CMT = "y"
    )
    # a NaN derivative makes the step size NaN; an infinite one makes it 0
    .want <- c(
      "NaN" = "^\\[exit of dop853 at x = [-0-9.e+]+, step size is not finite\\]: 1 warning\\(s\\) for subject\\(s\\): 103$",
      "Inf" = "^\\[exit of dop853 at x = [-0-9.e+]+, step size too small h = [-0-9.e+]+\\]: 1 warning\\(s\\) for subject\\(s\\): 103$"
    )
    for (.k in names(.want)) {
      .p <- data.frame(ID = 101:104, k = c(1, 1, as.numeric(.k), 1))
      .out <- capture.output(suppressWarnings(rxSolve(.m, .d, params = .p, method = "dop853")))
      .line <- grep("^\\[", .out, value = TRUE)
      expect_length(.line, 1L)
      expect_match(.line, .want[[.k]], label = .k)
    }
  })

  test_that("a dop853+ros4 solve that switches to ros4 prints no dop853 line", {
    # the composite's dop853 probe hands an interval it cannot step to ros4;
    # that is not a failure, so dop853 says nothing
    .m <- rxode2({
      y1(0) <- 1
      d/dt(y1) <- -0.04 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- 0.04 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    })
    .ev <- et(c(0.1, 1, 10, 100, 1000, 1e4, 1e5))
    .w <- capture_warnings(
      .out <- capture.output(.s <- as.data.frame(rxSolve(.m, .ev, method = "dop853+ros4", atol = 1e-6, rtol = 1e-6)))
    )
    expect_identical(.w, character(0))
    expect_identical(.out, character(0))
    expect_equal(.s$y1[.s$time == 1e5], 0.01786593, tolerance = 1e-5)
  })

  test_that("the dop853 step-size and intdy warnings give their x, h and t", {
    .m <- rxode2({
      y(0) <- y0
      d/dt(y) <- y^2
    })
    # subject 2 blows up at t = 1; subject 1 does not
    .ev <- et(seq(0, 2, by = 0.5)) |> et(id = 1:2)
    .p <- data.frame(y0 = c(0.1, 1))
    .out <- capture.output(suppressWarnings(rxSolve(.m, .ev, params = .p, method = "dop853")))
    .line <- grep("^\\[", .out, value = TRUE)
    expect_length(.line, 1L)
    expect_match(
      .line,
      "^\\[exit of dop853 at x = [-0-9.e+]+, step size too small h = [-0-9.e+]+\\]: 1 warning\\(s\\) for subject\\(s\\): 2$"
    )
    .out <- capture.output(suppressWarnings(rxSolve(.m, .ev, params = .p, method = "liblsoda")))
    .line <- grep("intdy", .out, value = TRUE)
    expect_length(.line, 1L)
    expect_match(
      .line,
      "^\\[intdy -- t = ([-0-9.e+]+|[-0-9.e+]+ \\+/- [-0-9.e+]+ \\([-0-9.e+]+/[-0-9.e+]+\\)) illegal\\]: [1-9][0-9]* warning\\(s\\) for subject\\(s\\): 2$"
    )
  })
})
