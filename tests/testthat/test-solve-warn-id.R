rxTest({
  # `_rxTestSolveWarnLabels(idLvl, ids, setLvl)`: set rx_global's ID factor
  # levels from `idLvl` (unless `setLvl` is FALSE), push one aggregated warning
  # per internal solve id in `ids`, then flush.
  .warnLabels <- function(idLvl, ids, setLvl = TRUE) {
    .out <- capture.output(
      .Call("_rxTestSolveWarnLabels", idLvl, ids, setLvl, PACKAGE = "rxode2")
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
    expect_match(.warnLabels(c("101", "202"), c(1L, 7L)),
                 "for subject\\(s\\): 202, internal #8")
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
    expect_equal(.Call("_rxTestGetIdLabels", c(0L, 2L), PACKAGE = "rxode2"),
                 c("11", "33"))
    .warnLabels(4:6, integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", 1L, PACKAGE = "rxode2"), "5")
    .warnLabels(c(TRUE, FALSE), integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", c(0L, 1L), PACKAGE = "rxode2"),
                 c("TRUE", "FALSE"))
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
    .e <- et(amt = 100) %>% et(seq(0, 24, 4)) %>% et(id = 1:3)
    withr::with_seed(42, rxSolve(.f, .e, nStud = 2, addDosing = FALSE))
    # rxGetId() alone cannot see past the levels ...
    expect_equal(.Call("_rxTestGetIdLabels", 0:5, PACKAGE = "rxode2"),
                 c("1", "2", "3", rep("Unknown", 3)))
    # ... but the flush resolves them, and says which simulation they came from
    expect_match(.warnLabels(NULL, c(1L, 4L), setLvl = FALSE),
                 "for subject\\(s\\): 2, 2 \\(sim 2\\)")
    # past nsub*nsim there is still nothing to resolve
    expect_match(.warnLabels(NULL, 20L, setLvl = FALSE),
                 "for subject\\(s\\): internal #21")
  })

  test_that("the id levels are left cleared for the rest of the suite", {
    # These tests write rx_global's ID factor table directly; later test files
    # read it back when a solve builds its output data frame, so hand it back
    # empty.
    .warnLabels(character(0), integer(0))
    expect_equal(.Call("_rxTestGetIdLabels", 0L, PACKAGE = "rxode2"), "Unknown")
  })
})
