rxTest({
  # A replace or multiply record (EVID 5/6) sets or scales one linCmt()
  # compartment's amount; rxode2lincmt's slot 10, called from handle_evid(),
  # makes that compartment's Jacobian rows in the state follow.  Every row
  # read here is checked against central differences of the same model's
  # amount in the parameter.

  # a linCmtB() model reading compartment `row`'s amount (am) and its
  # derivative in linCmt() parameter `col` (jr): 0 = cl, 1 = v, and ka at
  # 2 * ncmt when oral.  The parser lays out the state from the arguments
  # (a non-zero ka is a depot), so the ones the model does not use are 0.
  .evJacModel <- function(ncmt, oral0, row, col, extra = "") {
    .a <- sprintf(
      "rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, %s, 0, 0, %s",
      ncmt, oral0, if (ncmt > 1L) "q, v2" else "0, 0", if (oral0 > 0L) "ka" else "0"
    )
    suppressWarnings(rxode2(paste0(
      extra,
      "cp=linCmtB(", sprintf(.a, -1L, -1L), ")\n",
      "am=linCmtB(", sprintf(.a, row, -2L), ")\n",
      "jr=linCmtB(", sprintf(.a, row, col), ")"
    )))
  }

  # doses of 100 at 0 and 41 h into `cmt`, observations at 3, 15, 30 and
  # 50 h, and `rows` (data.frame(time, evid, amt, cmt)) in between
  .evJacData <- function(cmt, rows, nid = 1L) {
    .d <- data.frame(
      time = c(0, 3, 15, 30, 41, 50),
      evid = c(1, 0, 0, 0, 1, 0),
      amt = c(100, 0, 0, 0, 100, 0),
      cmt = cmt
    )
    .d <- rbind(.d, rows)
    .d <- .d[order(.d$time, seq_len(nrow(.d)) > 6), ]
    .n <- nrow(.d)
    .d <- .d[rep(seq_len(.n), nid), ]
    .d$id <- rep(seq_len(nid), each = .n)
    .d
  }

  .evJacSolve <- function(m, d, p, cores) {
    rxSolve(m, params = p, events = d, returnType = "data.frame", cores = cores)
  }

  .evJacPar <- c(cl = 2, v = 20, q = 1, v2 = 30, ka = 1.2)

  # max relative error of jr against the central differences fd; below
  # 1e-3 the differences' rounding (about 1e-9) dominates, so there the
  # error is absolute, scaled by 1e-3
  .evJacRel <- function(jr, fd) {
    max(abs(jr - fd) / pmax(abs(fd), 1e-3))
  }

  # .evJacRel() of jr against central differences of am in `par`, over the
  # observations, solving on `cores`
  .evJacErr <- function(m, d, par, cores = 1L, params = .evJacPar) {
    .h <- 1e-5
    .pp <- params
    .pm <- params
    .pp[par] <- params[par] + .h
    .pm[par] <- params[par] - .h
    .fd <- (.evJacSolve(m, d, .pp, cores)$am - .evJacSolve(m, d, .pm, cores)$am) / (2 * .h)
    .evJacRel(.evJacSolve(m, d, params, cores)$jr, .fd)
  }

  .ev <- function(time, evid, amt, cmt) data.frame(time = time, evid = evid, amt = amt, cmt = cmt)

  test_that("a replace or multiply into a 1-cmt model's central compartment", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    .m <- .evJacModel(1L, 0L, 0L, 0L)
    for (.r in list(.ev(24, 5, 50, 1), .ev(24, 6, 0.5, 1), .ev(24, 5, 0, 1), .ev(24, 6, 0, 1))) {
      expect_lt(.evJacErr(.m, .evJacData(1, .r), "cl"), 1e-6, label = paste(.r$evid, .r$amt))
    }
    # and in v
    .m <- .evJacModel(1L, 0L, 0L, 1L)
    expect_lt(.evJacErr(.m, .evJacData(1, .ev(24, 5, 50, 1)), "v"), 1e-6)
  })

  test_that("a replace or multiply into a 2-cmt model, read in both compartments", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    for (.row in 0:1) {
      .m <- .evJacModel(2L, 0L, .row, 0L)
      for (.r in list(.ev(24, 5, 50, 1), .ev(24, 6, 0.5, 1), .ev(24, 5, 10, 2), .ev(24, 6, 2, 2))) {
        expect_lt(.evJacErr(.m, .evJacData(1, .r), "cl"), 1e-6,
                  label = paste("row", .row, "evid", .r$evid, "cmt", .r$cmt))
      }
    }
  })

  test_that("a replace or multiply into an oral model's depot or central compartment", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    # depot is cmt 1 and block row 0; central cmt 2 and row 1; ka is column 2
    for (.r in list(.ev(24, 5, 30, 1), .ev(24, 6, 0.5, 1), .ev(24, 5, 50, 2), .ev(24, 6, 0.5, 2))) {
      .d <- .evJacData(1, .r)
      expect_lt(.evJacErr(.evJacModel(1L, 1L, 1L, 0L), .d, "cl"), 1e-6, label = paste(.r$evid, .r$cmt, "cl"))
      expect_lt(.evJacErr(.evJacModel(1L, 1L, 0L, 2L), .d, "ka"), 1e-6, label = paste(.r$evid, .r$cmt, "ka"))
      expect_lt(.evJacErr(.evJacModel(1L, 1L, 1L, 2L), .d, "ka"), 1e-6, label = paste(.r$evid, .r$cmt, "ka central"))
    }
  })

  test_that("an observation at the time of the event, and several events at one time", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    .m <- .evJacModel(1L, 0L, 0L, 0L)
    .cases <- list(
      rbind(.ev(24, 5, 50, 1), .ev(24, 0, 0, 1)),
      rbind(.ev(24, 5, 50, 1), .ev(24, 6, 0.5, 1)),
      rbind(.ev(24, 6, 0.5, 1), .ev(24, 5, 50, 1)),
      rbind(.ev(24, 1, 100, 1), .ev(24, 5, 50, 1)),
      rbind(.ev(24, 5, 50, 1), .ev(24, 1, 100, 1)),
      rbind(.ev(24, 5, 50, 1), .ev(24, 0, 0, 1), .ev(24, 6, 0.5, 1), .ev(24, 0, 0, 1))
    )
    for (.k in seq_along(.cases)) {
      expect_lt(.evJacErr(.m, .evJacData(1, .cases[[.k]]), "cl"), 1e-6, label = .k)
    }
  })

  test_that("a multiply under a modeled f() scales by F*amt", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    .m <- .evJacModel(1L, 0L, 0L, 0L, extra = "f(central) <- 0.8\n")
    expect_lt(.evJacErr(.m, .evJacData(1, .ev(24, 6, 0.5, 1)), "cl"), 1e-6)
    expect_lt(.evJacErr(.m, .evJacData(1, .ev(24, 5, 50, 1)), "cl"), 1e-6)
  })

  test_that("a replace or multiply pushed by the model", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    .d <- .evJacData(1, .ev(24, 0, 0, 1))
    .am0 <- .evJacSolve(.evJacModel(1L, 0L, 0L, 0L), .d, .evJacPar, 1L)
    .am0 <- .am0$am[.am0$time == 30]
    # the amount at 30 h after the event at 24 h: replaced with 50, or halved
    .want <- c(50 * exp(-6 * 2 / 20), 0.5 * .am0)
    .x <- c("replace(50, central)", "multiply(0.5, central)")
    for (.k in 1:2) {
      .m <- .evJacModel(1L, 0L, 0L, 0L, extra = paste0("if (t >= 24 & t < 24.5) ", .x[.k], "\n"))
      .s <- .evJacSolve(.m, .d, .evJacPar, 1L)
      expect_equal(.s$am[.s$time == 30], .want[.k], tolerance = 1e-10, label = .x[.k])
      expect_lt(.evJacErr(.m, .d, "cl"), 1e-6, label = .x[.k])
    }
  })

  test_that("on 1 and 8 cores, subject by subject", {
    skip_if_not(.linCmtEventJacSlot(), "rxode2lincmt without linCmtEventJac()")
    .m <- .evJacModel(2L, 0L, 0L, 0L)
    .d <- .evJacData(1, rbind(.ev(24, 5, 50, 1), .ev(24, 0, 0, 1), .ev(33, 6, 0.5, 1)), nid = 16L)
    .p <- data.frame(id = 1:16, cl = seq(1, 4, length.out = 16), v = 20, q = 1, v2 = 30, ka = 1.2)
    .h <- 1e-5
    .pp <- .p
    .pm <- .p
    .pp$cl <- .p$cl + .h
    .pm$cl <- .p$cl - .h
    for (.cores in c(1L, 8L)) {
      .fd <- (.evJacSolve(.m, .d, .pp, .cores)$am - .evJacSolve(.m, .d, .pm, .cores)$am) / (2 * .h)
      expect_lt(.evJacRel(.evJacSolve(.m, .d, .p, .cores)$jr, .fd), 1e-6, label = .cores)
    }
    expect_identical(.evJacSolve(.m, .d, .p, 1L)$jr, .evJacSolve(.m, .d, .p, 8L)$jr)
  })
})
