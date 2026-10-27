rxTest({
  # A subject that fails leaves every other subject as the same method solves it
  # without that subject.  A modeled rate (rate = -1) in a model with no rate()
  # fails subject 1 in every driver; a method that needs the adjoint expansion
  # gets the expanded model, so its sensitivities are compared too.
  .fsText <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\ncp=center/v"
  .fsPar <- c(ka = 1.2, cl = 3.5, v = 25)
  .fsData <- data.frame(
    ID = rep(1:3, each = 5),
    TIME = rep(c(0, 0.5, 1, 2, 4), 3),
    AMT = rep(c(100, 0, 0, 0, 0), 3),
    EVID = rep(c(1, 0, 0, 0, 0), 3),
    CMT = "depot",
    RATE = 0
  )
  # the solve as a data frame, its warnings and its printed lines, or the error
  # message
  .fsSolve <- function(m, d, method, cores) {
    tryCatch(
      {
        .w <- capture_warnings(
          .o <- capture.output(.s <- rxSolve(m, d, params = .fsPar, method = method, cores = cores))
        )
        list(d = as.data.frame(.s), w = .w, o = .o)
      },
      error = conditionMessage
    )
  }
  .fsReason <- function(ids) {
    paste0(
      "[solve error: Modeled rate requested in event table, but not in model; use 'rate(cmt) =']: ",
      length(ids),
      " warning(s) for subject(s): ",
      paste(ids, collapse = ", ")
    )
  }

  test_that("a subject that fails leaves the other subjects as they solve without it, for every method", {
    .mPlain <- rxode2(.fsText)
    .mAdj <- rxode2(rxode2::.rxAdjointExpand(.fsText, names(.fsPar))$text)
    .bad <- .fsData
    .bad$RATE[.bad$ID == 1 & .bad$EVID == 1] <- -1
    .msg <- "some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"
    .n <- c(plain = 0L, adjoint = 0L)
    for (.meth in eval(formals(rxode2::rxSolve)$method)) {
      # vv refuses a model whose states are not coordinate-momentum pairs (#1469)
      if (.meth == "vv") {
        next
      }
      .m <- .mPlain
      .ref <- .fsSolve(.m, .fsData[.fsData$ID != 1, ], .meth, 1L)
      if (is.character(.ref) && grepl("adjoint expansion", .ref, fixed = TRUE)) {
        .m <- .mAdj
        .ref <- .fsSolve(.m, .fsData[.fsData$ID != 1, ], .meth, 1L)
      }
      expect_true(is.list(.ref), label = .meth)
      if (!is.list(.ref)) {
        next
      }
      .cols <- setdiff(names(.ref$d), c("id", "time"))
      for (.cores in c(1L, 2L)) {
        .lab <- paste(.meth, "cores", .cores)
        .s <- .fsSolve(.m, .bad, .meth, .cores)
        expect_true(is.list(.s), label = .lab)
        if (!is.list(.s)) {
          next
        }
        expect_identical(.s$w, .msg, label = .lab)
        # the failing subject's reason, once, whichever thread solved it
        expect_identical(.s$o, .fsReason(1), label = .lab)
        # every observation row is there; the failing subject's states are NA
        # (a column of parameters alone is not), the others' exactly the solve
        # without it
        expect_identical(nrow(.s$d), 12L, label = .lab)
        .one <- .s$d[.s$d$id == 1, c("depot", "center")]
        expect_true(all(is.na(as.matrix(.one))), label = .lab)
        .rest <- .s$d[.s$d$id != 1, .cols]
        rownames(.rest) <- NULL
        expect_equal(.rest, .ref$d[, .cols], tolerance = 1e-12, label = .lab)
      }
      .kind <- if (identical(.m, .mAdj)) "adjoint" else "plain"
      .n[[.kind]] <- .n[[.kind]] + 1L
    }
    # a method added to rxSolve() needs these counts raised
    expect_identical(.n, c(plain = 163L, adjoint = 10L))
  })

  test_that("each failed subject's reason names it, whichever thread solved it", {
    .d <- .fsData[rep(seq_len(5), 4), ]
    .d$ID <- rep(c(101, 102, 103, 104), each = 5)
    .d$RATE[.d$ID %in% c(102, 104) & .d$EVID == 1] <- -1
    .mOde <- rxode2(.fsText)
    .mLin <- rxode2("cp <- linCmt(ka, cl, v)")
    for (.meth in c("dop853", "liblsoda", "lsoda", "rk4", "linCmt")) {
      .m <- if (.meth == "linCmt") .mLin else .mOde
      for (.cores in c(1L, 2L)) {
        .s <- .fsSolve(.m, .d, if (.meth == "linCmt") "liblsoda" else .meth, .cores)
        expect_identical(.s$o, .fsReason(c(102, 104)), label = paste(.meth, "cores", .cores))
      }
    }
  })

  test_that("a solver's failure code gives its own text", {
    # lsoda's istate -1 is its first message; a step limit only subject 103
    # reaches gives it
    .m <- rxode2("d/dt(y) <- -k * y\ny(0) <- 1")
    .d <- data.frame(ID = rep(c(101, 102, 103), each = 4), TIME = rep(c(0, 1, 5, 10), 3), EVID = 0, CMT = "y")
    .p <- data.frame(ID = c(101, 102, 103), k = c(1, 2, 1e5))
    for (.cores in c(1L, 2L)) {
      .o <- capture.output(suppressWarnings(rxSolve(
        .m,
        .d,
        params = .p,
        method = "lsoda",
        maxsteps = 40,
        cores = .cores
      )))
      expect_identical(
        grep("^\\[IDID", .o, value = TRUE),
        "[IDID=-1, excess work done on this call (perhaps wrong jt).]: 1 warning(s) for subject(s): 103",
        label = paste("cores", .cores)
      )
    }
  })

  test_that("an adjoint method's backward sweep runs for the subjects after a failing one", {
    # liblsodaadj and abs also solve the plain model; with the expanded one they
    # run the backward sweep that fills the sensitivities
    .mAdj <- rxode2(rxode2::.rxAdjointExpand(.fsText, names(.fsPar))$text)
    .bad <- .fsData
    .bad$RATE[.bad$ID == 1 & .bad$EVID == 1] <- -1
    for (.meth in c("liblsodaadj", "abs", "cvodesadj")) {
      .ref <- .fsSolve(.mAdj, .fsData[.fsData$ID != 1, ], .meth, 1L)$d
      .sens <- grep("^rx__sens_", names(.ref), value = TRUE)
      expect_length(.sens, 6L)
      # center depends on every parameter, so its sensitivities are not zero
      .center <- grep("^rx__sens_center_", .sens, value = TRUE)
      expect_length(.center, 3L)
      expect_true(all(colSums(abs(as.matrix(.ref[, .center]))) > 0), label = .meth)
      .s <- .fsSolve(.mAdj, .bad, .meth, 1L)$d
      expect_true(all(is.na(as.matrix(.s[.s$id == 1, c("depot", "center", .sens)]))), label = .meth)
      .rest <- .s[.s$id != 1, .sens]
      rownames(.rest) <- NULL
      expect_equal(.rest, .ref[, .sens], tolerance = 1e-12, label = .meth)
    }
  })

  # the solve-error test hooks: _rxTestSolveErrPush(err) resets the aggregator,
  # pushes the reasons err's bits stand for (no subject) and flushes;
  # _rxTestSolveWarnFlush() flushes, as an ind_solve() caller does
  .errPush <- function(err) {
    capture.output(invisible(.Call("_rxTestSolveErrPush", as.integer(err), PACKAGE = "rxode2")))
  }
  .flush <- function() capture.output(invisible(.Call("_rxTestSolveWarnFlush", PACKAGE = "rxode2")))
  .cap <- function(expr) capture.output(invisible(expr))
  .solverFail <- "the ODE solver failed (an exception, or a step it could not take)"
  .rateReason <- "[solve error: Modeled rate requested in event table, but not in model; use 'rate(cmt) =']: "

  test_that("every error bit has its own reason text", {
    # bit k is 2^k, as in inst/include/rxode2dataErr.h
    .texts <- c(
      "Corrupted event table during sort (1)",
      "Rate is zero/negative",
      "Modeled rate requested in event table, but not in model; use 'rate(cmt) ='",
      "Corrupted event table during sort (2)",
      "Duration is zero/negative",
      "Modeled duration requested in event table, but not in model; use 'dur(cmt) ='",
      "Data error 686: end of a modeled duration ('rate=-2') infusion is not preceded by its start",
      "Data Error -6: end of a modeled duration ('rate=-2') infusion is the first record",
      "Data Error 8: a modeled duration ('rate=-2') infusion starts on the last record, so it has no end",
      "Data error 886: start of a modeled duration ('rate=-2') infusion is not followed by its end",
      "Data error 797: end of a modeled rate ('rate=-1') infusion is not preceded by its start",
      "Data Error -7: end of a modeled rate ('rate=-1') infusion is the first record",
      "Data Error 9: a modeled rate ('rate=-1') infusion starts on the last record, so it has no end",
      "Data error 997: start of a modeled rate ('rate=-1') infusion is not followed by its end",
      "Corrupted event table during sort (3)",
      "Corrupted event table",
      "Supplied an invalid EVID",
      "Corrupted events",
      "Corrupted event table (during sync)",
      "Corrupted event table (end of sync)",
      "SS=2 & Modeled F does not work",
      "SS=2 & Modeled F does not work",
      "SS=2 & Modeled F does not work",
      "Rate is zero/negative",
      "Inductive linearization could not reach 'atol'/'rtol' within 'maxsteps' relinearization steps; loosen 'atol'/'rtol' or raise 'maxsteps'",
      "Unsupported inductive linearization code; the model's 'indLin' descriptor does not match the solver",
      "Inductive linearization failed with an unhandled exception (most likely out of memory)",
      "the ODE solver failed (an exception, or a step it could not take)",
      "method='vv' needs coordinate-momentum state pairs with d/dt(q_i) = p_i"
    )
    # every rxErr bit in the installed header has a text: the NA-time codes
    # (rxErrNaTime*, 1-5) are not bits
    .hdr <- system.file("include", "rxode2dataErr.h", package = "rxode2")
    skip_if(!nzchar(.hdr), "rxode2dataErr.h not installed")
    .l <- readLines(.hdr, warn = FALSE)
    .def <- regmatches(.l, regexec("^#define +(rxErr[A-Za-z0-9]+) +([0-9]+)", .l))
    .def <- .def[lengths(.def) == 3L]
    .bits <- setNames(as.numeric(vapply(.def, `[`, "", 3L)), vapply(.def, `[`, "", 2L))
    .bits <- sort(.bits[!grepl("^rxErrNaTime", names(.bits))])
    expect_identical(unname(.bits), 2^(seq_along(.texts) - 1))
    for (.k in seq_along(.bits)) {
      .line <- .errPush(.bits[[.k]])
      expect_false(grepl("^\\[solve error: code [0-9]+\\]", .line), label = names(.bits)[.k])
      expect_identical(.line, paste0("[solve error: ", .texts[.k], "]: 1 warning(s)"), label = names(.bits)[.k])
    }
    # a bit with no text is pushed as its code
    expect_identical(.errPush(bitwShiftL(1L, 29L)), "[solve error: code 536870912]: 1 warning(s)")
    # bits that share a text push it once
    expect_identical(
      .errPush(bitwShiftL(1L, 20L) + bitwShiftL(1L, 21L) + bitwShiftL(1L, 22L)),
      "[solve error: SS=2 & Modeled F does not work]: 1 warning(s)"
    )
    expect_identical(.errPush(2L + bitwShiftL(1L, 23L)), "[solve error: Rate is zero/negative]: 1 warning(s)")
  })

  test_that("no solver failure sets an event-table bit", {
    # err = 1 is "Corrupted event table during sort (1)"; a driver's own failure
    # is rxSolverFail(), which sets rxErrSolverFail
    .src <- testthat::test_path("..", "..", "src")
    skip_if(!dir.exists(.src), "source tree not available (installed package)")
    .f <- c(
      list.files(.src, pattern = "\\.(c|cpp|h|hpp)$", full.names = TRUE, recursive = TRUE),
      list.files(
        testthat::test_path("..", "..", "inst", "include"),
        pattern = "\\.(h|hpp)$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    expect_true(all(c("odeinter.h", "implicit_euler_rxode2.hpp") %in% basename(.f)))
    expect_true(any(grepl("/ode/", .f, fixed = TRUE)))
    # = 1, |= 1, += 1, = 1u, = (1) or = rxErrCorruptETSort on an err field
    .gate <- "(->|\\.)err *(\\|=|\\+=|=) *(1u?\\b|\\(1\\))|(->|\\.)err *= *rxErrCorruptETSort\\b"
    .hit <- vapply(
      .f,
      function(f) any(grepl(.gate, readLines(f, warn = FALSE), perl = TRUE)),
      logical(1)
    )
    expect_identical(basename(.f[.hit]), character(0))
  })

  test_that("a solver's own failure says so, for the bridges and the odeint checker", {
    .m <- rxode2("y(0) <- 1\nd/dt(y) <- -k * y")
    .d <- data.frame(ID = rep(101:104, each = 3), TIME = rep(c(0, 1, 2), 4), EVID = 0, CMT = "y")
    # an infinite rate: the bridges' non-finite right-hand side
    .p <- data.frame(ID = 101:104, k = c(1, 1, Inf, 1))
    for (.meth in c("f45", "dop54", "cvode")) {
      .o <- .cap(suppressWarnings(rxSolve(.m, .d, params = .p, method = .meth)))
      expect_identical(.o, paste0("[solve error: ", .solverFail, "]: 1 warning(s) for subject(s): 103"), label = .meth)
    }
    # a step limit every subject reaches: odeinter.h's checker (rk4, ab), and
    # cvode's negative return, which has no IDID text
    for (.meth in c("rk4", "ab", "cvode")) {
      .o <- .cap(suppressWarnings(rxSolve(.m, .d, params = c(k = 1), method = .meth, maxsteps = 3L)))
      expect_identical(
        .o,
        paste0("[solve error: ", .solverFail, "]: 4 warning(s) for subject(s): 101, 102, 103, 104"),
        label = .meth
      )
    }
    # liblsoda gives its istate text, as lsoda does
    .p <- data.frame(ID = 101:104, k = c(1e5, 1, 1, 1))
    .o <- .cap(suppressWarnings(rxSolve(.m, .d, params = .p, method = "liblsoda", maxsteps = 40L)))
    expect_identical(
      .o,
      "[IDID=-1, excess work done on this call (perhaps wrong jt).]: 1 warning(s) for subject(s): 101"
    )
    # dop853 reports its step limit itself, with the x it stopped at: one line,
    # and no IDID line besides
    .o <- .cap(suppressWarnings(rxSolve(.m, .d, params = c(k = 1), method = "dop853", maxsteps = 1L)))
    expect_length(.o, 1L)
    expect_match(
      .o,
      "^\\[exit of dop853 at x = [-0-9.e+]+ \\+/- [-0-9.e+]+ \\([-0-9.e+]+/[-0-9.e+]+\\), more than nmax = 1 are needed\\]: 4 warning\\(s\\) for subject\\(s\\): 101, 102, 103, 104$"
    )
  })

  test_that("reasons name the simulation, and a later clean solve prints none", {
    .m <- rxode2(paste0("cl <- exp(tcl + eta.cl)\n", .fsText))
    .d <- .fsData[rep(seq_len(5), 4), ]
    .d$ID <- rep(c(101, 102, 103, 104), each = 5)
    .d$RATE[.d$ID %in% c(102, 104) & .d$EVID == 1] <- -1
    .par <- c(ka = 1.2, v = 25, tcl = log(3.5))
    set.seed(1)
    .o <- .cap(suppressWarnings(rxSolve(
      .m,
      .d,
      params = .par,
      omega = lotri::lotri(eta.cl ~ 0.1),
      nStud = 2
    )))
    expect_identical(.o, paste0(.rateReason, "4 warning(s) for subject(s): 102, 104, 102 (sim 2), 104 (sim 2)"))
    .o <- .cap(suppressWarnings(rxSolve(.m, .fsData, params = c(.par, eta.cl = 0))))
    expect_identical(.o, character(0))
  })

  test_that("ind_solve() pushes a failed subject's reasons for its caller to flush", {
    .m <- rxode2(.fsText)
    .d <- .fsData[rep(seq_len(5), 4), ]
    .d$ID <- rep(c(101, 102, 103, 104), each = 5)
    .d$RATE[.d$ID %in% c(102, 104) & .d$EVID == 1] <- -1
    .drive <- function() capture.output(invisible(.Call("_rxode2_rxIndLinDriveTeam", 2L, FALSE, PACKAGE = "rxode2")))
    expect_identical(.fsSolve(.m, .d, "liblsoda", 2L)$o, paste0(.rateReason, "2 warning(s) for subject(s): 102, 104"))
    # the drive solves every subject through ind_solve() and prints nothing ...
    expect_identical(.drive(), character(0))
    # ... until its caller flushes
    expect_identical(.flush(), paste0(.rateReason, "2 warning(s) for subject(s): 102, 104"))
    # an rxSolve() starts from an empty summary: an unflushed drive's lines are
    # not this solve's
    expect_identical(.drive(), character(0))
    expect_identical(.fsSolve(.m, .fsData[.fsData$ID != 1, ], "liblsoda", 1L)$o, character(0))
  })

  test_that("par_solve() keeps the lines pushed before it, as SAEM needs", {
    # SAEM calls par_solve() between its subjects' ind_solve() calls and
    # flushes after; a reset inside par_solve() would drop what they pushed
    .m <- rxode2(.fsText)
    expect_identical(.fsSolve(.m, .fsData, "liblsoda", 1L)$o, character(0))
    expect_identical(
      .cap(.Call("_rxTestParSolveAfterPush", PACKAGE = "rxode2")),
      "[rxTest -- pushed before par_solve()]: 1 warning(s)"
    )
  })
})
