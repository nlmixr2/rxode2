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
      "[could not solve: Modeled rate requested in event table, but not in model; use 'rate(cmt) =']: ",
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
    expect_identical(.n, c(plain = 164L, adjoint = 10L))
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
})
