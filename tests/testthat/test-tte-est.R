rxTest({
  # estimation-style model: every endpoint's likelihood lines, with the
  # likelihood kept as an output column
  .tteEstModel <- function(ui) {
    if (is.function(ui)) ui <- ui()
    .p <- ui$predDf
    .err <- lapply(seq_len(nrow(.p)), function(i) {
      .handleSingleErrTypeNormOrTFoceiBase(ui, .p[i, ], i)
    })
    .l <- rxCombineErrorLines(ui, errLines = .err, cmtLines = TRUE, dvidLine = FALSE)
    .txt <- vapply(as.list(.l[[2]])[-1], deparse1, character(1))
    .txt <- gsub("(rx_[A-Za-z_]+) ~ ", "\\1 <- ", .txt)
    rxode2(paste(.txt, collapse = "\n"))
  }
  .tteLl <- function(f, d, p, cmt = NULL) {
    # pruned like the estimation models (endpoint branches become products)
    .s <- rxSolve(rxode2(rxPrune(.tteEstModel(f))), d, params = p, returnType = "data.frame",
                  atol = 1e-12, rtol = 1e-12, addDosing = FALSE)
    if (!is.null(cmt)) .s <- .s[.s$CMT == cmt, ]
    sum(.s$rx_pred_)
  }
  .wH <- function(t, lam, k) (lam * t)^k
  .wh <- function(t, lam, k) k * lam * (lam * t)^(k - 1)

  test_that("tte() estimation lines pack the tte distribution", {
    f <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda
        ev ~ tte(haz)
      })
    }
    .ui <- f()
    .l <- .handleSingleErrTypeNormOrTFoceiBase(.ui, .ui$predDf[1, ], 1L)
    .yj <- vapply(.l, function(x) identical(x[[2]], quote(rx_yj_)), logical(1))
    expect_equal(eval(.l[[which(.yj)]][[3]]) %/% 10 + 1, 20)
  })

  test_that("tte() exact-event log-likelihood", {
    f <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda
        ev ~ tte(haz)
      })
    }
    .d <- data.frame(id = 1, time = c(0, 3, 7, 10), dv = c(0, 1, 1, 0), evid = 0)
    expect_equal(.tteLl(f, .d, c(lambda = 0.1)), 2 * log(0.1) - 0.1 * 10, tolerance = 1e-8)
    fw <- function() {
      ini({
        lam <- 0.1
        k <- 1.5
      })
      model({
        haz <- k * lam * (lam * time)^(k - 1)
        ev ~ tte(haz)
      })
    }
    # a start record at 2 (late entry): the hazard before it does not count
    .d <- data.frame(id = 1, time = c(0, 2, 3, 7, 10), dv = c(NA, 0, 1, 1, 0),
                     evid = c(2, 0, 0, 0, 0))
    expect_equal(
      .tteLl(fw, .d, c(lam = 0.1, k = 1.5)),
      log(.wh(3, 0.1, 1.5)) + log(.wh(7, 0.1, 1.5)) - (.wH(10, 0.1, 1.5) - .wH(2, 0.1, 1.5)),
      tolerance = 1e-6
    )
  })

  test_that("tte(origin = TRUE) counts the hazard from the subject's start", {
    f <- function() {
      ini({
        lam <- 0.1
        k <- 1.5
      })
      model({
        haz <- k * lam * (lam * time)^(k - 1)
        ev ~ tte(haz, origin = TRUE)
      })
    }
    .d <- data.frame(id = 1, time = c(0, 3, 7, 10), dv = c(NA, 1, 1, 0), evid = c(2, 0, 0, 0))
    expect_equal(
      .tteLl(f, .d, c(lam = 0.1, k = 1.5)),
      log(.wh(3, 0.1, 1.5)) + log(.wh(7, 0.1, 1.5)) - .wH(10, 0.1, 1.5),
      tolerance = 1e-6
    )
  })

  test_that("tte() interval-censored log-likelihood", {
    f <- function() {
      ini({
        lam <- 0.1
      })
      model({
        haz <- lam
        ev ~ tte(haz, eventType = "intervalCensored")
      })
    }
    .d <- data.frame(id = 1, time = c(0, 5, 10, 15), dv = c(0, 0, 1, 0), evid = 0)
    expect_equal(.tteLl(f, .d, c(lam = 0.1)), -0.5 + log(1 - exp(-0.5)) - 0.5, tolerance = 1e-8)
  })

  test_that("tteTime()/tteCount() come from the data in an estimation solve", {
    fr <- function() {
      ini({
        lam <- 0.1
        k <- 2
      })
      model({
        tl <- time - tteTime(ev)
        haz <- k * lam * (lam * tl)^(k - 1)
        ev ~ tte(haz)
      })
    }
    .d <- data.frame(id = 1, time = c(0, 3, 7, 10), dv = c(0, 1, 1, 0), evid = 0)
    expect_equal(
      .tteLl(fr, .d, c(lam = 0.1, k = 2)),
      log(.wh(3, 0.1, 2)) + log(.wh(4, 0.1, 2)) - (.wH(3, 0.1, 2) + .wH(4, 0.1, 2) + .wH(3, 0.1, 2)),
      tolerance = 1e-6
    )
    fc <- function() {
      ini({
        lam <- 0.1
      })
      model({
        haz <- lam * (1 + tteCount(ev))
        ev ~ tte(haz)
      })
    }
    expect_equal(
      .tteLl(fc, .d, c(lam = 0.1)),
      log(0.1) + log(0.2) - (0.1 * 3 + 0.2 * 4 + 0.3 * 3),
      tolerance = 1e-6
    )
  })

  test_that("tte() log-likelihood beside another endpoint", {
    f <- function() {
      ini({
        ke <- 0.2
        lam <- 0.1
        add.sd <- 0.1
      })
      model({
        d / dt(central) <- -ke * central
        cp <- central
        haz <- lam * (1 + tteCount(ev))
        cp ~ add(add.sd)
        ev ~ tte(haz)
      })
    }
    .ui <- f()
    .cmt <- .ui$predDf$cmt[.ui$predDf$cond == "ev"]
    .cp <- .ui$predDf$cmt[.ui$predDf$cond == "cp"]
    .d <- data.frame(
      id = 1,
      time = c(0, 0, 1, 3, 5, 7, 10),
      amt = c(10, 0, 0, 0, 0, 0, 0),
      evid = c(1, 0, 0, 0, 0, 0, 0),
      cmt = c(1, .cmt, .cp, .cmt, .cp, .cmt, .cmt),
      dv = c(NA, 0, 8, 1, 4, 1, 0)
    )
    expect_equal(
      .tteLl(f, .d, c(ke = 0.2, lam = 0.1, add.sd = 0.1), cmt = .cmt),
      log(0.1) + log(0.2) - (0.1 * 3 + 0.2 * 4 + 0.3 * 3),
      tolerance = 1e-6
    )
    # the other endpoint's DV stays out of the tte log-likelihood
    .s <- rxSolve(rxode2(rxPrune(.tteEstModel(f))), .d, params = c(ke = 0.2, lam = 0.1, add.sd = 0.1),
                  returnType = "data.frame", addDosing = FALSE)
    expect_true(all(is.finite(.s$rx_pred_)))
  })

  test_that("tte(origin = TRUE) simulation has no start row", {
    f <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda
        ev ~ tte(haz, maxEventNumber = 1, origin = TRUE)
      })
    }
    .e <- et(c(2, 30)) |> et(id = 1:500)
    .s <- withr::with_seed(7, as.data.frame(rxSolve(f(), .e)))
    expect_true(all(table(.s$id) == 1L))
    expect_false(any(.s$time == 2 & .s$sim == 0))
    # observed from the subject's start (time 2): P(event by 30) = 1 - exp(-2.8)
    expect_equal(mean(.s$sim), 1 - exp(-2.8), tolerance = 0.05)
  })
})
