rxTest({
  .tteTwo <- function() {
    ini({
      la <- 0.3
      lb <- 0.05
    })
    model({
      ha <- la
      hb <- lb * (1 + tteCount(a))
      a ~ tte(ha)
      b ~ tte(hb)
    })
  }

  test_that("tte() endpoint whose window opens later and depends on another's events", {
    .e <- et(c(0, 30), cmt = "a") |> et(c(5, 30), cmt = "b") |> et(id = 1:100)
    # rxTteH0(2): the cumulative hazard b's window opened with
    .s <- withr::with_seed(1, as.data.frame(rxSolve(.tteTwo() |> model(h0b <- rxTteH0(2), append = TRUE), .e)))
    .st <- .s[.s$CMT == 4 & .s$time == 5, ]
    .early <- .st$id %in% .s$id[.s$CMT == 3 & .s$sim == 1 & .s$time < 5]
    expect_true(sum(.early) > 50L)
    # opened on the trajectory kept after a's earlier events
    expect_equal(.st$h0b, .st$rx.tteH.b)
    .n <- 0L
    for (.id in unique(.s$id)) {
      .d <- .s[.s$id == .id, ]
      .ta <- .d$time[.d$CMT == 3 & .d$sim == 1]
      .h0 <- .d$rx.tteH.b[.d$CMT == 4 & .d$time == 5][1]
      for (.j in which(.d$CMT == 4 & .d$sim == 1)) {
        .t <- .d$time[.j]
        # lb * integral over (5, t] of 1 + (events of a before s)
        .ref <- 0.05 * ((.t - 5) + sum(.t - pmax(.ta[.ta < .t], 5)))
        expect_equal(.d$rx.tteH.b[.j] - .h0, .ref, tolerance = 1e-5)
        .n <- .n + 1L
      }
    }
    expect_true(.n > 50L)
  })

  test_that("tte() endpoint running out of event slots keeps the others' events", {
    f <- .tteTwo() |> model(hb <- lb) |> ini(lb = 0.02)
    .e <- et(c(0, 30), cmt = "a") |> et(c(0, 30), cmt = "b") |> et(id = 1:40)
    .ref <- withr::with_seed(2, as.data.frame(rxSolve(f, .e)))
    # two slots per endpoint: a runs out, b (rarely more than two events) keeps its events
    .s <- withr::with_options(list(rxode2.tteMaxEvents = 2L), {
      suppressMessages(withr::with_seed(2, as.data.frame(rxSolve(f, .e))))
    })
    expect_true(all(tapply(.s$sim[.s$CMT == 3], .s$id[.s$CMT == 3], sum) == 2))
    .b <- function(d) {
      d <- d[d$CMT == 4, c("id", "time", "sim")]
      rownames(d) <- NULL
      d
    }
    .nb <- tapply(.ref$sim[.ref$CMT == 4], .ref$id[.ref$CMT == 4], sum)
    .ok <- as.numeric(names(.nb)[.nb <= 2])
    expect_true(length(.ok) > 30L)
    expect_equal(.b(.s)[.b(.s)$id %in% .ok, ], .b(.ref)[.b(.ref)$id %in% .ok, ], ignore_attr = TRUE)
  })

  test_that("tte() hazard of covariates given on the endpoint's records", {
    f <- function() {
      ini({
        lambda <- 0.01
      })
      model({
        haz <- lambda * wt / 70
        ev ~ tte(haz, maxEventNumber = 1)
      })
    }
    # wt steps from 70 to 280 at 10
    .d <- data.frame(id = rep(1:400, each = 3), time = rep(c(0, 10, 30), 400), evid = 0,
                     wt = rep(c(70, 280, 280), 400))
    .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), .d)))
    expect_true(all(table(.s$id) == 2L))
    .ev <- .s[.s$sim == 1, ]
    .h <- ifelse(.ev$time < 10, 0.01 * .ev$time, 0.1 + 0.04 * (.ev$time - 10))
    expect_equal(.ev$rx.tteH.ev, .h, tolerance = 1e-6)
    expect_equal(nrow(.ev) / 400, 1 - exp(-0.9), tolerance = 0.1)
    # a constant covariate per subject
    .d2 <- data.frame(id = rep(1:400, each = 2), time = rep(c(0, 30), 400), evid = 0,
                      wt = rep(rep(c(140, 35), each = 200), each = 2))
    .s2 <- withr::with_seed(4, as.data.frame(rxSolve(f(), .d2)))
    .e2 <- .s2[.s2$sim == 1, ]
    expect_equal(.e2$rx.tteH.ev, 0.01 * .e2$wt / 70 * .e2$time, tolerance = 1e-6)
    # linear interpolation from 70 at 0 to 280 at 30, also at the event rows
    .d3 <- data.frame(id = rep(1:400, each = 2), time = rep(c(0, 30), 400), evid = 0,
                      wt = rep(c(70, 280), 400))
    .s3 <- withr::with_seed(5, as.data.frame(rxSolve(f(), .d3, covsInterpolation = "linear")))
    .e3 <- .s3[.s3$sim == 1, ]
    expect_true(nrow(.e3) > 100L)
    expect_equal(.e3$wt, 70 + 7 * .e3$time, tolerance = 1e-8)
    expect_equal(.e3$rx.tteH.ev, 0.01 * (.e3$time + 0.05 * .e3$time^2), tolerance = 1e-6)
  })

  test_that("tte() keeps the NA covariate semantics of the user's own records", {
    f <- function() {
      ini({
        lambda <- 1e-6
      })
      model({
        d/dt(depot) <- -depot
        w <- wt
        haz <- lambda * wt / 70
        ev ~ tte(haz, maxEventNumber = 1)
      })
    }
    g <- function() {
      ini({
        lambda <- 1e-6
      })
      model({
        d/dt(depot) <- -depot * lambda
        w <- wt
      })
    }
    .d <- data.frame(id = 1, time = c(0, 5, 10), evid = c(0, 1, 0), amt = c(0, 10, 0),
                     cmt = c(NA, "depot", NA), wt = c(0, NA, 10))
    for (.m in c("linear", "midpoint", "locf", "nocb")) {
      .a <- withr::with_seed(1, as.data.frame(rxSolve(f(), .d, covsInterpolation = .m,
                                                      addDosing = TRUE)))
      .b <- as.data.frame(rxSolve(g(), .d, covsInterpolation = .m, addDosing = TRUE))
      expect_equal(.a$w[.a$time == 5], .b$w[.b$time == 5], info = .m)
    }
  })

  test_that("tte() interval censoring with a window shorter than one interval", {
    f <- function() {
      ini({
        lambda <- 0.05
      })
      model({
        haz <- lambda
        ev ~ tte(haz, eventType = "intervalCensored", intervalLength = 10, rightCensoringTime = 5)
      })
    }
    .s <- as.data.frame(rxSolve(f(), et(0) |> et(id = 1:3)))
    expect_equal(sort(unique(.s$time)), c(0, 5))
  })
})
