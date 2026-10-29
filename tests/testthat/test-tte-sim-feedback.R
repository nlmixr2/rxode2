rxTest({
  test_that("tte() hazards fed back by tteTime() and tteCount()", {
    f <- function() {
      ini({
        te <- 5
        sh <- 2
      })
      model({
        haz <- sh / te * ((time - tteTime(ev)) / te)^(sh - 1)
        ev ~ tte(haz, maxEventNumber = 2)
      })
    }
    .s <- withr::with_seed(6, as.data.frame(rxSolve(f(), et(c(0, 500)) |> et(id = 1:500))))
    .s <- .s[.s$sim == 1, ]
    .g <- unlist(lapply(split(.s$time, .s$id), diff))
    expect_gt(stats::ks.test(.g, function(x) pweibull(x, 2, 5))$p.value, 0.01)
    g <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda * (1 + tteCount(ev))
        ev ~ tte(haz, maxEventNumber = 3)
      })
    }
    .s <- withr::with_seed(7, as.data.frame(rxSolve(g(), et(c(0, 1000)) |> et(id = 1:500))))
    .s <- .s[.s$sim == 1, ]
    .s$n <- ave(.s$time, .s$id, FUN = seq_along)
    .s$gap <- ave(.s$time, .s$id, FUN = function(t) diff(c(0, t)))
    expect_equal(as.numeric(tapply(.s$gap, .s$n, mean)), 1 / (0.1 * 1:3), tolerance = 0.15)
    expect_error(
      rxode2(function() {
        ini({
          lambda <- 0.1
        })
        model({
          haz <- lambda * tteTime(x)
          ev ~ tte(haz)
        })
      }),
      "does not name a tte"
    )
  })

  .ttePk <- function() {
    ini({
      tcl <- log(1)
      tv <- log(10)
      add.sd <- 0.1
      lambda <- 0.02
      eta.cl ~ 0.1
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      d / dt(central) <- -cl / v * central
      cp <- central / v
      haz <- lambda * (1 + cp)
      cp ~ add(add.sd)
      drop ~ tte(haz, maxEventNumber = 1, dropout = TRUE)
    })
  }

  test_that("tte() next to a PK endpoint, with dropout", {
    .e <- et(amt = 100, cmt = "central") |>
      et(seq(0, 48, by = 4), cmt = "cp") |>
      et(c(0, 48), cmt = "drop") |>
      et(id = 1:20)
    .s <- withr::with_seed(8, as.data.frame(rxSolve(.ttePk(), .e)))
    for (.d in split(.s, .s$id)) {
      .ev <- .d$time[.d$CMT == 4 & .d$sim == 1]
      if (length(.ev) == 1L) {
        # no observation of any endpoint after the dropout
        expect_true(all(.d$time <= .ev))
      } else {
        expect_true(any(.d$CMT == 3 & .d$time == 48))
      }
    }
    .noDrop <- .ttePk() |> model(drop ~ tte(haz, maxEventNumber = 1))
    .s2 <- withr::with_seed(8, as.data.frame(rxSolve(.noDrop, .e)))
    expect_true(all(tapply(.s2$time[.s2$CMT == 3], .s2$id[.s2$CMT == 3], max) == 48))
  })

  test_that("tte() hazard of a delay() state", {
    f <- function() {
      ini({
        k <- 0.3
        lambda <- 0.02
        beta <- 0.05
        tau <- 2
      })
      model({
        d/dt(central) <- -k * delay(central, tau)
        haz <- lambda * exp(beta * central)
        ev ~ tte(haz)
      })
    }
    # the same hazard integrated as an ordinary state on the dense path
    ref <- function() {
      ini({
        k <- 0.3
        lambda <- 0.02
        beta <- 0.05
        tau <- 2
      })
      model({
        d/dt(central) <- -k * delay(central, tau)
        haz <- lambda * exp(beta * central)
        d/dt(H) <- haz
      })
    }
    .e <- et(amt = 100, cmt = "central") |>
      et(seq(0, 48, by = 2), cmt = "central") |>
      et(c(0, 48), cmt = "ev") |>
      et(id = 1:10)
    for (.m in c("dop853+ros4", "ros4")) {
      .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), .e, method = .m)))
      .ev <- .s[.s$sim == 1 & .s$evid == 0, ]
      expect_true(nrow(.ev) > 0, info = .m)
      .g <- as.data.frame(rxSolve(ref(), et(amt = 100, cmt = "central") |>
        et(sort(unique(c(seq(0, 48, by = 2), .ev$time))))))
      expect_equal(.ev$rx.tteH.ev, .g$H[match(.ev$time, .g$time)], tolerance = 1e-4, info = .m)
      .pk <- .s[.s$evid == 2, ]
      expect_equal(.pk$central, .g$central[match(.pk$time, .g$time)], tolerance = 1e-4, info = .m)
    }
  })
})
