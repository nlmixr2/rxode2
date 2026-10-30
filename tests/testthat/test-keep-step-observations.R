rxTest({
  # Ten oral doses every 12 h, observed once per interval or every hour.  An
  # observation record changes neither the state nor the dosing, so a solver
  # starts the next segment with the step it proposed at the end of the last
  # one: each extra observation record costs about one step, not a restart, and
  # both schedules match the closed form.
  .ksModel <- rxode2({
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - k * center
  })
  .ksPar <- c(ka = 1.2, k = 0.2)
  .ksDose <- (0:9) * 12

  # closed form: 100 into depot at every dose time
  .ksExact <- function(time) {
    .ka <- .ksPar[["ka"]]
    .k <- .ksPar[["k"]]
    .depot <- 0 * time
    .center <- 0 * time
    for (.t in .ksDose) {
      .on <- time >= .t
      .dt <- pmax(time - .t, 0)
      .depot <- .depot + .on * 100 * exp(-.ka * .dt)
      .center <- .center + .on * 100 * .ka / (.ka - .k) * (exp(-.k * .dt) - exp(-.ka * .dt))
    }
    list(depot = .depot, center = .center)
  }

  # the solve and its right-hand-side evaluations
  .ksSolve <- function(method, obs) {
    .ev <- rbind(
      data.frame(id = 1L, time = .ksDose, evid = 1L, amt = 100, cmt = "depot"),
      data.frame(id = 1L, time = obs, evid = 0L, amt = 0, cmt = "center")
    )
    .s <- rxSolve(.ksModel, .ev, params = .ksPar, method = method,
                  atol = 1e-10, rtol = 1e-10, returnType = "rxSolve")
    list(d = as.data.frame(.s), dadt = .s$counts$dadt)
  }

  test_that("dop853, dop5 and ros4 keep the proposed step across observation records", {
    .sparse <- .ksDose + 6
    .hourly <- seq(0.5, 119.5, by = 1)
    for (.method in c("dop853", "dop5", "ros4")) {
      .s <- .ksSolve(.method, .sparse)
      .h <- .ksSolve(.method, .hourly)
      for (.x in list(.s, .h)) {
        .exact <- .ksExact(.x$d$time)
        expect_equal(.x$d$depot, .exact$depot, tolerance = 1e-7, label = .method)
        expect_equal(.x$d$center, .exact$center, tolerance = 1e-7, label = .method)
      }
      # a restart at every observation record costs 26 to 32 evaluations per
      # record here; keeping the step, 6 to 13
      .perRecord <- (.h$dadt - .s$dadt) / (length(.hourly) - length(.sparse))
      expect_lt(.perRecord, 18, label = .method)
    }
  })

  test_that("dop853, dop5 and ros4 restart the step at an mtime() or evid 2 record", {
    # A pulse of area 100 just after t = 5, where there is a record.  The
    # first step after a restart samples the pulse; a step kept from before
    # can step over it unseen, so these records restart the step.
    .mt <- rxode2({
      mtime(tp) <- 5
      if (t > tp && t <= tp + 0.01) {
        r <- 1e4
      } else {
        r <- 0
      }
      d/dt(y) <- r
    })
    .e2 <- rxode2({
      if (t > 5 && t <= 5.01) {
        r <- 1e4
      } else {
        r <- 0
      }
      d/dt(y) <- r
    })
    .obs <- data.frame(id = 1L, time = c(1, 2, 3, 4, 6), evid = 0L, cmt = "y")
    .ev2 <- rbind(.obs, data.frame(id = 1L, time = 5, evid = 2L, cmt = "y"))
    .ev2 <- .ev2[order(.ev2$time), ]
    for (.method in c("dop853", "dop5", "ros4")) {
      for (.case in list(list(m = .mt, ev = .obs, lab = "mtime"),
                         list(m = .e2, ev = .ev2, lab = "evid 2"))) {
        .s <- rxSolve(.case$m, .case$ev, method = .method, atol = 1e-10, rtol = 1e-10,
                      returnType = "data.frame")
        expect_equal(.s$y[.s$time == 6], 100, tolerance = 1e-6,
                     label = paste(.method, .case$lab))
      }
    }
  })
})
