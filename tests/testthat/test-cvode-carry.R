rxTest({
  # cvode re-initialises only where the state or the right-hand side may
  # change: it carries on across a plain observation record, and restarts at a
  # dose, at an mtime() or evid 2 record, and where a covariate changes.
  .cvModel <- rxode2({
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - kel * center
  })
  .cvDose <- (0:9) * 12

  # exact solution of the linear system with kel piecewise constant: kel[j]
  # from change[j] on; 100 into depot at every dose
  .cvExact <- function(time, ka, kel, change) {
    .A <- function(k) matrix(c(-ka, ka, 0, -k), 2, 2)
    .prop <- function(x, dt, k) {
      .e <- eigen(.A(k))
      drop(Re(.e$vectors %*% diag(exp(.e$values * dt)) %*% solve(.e$vectors)) %*% x)
    }
    .ev <- sort(unique(c(.cvDose, change, time)))
    .x <- c(0, 0)
    .t <- 0
    .out <- matrix(NA_real_, length(time), 2)
    for (.te in .ev) {
      if (.te > .t) {
        .k <- kel[max(which(change <= .t))]
        .x <- .prop(.x, .te - .t, .k)
        .t <- .te
      }
      if (.te %in% .cvDose) .x[1] <- .x[1] + 100
      .w <- which(time == .te)
      if (length(.w)) .out[.w, ] <- matrix(.x, length(.w), 2, byrow = TRUE)
    }
    .out
  }

  .cvSolve <- function(ev, params = NULL) {
    .s <- rxSolve(.cvModel, ev, params = params, method = "cvode",
                  atol = 1e-10, rtol = 1e-10, returnType = "rxSolve")
    list(d = as.data.frame(.s), dadt = .s$counts$dadt)
  }
  .cvEvents <- function(obs, kelCol = NULL) {
    .ev <- rbind(
      data.frame(id = 1L, time = .cvDose, evid = 1L, amt = 100, cmt = "depot"),
      data.frame(id = 1L, time = obs, evid = 0L, amt = 0, cmt = "center")
    )
    .ev[order(.ev$time, -.ev$evid), ]
  }

  test_that("cvode carries on across observation records", {
    .sparse <- .cvSolve(.cvEvents(.cvDose + 6), c(ka = 1.2, kel = 0.2))
    .hourly <- .cvSolve(.cvEvents(seq(0.5, 119.5, by = 1)), c(ka = 1.2, kel = 0.2))
    for (.x in list(.sparse, .hourly)) {
      .exact <- .cvExact(.x$d$time, 1.2, 0.2, 0)
      expect_equal(.x$d$depot, .exact[, 1], tolerance = 1e-7)
      expect_equal(.x$d$center, .exact[, 2], tolerance = 1e-7)
    }
    # re-initialising at every observation record costs about 50 evaluations
    # per record here
    .perRecord <- (.hourly$dadt - .sparse$dadt) / (120 - 10)
    expect_lt(.perRecord, 3)
  })

  test_that("cvode matches the exact solution across a covariate step on an evid 2 or an observation record", {
    # an accuracy check: covariates are looked up by time, so this holds with
    # or without the re-initialisation at the step
    .obs <- seq(0.5, 119.5, by = 1)
    for (.evid in c(2L, 0L)) {
      .ev <- .cvEvents(.obs)
      # kel steps from 0.2 to 0.5 at t = 30 (an evid 2 record, or the
      # observation there)
      .ev <- rbind(.ev, data.frame(id = 1L, time = 30, evid = .evid, amt = 0, cmt = "center"))
      .ev <- .ev[order(.ev$time, -.ev$evid), ]
      .ev$kel <- ifelse(.ev$time < 30, 0.2, 0.5)
      .ev$ka <- 1.2
      .r <- .cvSolve(.ev)
      .r$d <- .r$d[.r$d$time %in% .obs, ]
      .exact <- .cvExact(.r$d$time, 1.2, c(0.2, 0.5), c(0, 30))
      expect_equal(.r$d$depot, .exact[, 1], tolerance = 1e-7, label = paste("evid", .evid))
      expect_equal(.r$d$center, .exact[, 2], tolerance = 1e-7, label = paste("evid", .evid))
    }
  })

  test_that("cvode re-initialises at an mtime() or evid 2 record", {
    # A pulse of area 100 just after t = 5, where there is a record.  A
    # re-initialised step samples the pulse; one carried on from before can
    # step over it unseen, so these records re-initialise.
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
    for (.case in list(list(m = .mt, ev = .obs, lab = "mtime"),
                       list(m = .e2, ev = .ev2, lab = "evid 2"))) {
      .s <- rxSolve(.case$m, .case$ev, method = "cvode", atol = 1e-10, rtol = 1e-10,
                    returnType = "data.frame")
      expect_equal(.s$y[.s$time == 6], 100, tolerance = 1e-6, label = .case$lab)
    }
  })

  test_that("cvode carries on between resets, replacements and multiplications as lsoda solves them", {
    .ev <- rbind(
      data.frame(id = 1L, time = .cvDose, evid = 1L, amt = 100, cmt = "depot"),
      data.frame(id = 1L, time = seq(0.5, 119.5, by = 1), evid = 0L, amt = 0, cmt = "center"),
      data.frame(id = 1L, time = c(30.25, 50.25, 70.25), evid = c(3L, 5L, 6L),
                 amt = c(0, 40, 0.5), cmt = c("depot", "center", "depot"))
    )
    .ev <- .ev[order(.ev$time), ]
    .solve <- function(method) {
      .s <- rxSolve(.cvModel, .ev, params = c(ka = 1.2, kel = 0.2), method = method,
                    atol = 1e-10, rtol = 1e-10, returnType = "data.frame")
      .s[.s$time %in% seq(0.5, 119.5, by = 1), ]
    }
    .c <- .solve("cvode")
    .l <- .solve("lsoda")
    expect_equal(.c$depot, .l$depot, tolerance = 1e-6)
    expect_equal(.c$center, .l$center, tolerance = 1e-6)
  })

  test_that("cvode reaches each record when extra doses fall between records", {
    # a steady-state bolus whose alag() is longer than ii leaves several
    # pending extra doses between sparse observations; cvode must still return
    # the state at each observation, as lsoda does
    .ssModel <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(c) <- ka * depot - k * c
      alag(depot) <- lag
    })
    .ev <- et(amt = 100, ss = 1, ii = 12) |> et(c(0.5, 30, 60))
    .solve <- function(method) {
      rxSolve(.ssModel, .ev, params = c(k = 0.05, ka = 0.7, lag = 30), method = method,
              atol = 1e-10, rtol = 1e-10, returnType = "data.frame")$c
    }
    expect_equal(.solve("cvode"), .solve("lsoda"), tolerance = 1e-6)
  })
})
