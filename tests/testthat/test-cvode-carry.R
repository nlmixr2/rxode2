rxTest({
  # cvode re-initialises only where the state or the right-hand side changes:
  # it carries on across an observation record, and restarts at a dose or where
  # a covariate changes, whether the change is on an evid 2 or an observation
  # record.
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
    expect_lt(.perRecord, 10)
  })

  test_that("cvode re-initialises where a covariate changes on an evid 2 or an observation record", {
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
})
