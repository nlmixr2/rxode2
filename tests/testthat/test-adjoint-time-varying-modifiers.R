rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # f(), alag(), rate() and dur() that depend on a time-varying covariate take
  # its value at each event (the record's own, as the forward solve reads it),
  # not at the window start: WT steps from 70 to 100 at t = 2 and to 50 at t = 5.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .d <- function(rows, ...) {
    .ev <- do.call(rbind, lapply(rows, function(r) {
      data.frame(id = 1, time = r[[1]], evid = r[[2]], amt = r[[3]], cmt = r[[4]])
    }))
    .obs <- data.frame(id = 1, time = c(1, 2, 3, 4.5, 6, 8), evid = 0, amt = 0, cmt = "center")
    .ev <- rbind(.ev, .obs)
    .ev <- .ev[order(.ev$time, -.ev$evid), ]
    .ev$WT <- ifelse(.ev$time < 2, 70, ifelse(.ev$time < 5, 100, 50))
    .extra <- list(...)
    for (.n in names(.extra)) .ev[[.n]] <- ifelse(.ev$evid == 1 & .ev$time == 3, .extra[[.n]], 0)
    .ev
  }
  .doses <- list(list(0, 1, 100, "depot"), list(3, 1, 80, "depot"), list(5.5, 1, 60, "depot"))
  .twoDoses <- list(list(0, 1, 100, "depot"), list(3, 1, 80, "depot"))
  .p0 <- c(ka = 1.2, k = 0.2)
  .scen <- list(
    bolusF = list(txt = paste0(.base, "\nf(depot) <- bio*WT/70"), p = c(.p0, bio = 0.8), ev = .d(.doses)),
    multiplyF = list(txt = paste0(.base, "\nf(center) <- bio*WT/70"), p = c(.p0, bio = 0.8),
      ev = .d(list(list(0, 1, 100, "depot"), list(3, 6, 0.5, "center")))),
    replaceF = list(txt = paste0(.base, "\nf(center) <- bio*WT/70"), p = c(.p0, bio = 0.8),
      ev = .d(list(list(0, 1, 100, "depot"), list(3, 5, 30, "center")))),
    # the last dose's lag is scheduled at its record (WT = 50) and lands past it
    bolusLag = list(txt = paste0(.base, "\nalag(depot) <- lg*WT/70"), p = c(.p0, lg = 0.3), ev = .d(.doses)),
    infusionLag = list(txt = paste0(.base, "\nalag(depot) <- lg*WT/70"), p = c(.p0, lg = 0.3),
      ev = .d(.twoDoses, rate = 40)),
    modeledRate = list(txt = paste0(.base, "\nrate(depot) <- rr*WT/70"), p = c(.p0, rr = 30),
      ev = .d(.twoDoses, rate = -1)),
    durF = list(txt = paste0(.base, "\nf(depot) <- bio*WT/70"), p = c(.p0, bio = 0.8),
      ev = .d(.twoDoses, dur = 2.5)),
    resetIc = list(txt = paste0(.base, "\ncenter(0) <- c0*WT/70"), p = c(.p0, c0 = 5),
      ev = .d(list(list(0, 1, 100, "depot"), list(3, 3, 0, "depot"), list(3, 1, 80, "depot"))))
  )
  .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")
  .chk <- function(k, meths, label) {
    .ref <- .adjFd(k$txt, k$ev, k$p, method = "liblsoda", maxsteps = 1e6)
    for (.m in meths) {
      .a <- rxSolveAdjointRk4(k$txt, k$ev,
        params = k$p, calcSens = names(k$p), method = .m,
        atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
      )
      expect_equal(nrow(.a), length(.ref[[1]]), label = paste(label, .m, "rows"))
      # a lagged dose landing between abs's fixed steps, and cvodesadj's own
      # tolerances, limit those two drivers to about 1e-5
      .tol <- if (.m %in% c("abs", "cvodesadj")) 2e-5 else 1e-6
      for (.cn in names(.ref)) {
        expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), .tol, label = paste(label, .m, .cn))
      }
    }
  }

  test_that("modifiers with a time-varying covariate are read at each event in every driver", {
    for (.nm in names(.scen)) .chk(.scen[[.nm]], .meths, .nm)
  })

  test_that("an interior ss = 1 dose's f() reads its record's covariate", {
    .ev <- .d(.twoDoses, ss = 1, ii = 12)
    # cvodesadj and abs do not carry ss
    .chk(list(txt = paste0(.base, "\nf(depot) <- bio*WT/70"), p = c(.p0, bio = 0.8), ev = .ev),
      c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj"), "ssF")
  })
})
