rxTest({
  # handleSS() reads a modeled duration's stop by dose index; records before the
  # dose (observations, a zero-amount replace) no longer make it read another
  # record (#1602).
  .o <- c(4, 9, 15, 20)
  .off <- 12 * 300
  .p <- c(ka = 1.2, k = 0.2)
  .mod <- function(durv, lag) {
    rxode2(paste0(
      "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\n",
      "f(depot) <- 0.8\ndur(depot) <- ", durv, "\n", lag
    ))
  }
  for (.durv in c(2.2, 0)) {
    test_that(paste0("a modeled dur() = ", .durv, " ss dose after other records matches the explicit regimen"), {
      .want <- et(time = 3, amt = 50, rate = -2, ii = 12, addl = 300, cmt = "depot") |>
        et(.o + .off)
      for (.lag in c("", "alag(depot) <- 0\n", "alag(depot) <- 0.7\n")) {
        .m <- .mod(.durv, .lag)
        .w <- rxSolve(.m, .want, .p)$center
        for (.ss in 1:2) {
          .ev <- et(c(0, 0.5)) |>
            et(time = 1, amt = 0, evid = 5, cmt = "depot") |>
            et(time = 3, amt = 50, rate = -2, ii = 12, ss = .ss, cmt = "depot") |>
            et(.o)
          .r <- rxSolve(.m, .ev, .p)
          expect_equal(.r$center[.r$time %in% .o], .w, tolerance = 1e-6)
        }
      }
    })
  }
})
