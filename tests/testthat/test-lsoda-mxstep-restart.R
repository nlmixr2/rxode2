test_that("liblsoda counts maxsteps per solve call, not across dose restarts", {
  m <- rxode2({
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - vm * center / (km + center)
  })
  p <- c(ka = 0.5, vm = 8.5, km = 30)
  ev <- et(amt = 100, cmt = "depot", ii = 12, addl = 399) |>
    et(c(12 * 399 + 1, 12 * 399 + 6))
  ref <- rxSolve(m, p, ev, method = "liblsoda", maxsteps = 1e6, cores = 1)
  # every dose restarts lsoda; the steps of all restarts must not add up
  # against maxsteps (ODEPACK resets NST/NSLAST at istate 1)
  expect_silent(
    lim <- rxSolve(m, p, ev, method = "liblsoda", maxsteps = 500, cores = 1)
  )
  expect_false(anyNA(lim$center))
  expect_identical(as.data.frame(lim), as.data.frame(ref))
})
