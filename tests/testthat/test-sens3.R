rxTest({
  # Analytic third-order forward sensitivities (rxExpandSens3_ / .rxSens vars3)
  # must match a central finite difference of the analytic second-order
  # sensitivities (which differentiates an exact derivative, so the agreement
  # is finite-difference-step limited rather than formula limited).

  .nm <- function(cS, ...) paste0("rx__sens_", cS, "_BY_",
                                  paste(c(...), collapse = "_BY_"), "__")

  m <- "d/dt(depot)  = -ka*depot\nd/dt(center) = ka*depot - (cl/v)*center"
  model <- rxode2::rxS(rxode2::rxGetModel(m), TRUE, promoteLinSens = FALSE)
  vars  <- c("ka", "cl", "v")
  st    <- rxode2:::rxStateOde(model)

  invisible(rxode2::.rxJacobian(model, c(st, vars)))
  s1 <- rxode2::.rxSens(model, vars)              # first order
  s2 <- rxode2::.rxSens(model, vars, vars)        # second order
  s3 <- rxode2::.rxSens(model, vars, vars, vars)  # third order (new path)

  test_that("rxExpandSens3_ stores ..sens3 and emits one ODE per triple/state", {
    expect_true(!is.null(model$..sens3))
    expect_equal(length(s3), length(st) * length(vars)^3)
  })

  mod <- rxode2::rxode2(paste(c(
    "d/dt(depot)  = -ka*depot",
    "d/dt(center) =  ka*depot - (cl/v)*center",
    s1, s2, s3), collapse = "\n"))

  p0     <- c(ka = 1.1, cl = 3.0, v = 20.0)
  ev     <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(seq(0.25, 12, by = 0.25))
  solve1 <- function(p) as.data.frame(rxode2::rxSolve(mod, params = p, ev,
                                                      returnType = "data.frame",
                                                      atol = 1e-12, rtol = 1e-12))
  base <- solve1(p0)

  # d^3 x / d(pa) d(pb) d(pc) vs central FD of d^2 x / d(pb) d(pc) over pa
  relErr <- function(pa, pb, pc, state, h = 1e-4) {
    pp <- p0; pm <- p0; pp[pa] <- pp[pa]*(1+h); pm[pa] <- pm[pa]*(1-h); hh <- p0[pa]*h
    d <- (solve1(pp)[[.nm(state, pb, pc)]] - solve1(pm)[[.nm(state, pb, pc)]]) / (2*hh)
    a <- base[[.nm(state, pa, pb, pc)]]
    max(abs(a - d) / (abs(a) + 1e-8))
  }

  combos <- list(c("ka","ka","ka","center"), c("cl","cl","cl","center"),
                 c("v","v","v","center"),    c("cl","ka","ka","center"),
                 c("v","cl","ka","center"),  c("ka","cl","v","center"),
                 c("ka","ka","ka","depot"),  c("v","v","cl","center"))

  test_that("analytic third-order sensitivities match finite differences", {
    err <- max(vapply(combos, function(z) relErr(z[1], z[2], z[3], z[4]), numeric(1)))
    expect_true(err < 1e-4)
  })

  test_that("mixed third derivatives are symmetric in their variables", {
    # d^3/dka dcl dv is independent of ordering
    expect_equal(base[[.nm("center","ka","cl","v")]],
                 base[[.nm("center","v","cl","ka")]], tolerance = 1e-8)
    expect_equal(base[[.nm("center","ka","cl","v")]],
                 base[[.nm("center","cl","ka","v")]], tolerance = 1e-8)
  })
})
