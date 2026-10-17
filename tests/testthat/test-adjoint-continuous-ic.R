rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # Initial conditions that depend on parameters: the continuous adjoint must
  # add lambda(t0)^T dX0/dtheta.  Closed forms:
  #   one: y = y0 exp(-k t)
  #   two: x = a b exp(-k t), z = a b k / (k2 - k) (exp(-k t) - exp(-k2 t))
  .tt <- c(0, 0.5, 1, 2, 3)
  .one <- list(txt = "y(0) <- y0\nd/dt(y) <- -k*y", p = c(k = 0.7, y0 = 3))
  .two <- list(txt = "x(0) <- a*b\nd/dt(x) <- -k*x\nd/dt(z) <- k*x - k2*z", p = c(a = 2, b = 1.5, k = 0.7, k2 = 0.3))
  .grad <- function(f, p) {
    .d <- deriv(f, names(p), function.arg = c(names(p), "t"))
    attr(do.call(.d, c(as.list(p), list(t = .tt))), "gradient")
  }
  .known <- list(
    one = list(y = .grad(~ y0 * exp(-k * t), .one$p)),
    two = list(
      x = .grad(~ a * b * exp(-k * t), .two$p),
      z = .grad(~ a * b * k / (k2 - k) * (exp(-k * t) - exp(-k2 * t)), .two$p)
    )
  )
  .mods <- list(one = .one, two = .two)
  # the continuous adjoint converges at O(denseBy^2)
  .db <- 2e-4

  .check <- function(a, nm) {
    for (.s in names(.known[[nm]])) {
      for (.pn in names(.mods[[nm]]$p)) {
        .cn <- paste0("rx__sens_", .s, "_BY_", .pn, "__")
        expect_lt(max(abs(a[[.cn]] - .known[[nm]][[.s]][, .pn])), 1e-6, label = paste(nm, .cn))
      }
    }
  }

  test_that(".rxAdjointSolve adds the initial-condition term", {
    for (.nm in names(.mods)) {
      .m <- .mods[[.nm]]
      # approx() collapses the forward checkpoint's duplicated t0 row
      .a <- suppressWarnings(.rxAdjointSolve(.m$txt, .m$p, et(.tt), names(.m$p), .tt, denseBy = .db, atol = 1e-12, rtol = 1e-12))
      .check(.a, .nm)
    }
  })

  test_that("rxSolveAdjoint adds the initial-condition term", {
    for (.nm in names(.mods)) {
      .m <- .mods[[.nm]]
      .a <- rxSolveAdjoint(.m$txt, .m$p, et(.tt), names(.m$p), denseBy = .db, atol = 1e-12, rtol = 1e-12)
      .check(.a[.a$time %in% .tt, , drop = FALSE], .nm)
    }
  })

  test_that("the adjoint objective gradient adds the initial-condition term, including an observation at t0", {
    .m <- .one
    .obs <- c(2.5, 2, 1.5, 0.8, 0.3)
    .y <- 3 * exp(-0.7 * .tt)
    .gKnown <- colSums((.y - .obs) * .known$one$y)
    .b <- .rxAdjointGradBuild(.m$txt, names(.m$p), "y", et(.tt))
    expect_lt(max(abs(.rxAdjointGradEvalC(.b, .m$p, .tt, .obs, denseBy = .db, atol = 1e-12, rtol = 1e-12) - .gKnown)), 1e-6)
    # approx() collapses the forward checkpoint's duplicated t0 row
    .gR <- suppressWarnings(.rxAdjointGradEval(.b, .m$p, .tt, .obs, denseBy = .db, atol = 1e-12, rtol = 1e-12))
    expect_lt(max(abs(.gR - .gKnown)), 1e-6)
  })
})
