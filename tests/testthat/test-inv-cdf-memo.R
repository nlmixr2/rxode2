rxTest({
  test_that("the inverse-CDF memo returns the same value for repeated inputs", {
    # alternate a few nearby inputs, as decoders and their sensitivities do
    .a <- rep(c(0.5, 0.50001, 2, 0.5, 7.3, 0.50001), 30L)
    .b <- rep(c(1.5, 3, 1.5, 0.8, 3, 1.5), 30L)
    .p <- rep(c(0.25, 0.25, 0.75, 0.9, 0.25, 0.75), 30L)
    .u <- !duplicated(cbind(.a, .b, .p))
    .same <- function(f, ...) {
      .args <- list(...)
      .all <- do.call(f, .args)
      .once <- do.call(f, lapply(.args, function(x) x[.u]))
      .key <- do.call(paste, .args)
      expect_identical(.all, .once[match(.key, .key[.u])])
      .all
    }
    expect_equal(.same(gammapInv, .a, .p), qgamma(.p, .a), tolerance = 1e-12)
    expect_equal(.same(gammaqInv, .a, .p), qgamma(.p, .a, lower.tail = FALSE), tolerance = 1e-12)
    expect_equal(.same(ibetaInv, .a, .b, .p), qbeta(.p, .a, .b), tolerance = 1e-10)
    expect_equal(.same(studentTInv, .p, .b), qt(.p, .b), tolerance = 1e-10)
    .x <- rep(c(0.4, 1.2, 0.4, 3, 1.2, 0.4), 30L)
    .ga <- .same(gammapInva, .x, .p)
    expect_equal(pgamma(.x, .ga), .p, tolerance = 1e-8)
    .gq <- .same(gammaqInva, .x, .p)
    expect_equal(pgamma(.x, .gq, lower.tail = FALSE), .p, tolerance = 1e-8)
  })

  test_that("every eta-dist decoder with a root-finding inverse decodes correctly", {
    # the dist(eta) quantile templates that go through the memo, solved on
    # two threads with parameters that repeat and alternate across subjects
    .m <- rxode2({
      gam <- gammapInv(a, u) / r
      chi <- 2 * gammapInv(nu / 2, u)
      ichi <- 1 / (2 * gammapInv(nu / 2, 1 - u))
      sichi <- nu * s * s / (2 * gammapInv(nu / 2, 1 - u))
      igam <- r / gammapInv(a, 1 - u)
      bet <- ibetaInv(a, b, u)
      bprop <- ibetaInv(mu * k, (1 - mu) * k, u)
      tt <- m0 + s * studentTInv(u, nu)
    })
    .n <- 240L
    .p <- data.frame(
      a = rep(c(0.5, 0.50001, 2, 7.3), length.out = .n),
      b = rep(c(1.5, 3, 0.8), length.out = .n),
      r = rep(c(1, 0.3), length.out = .n),
      nu = rep(c(3, 3.00001, 12), length.out = .n),
      s = rep(c(1, 2.5), length.out = .n),
      mu = rep(c(0.3, 0.7), length.out = .n),
      k = rep(c(4, 20), length.out = .n),
      m0 = 0.1,
      u = rep(c(0.05, 0.25, 0.25, 0.5, 0.9, 0.999), length.out = .n)
    )
    .s <- as.data.frame(rxSolve(.m, .p, et(0), cores = 2L))
    .s <- .s[order(.s$sim.id), ]
    with(.p, {
      expect_equal(.s$gam, qgamma(u, a, r), tolerance = 1e-10)
      expect_equal(.s$chi, qchisq(u, nu), tolerance = 1e-10)
      expect_equal(.s$ichi, 1 / qchisq(1 - u, nu), tolerance = 1e-10)
      expect_equal(.s$sichi, nu * s^2 / qchisq(1 - u, nu), tolerance = 1e-10)
      expect_equal(.s$igam, r / qgamma(1 - u, a), tolerance = 1e-10)
      expect_equal(.s$bet, qbeta(u, a, b), tolerance = 1e-10)
      expect_equal(.s$bprop, qbeta(u, mu * k, (1 - mu) * k), tolerance = 1e-10)
      expect_equal(.s$tt, 0.1 + s * qt(u, nu), tolerance = 1e-10)
    })
  })
})
