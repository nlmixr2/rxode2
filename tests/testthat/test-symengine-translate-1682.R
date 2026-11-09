rxTest({
  # The shapes issue #1682 sent to the R walker -- log() of a sum, Subs() of a
  # derivative, higher-order Derivative() -- now translated in C.  Held to the
  # R walker directly, which is the oracle, rather than to a recorded fixture.

  .rTxt <- function(s, numDer = 1L) {
    .old <- .rxSEstate$fromNumDer
    on.exit(.rxSEstate$fromNumDer <- .old)
    .rxSEstate$fromNumDer <- numDer
    .x <- .rxSubsTupleExpand(.rxUnXi(s))
    tryCatch(.rxFromSE(eval(parse(text = paste0("quote({", .x, "})")))),
      error = function(e) NA_character_
    )
  }

  .cmp <- function(inputs, numDer = 1L, minAccept = 1) {
    rxSymEngineCacheReset()
    .c <- .rxFromSEC(inputs, numDer)
    .r <- vapply(inputs, .rTxt, character(1), numDer = numDer, USE.NAMES = FALSE)
    .acc <- !is.na(.c)
    expect_gte(mean(.acc), minAccept)
    .bad <- which(.acc & (is.na(.r) | .c != .r))
    expect_equal(length(.bad), 0L,
      info = paste0(inputs[.bad], "\n  C: ", .c[.bad], "\n  R: ", .r[.bad], collapse = "\n")
    )
  }

  test_that("log()/lgamma() of a sum, trig of pi, and folds match the R walker", {
    .cmp(c(
      "log(1e-06 + rxGt(-T1 + t, 0.0)*(-T1 + t))",
      "-log(1e-06 + a*b) + log(exp(THETA_4_))",
      "log(1.0 + exp(a))", "log(1 + exp(a^2))", "log(1 - a)", "log(a + 1 + b)",
      "log(2 + a)", "lgamma(1 + a)", "loggamma(1 + a)", "lgamma(a + 1)",
      "sin(pi*a)", "cos(a*pi/2)", "tan(pi/a)", "sin(2*(t - a)/p)", "sin(pi)",
      "a*log(4)", "a/(2^-2)", "a*(pi/3)", "a/gamma(3)", "exp(1e-06)*a",
      "a*100000", "a + 0.0001", "a*1234567890123456789", "log(1e-04 + a)",
      "exp(a)/log(3)", "a/(3*pi)", "1.0/(1.0 + exp(exp(THETA_3_)*(-log(1e-06 + a) + log(exp(THETA_4_)))))"
    ))
  })

  test_that("Subs() and higher-order Derivative() match the R walker", {
    .s <- c(
      "Subs(Derivative(abs0(_xi_1), _xi_1), (_xi_1), (a - b))",
      "2*Subs(Derivative(abs0(_xi_1), _xi_1), (_xi_1), (a - exp(THETA_2_)))*c",
      "-Subs(Derivative(abs0(_xi_1), _xi_1), (_xi_1), (-a + b))",
      "Subs(Derivative(abs0(_xi_1), _xi_1, _xi_1), (_xi_1), (a - b))",
      "Subs(Derivative(rxGt(_xi_1, b), _xi_1), (_xi_1), (a + c))",
      "Derivative(abs0(rx_xi_1), rx_xi_1, rx_xi_1)",
      "Derivative(rxTBS(a, b, c, d, f), a, a)",
      "Derivative(llikNorm(a, b, c), b)"
    )
    .cmp(.s, minAccept = 1)
  })

  test_that("finite differences match the R walker", {
    .s <- c(
      "Derivative(logspace_add(a, b), a)",
      "Subs(Derivative(lgamma(_xi_1), _xi_1), (_xi_1), (1 + a - b))",
      "Derivative(llikNorm(a, b, c), a)"
    )
    .cmp(.s, numDer = 1L)
    .cmp(.s, numDer = 2L)
  })

  test_that("derivatives of a #1682-shaped model match the R walker", {
    .m <- paste(
      "TON = exp(THETA[2])",
      "ONSET = (t + TON - abs(t - TON))/2/TON",
      "D1 = (t > T1)*(t - T1)",
      "H1 = 1/(1 + exp(exp(THETA[3])*(log(exp(THETA[4])) - log(D1 + 1e-06))))",
      "A1 = THETA[5]*R1D1/(exp(THETA[6]) + R1D1)",
      "E1 = A1*H1",
      "LMAX = THETA[1] + ETA[1] + E1",
      "PRESP = ONSET/(1 + exp(-LMAX))",
      "rx_pred_ = log(PRESP/(1 - PRESP))",
      sep = "\n"
    )
    .s <- rxS(.m)
    .d <- function(a, b) {
      .t <- sprintf('assign("rxTmpD", with(.s, D(D(rx_pred_, %s), %s)), envir = .s)', a, b)
      as.character(eval(parse(text = .t)))
    }
    .d1 <- function(a) {
      .t <- sprintf('assign("rxTmpD", with(.s, D(rx_pred_, %s)), envir = .s)', a)
      as.character(eval(parse(text = .t)))
    }
    .p <- c("THETA_2_", "THETA_3_", "THETA_4_", "ETA_1_")
    .lines <- c(vapply(.p, .d1, character(1)))
    for (.i in seq_along(.p)) {
      for (.j in seq_len(.i)) {
        .lines <- c(.lines, .d(.p[.i], .p[.j]))
      }
    }
    .cmp(unname(.lines))
  })
})
