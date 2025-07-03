test_that("activation functions", {

  r <- rxode2({
    r <- ReLU(time)
    dr <- dReLU(time)
    #
    l <- lReLU(time)
    dl <- dlReLU(time)
    #
    p <- PReLU(time, alpha)
    dp <- dPReLU(time, alpha)
    dpa <- dPReLUa(time, alpha)
    dpa1 <- dPReLUa1(time, alpha)
    #
    g <- GELU(time)
    dg <- dGELU(time)
    d2g <- d2GELU(time)
    d3g <- d3GELU(time)
    d4g <- d4GELU(time)
    #
    s <- Swish(time)
    ds <- dSwish(time)
    #
    sp <- softplus(time)
    dsp <- dsoftplus(time)
    #
    el <- ELU(time, alpha)
    del <- dELU(time, alpha)
    d2el <- d2ELU(time, alpha)
    d2a <- dELUa(time, alpha)
    d2ael <- d2aELU(time, alpha)
    dael <- d2aELU(time, alpha)
    d2aela <- d2ELUa(time, alpha)
  })

  e <- et(seq(-10, 10, length.out = 41))

  s <- suppressWarnings(rxSolve(r, e, c(alpha=2)))

  expect_equal(ReLU(e$time), s$r)
  expect_equal(ReLU(e$time), vapply(e$time,
                                    function(x) {
                                      max(0, x)
                                    }, double(1)))
  expect_equal(dReLU(e$time), s$dr)
  expect_equal(dReLU(e$time), vapply(e$time,
                                    function(x) {
                                      (x > 0)*1
                                    }, double(1)))
  expect_equal(lReLU(e$time), s$l)
  expect_equal(lReLU(e$time), vapply(e$time,
                                     function(x) {
                                       (x > 0)*x + (x <= 0)*0.01*x
                                    }, double(1)))
  expect_equal(dlReLU(e$time), s$dl)
  expect_equal(dlReLU(e$time), vapply(e$time,
                                     function(x) {
                                       (x > 0) + (x <= 0)*0.01
                                     }, double(1)))

  expect_equal(PReLU(e$time, 2), s$p)
  expect_equal(PReLU(e$time, 2), vapply(e$time,
                                     function(x) {
                                       (x > 0)*x + (x <= 0)*2*x
                                     }, double(1)))

  expect_equal(dPReLU(e$time, 2), s$dp)
  expect_equal(dPReLU(e$time, 2), vapply(e$time,
                                        function(x) {
                                          (x > 0) + (x <= 0)*2
                                        }, double(1)))

  expect_equal(GELU(e$time), s$g)
  expect_equal(GELU(e$time), vapply(e$time,
                                    function(x) {
                                      phi(x)*x
                                    }, double(1)))

  expect_equal(dGELU(e$time), s$dg)
  expect_equal(dGELU(e$time), vapply(e$time,
                                     function(x) {
                                       M_SQRT1_2 <- 1/sqrt(2)
                                       1.0*x*exp(-x^2*M_SQRT1_2^2)*M_SQRT1_2/sqrt(pi) + 0.5*(1 + erf(x*M_SQRT1_2))
                                     }, double(1)))
  expect_equal(d2GELU(e$time), s$d2g)
  expect_equal(d2GELU(e$time),
               vapply(e$time,
                      function(x) {
                        M_SQRT1_2 <- 1/sqrt(2)
                        2.0*exp(-x^2*M_SQRT1_2^2)*M_SQRT1_2/sqrt(pi) - 2.0*x^2*exp(-x^2*M_SQRT1_2^2)*M_SQRT1_2^3/sqrt(pi)
                      }, double(1)))

  expect_equal(d3GELU(e$time), s$d3g)
  expect_equal(d3GELU(e$time),
               vapply(e$time,
                      function(x) {
                        M_SQRT1_2 <- 1/sqrt(2)
                        M_1_SQRT_2PI <- 1/sqrt(2*pi)
                        -8.0*x*exp(-x^2*0.5)*0.5*M_1_SQRT_2PI + 4.0*x^3*exp(-x^2*0.5)*0.25*M_1_SQRT_2PI
                      }, double(1)))
  expect_equal(d4GELU(e$time), s$d4g)
  expect_equal(d4GELU(e$time),
               vapply(e$time,
                      function(x) {
                        M_SQRT1_2 <- 1/sqrt(2)
                        -8.0*exp(-x^2*M_SQRT1_2^2)*M_SQRT1_2^3/sqrt(pi) + 28.0*x^2*exp(-x^2*M_SQRT1_2^2)*M_SQRT1_2^5/sqrt(pi) - 8.0*x^4*exp(-x^2*M_SQRT1_2^2)*M_SQRT1_2^7/sqrt(pi)
                      }, double(1)))

  expect_equal(Swish(e$time), s$s)
  expect_equal(Swish(e$time), vapply(e$time,
                                     function(x) {
                                        x/(1 + exp(-x))
                                     }, double(1)))

  expect_equal(dSwish(e$time), s$ds)
  expect_equal(dSwish(e$time), vapply(e$time,
                                     function(x) {
                                       x*exp(-x)/(1 + exp(-x))^2 + (1 + exp(-x))^(-1)
                                     }, double(1)))

  expect_equal(softplus(e$time), s$sp)
  expect_equal(softplus(e$time), vapply(e$time,
                                        function(x) {
                                          log(1 + exp(x))
                                        }, double(1)))

  expect_equal(dsoftplus(e$time), s$dsp)
  expect_equal(dsoftplus(e$time), vapply(e$time,
                                        function(x) {
                                          exp(x)/(1 + exp(x))
                                        }, double(1)))

  expect_equal(ELU(e$time, 2), s$el)
  expect_equal(ELU(e$time, 2), vapply(e$time,
                                      function(x) {
                                        (x > 0)*x + (x <= 0)*2*(exp(x) - 1)
                                      }, double(1)))

  # dELU/dx
  expect_equal(dELU(e$time, 2), s$del)
  expect_equal(dELU(e$time, 2), vapply(e$time,
                                      function(x) {
                                        (x > 0) + (x <= 0)*2*exp(x)
                                      }, double(1)))

  # d2ELU/d2x
  expect_equal(d2ELU(e$time, 2), s$d2el)
  expect_equal(d2ELU(e$time, 2), vapply(e$time,
                                        function(x) {
                                          (x <= 0)*2*exp(x)
                                        }, double(1)))

  # dELU/dalpha
  expect_equal(dELUa(e$time, 2), s$d2a)
  expect_equal(dELUa(e$time, 2), vapply(e$time,
                                        function(x) {
                                          (x <= 0)*(exp(x) - 1)
                                        }, double(1)))

  # d2ELU/dalphad2x
  expect_equal(d2ELUa(e$time, 2), s$d2ael)
  expect_equal(d2ELUa(e$time, 2), vapply(e$time,
                                        function(x) {
                                          (x <= 0)*exp(x)
                                        }, double(1)))

})
