rxTest({
  test_that("dnorm() solves with 1-3 arguments", {
    o <- rxode2({
      a1 <- dnorm(a)
      a2 <- dnorm(a, 0.5)
      a3 <- dnorm(a, 0.5, 2)
    })
    .a <- c(-1, 0, 3)
    s <- rxSolve(o, data.frame(a = .a), et(0))
    expect_equal(s$a1, dnorm(.a))
    expect_equal(s$a2, dnorm(.a, 0.5))
    expect_equal(s$a3, dnorm(.a, 0.5, 2))

    suppressMessages(expect_error(rxode2({
      o <- dnorm()
    })))
    suppressMessages(expect_error(rxode2({
      o <- dnorm(a, b, c, d)
    })))
  })

  test_that("dnorm()/qnorm() translate to symengine with 1-3 arguments", {
    expect_equal(rxToSE("dnorm(x)"), "dnorm(x)")
    expect_equal(rxToSE("dnorm(x, mu, s)"), "(dnorm(((x)-(mu))/(s))/(s))")
    expect_equal(rxToSE("qnorm(p, mu, s)"), "((mu)+(s)*sqrt(2)*erfinv(2*(p)-1))")
    expect_equal(rxFromSE("Derivative(dnorm(x),x)"), "-(x)*dnorm(x)")
    o <- rxode2({
      a <- 2 * qnorm(p, mu, s)
      b <- 1 / dnorm(x, mu, s)
    })
    s <- rxSolve(o, c(p = 0.3, mu = 0.2, s = 1.3, x = 0.7), et(0))
    expect_equal(s$a, 2 * qnorm(0.3, 0.2, 1.3))
    expect_equal(s$b, 1 / dnorm(0.7, 0.2, 1.3))
    .s <- rxS("a = 2*qnorm(p, mu, s)\nb = 1/dnorm(x, mu, s)")
    .a <- with(.s, D(a, mu))
    .b <- with(.s, D(b, mu))
    .r <- c(rxFromSE(.a), rxFromSE(.b))
    o <- rxode2(paste0("da = ", .r[1], "\ndb = ", .r[2]))
    s <- rxSolve(o, c(p = 0.3, mu = 0.2, s = 1.3, x = 0.7), et(0))
    expect_equal(s$da, 2)
    .h <- 1e-5
    expect_equal(s$db, (1 / dnorm(0.7, 0.2 + .h, 1.3) - 1 / dnorm(0.7, 0.2 - .h, 1.3)) / (2 * .h), tolerance = 1e-6)
  })

  test_that("dnorm()/pnorm()/qnorm() symbolic derivatives match finite differences", {
    .s <- rxS("y = dnorm(x, mu, s)\np=pnorm(x, mu, s)\nq=qnorm(x, mu, s)")
    .d1 <- with(.s, D(y, s))
    .d2 <- with(.s, D(y, x))
    .d3 <- with(.s, D(y, mu))
    .d4 <- with(.s, D(p, x))
    .d5 <- with(.s, D(q, x))
    .d6 <- with(.s, D(D(y, x), x))
    .r <- c(rxFromSE(.d1), rxFromSE(.d2), rxFromSE(.d3), rxFromSE(.d4), rxFromSE(.d5), rxFromSE(.d6))
    o <- rxode2(paste(paste0("r", 1:6, " = ", .r), collapse = "\n"))
    .x <- 0.7
    .mu <- 0.2
    .sd <- 1.3
    .h <- 1e-5
    s <- rxSolve(o, c(x = .x, mu = .mu, s = .sd), et(0))
    .f <- function(x = .x, mu = .mu, s = .sd) dnorm(x, mu, s)
    expect_equal(s$r1, (.f(s = .sd + .h) - .f(s = .sd - .h)) / (2 * .h), tolerance = 1e-6)
    expect_equal(s$r2, (.f(x = .x + .h) - .f(x = .x - .h)) / (2 * .h), tolerance = 1e-6)
    expect_equal(s$r3, (.f(mu = .mu + .h) - .f(mu = .mu - .h)) / (2 * .h), tolerance = 1e-6)
    expect_equal(s$r4, dnorm(.x, .mu, .sd))
    expect_equal(s$r5, .sd / dnorm(qnorm(.x)))
    expect_equal(s$r6, ((.x - .mu)^2 / .sd^2 - 1) * dnorm(.x, .mu, .sd) / .sd^2)
  })

  test_that("dnorm()/pnorm()/qnorm() take R's arguments in model functions", {
    f <- function() {
      model({
        d1 <- dnorm(x, sd = 2, mean = m)
        d2 <- dnorm(x, log = TRUE)
        d3 <- dnorm(x, 1, 2, log = TRUE)
        p1 <- pnorm(x, lower.tail = FALSE)
        p2 <- pnorm(x, m, 2, lower.tail = FALSE, log.p = TRUE)
        p3 <- pnorm(x, sd = 3)
        p4 <- pnorm(x, 0.5)
        q1 <- qnorm(pp, lower.tail = FALSE)
        q2 <- qnorm(lp, m, 2, lower.tail = FALSE, log.p = TRUE)
        q3 <- qnorm(pp, 1, 2)
        q4 <- qnorm(lp, log.p = TRUE)
      })
    }
    expect_equal(
      modelExtract(f()),
      c(
        "d1 <- dnorm(x, m, 2)",
        "d2 <- (-0.5 * x^2 - 0.5 * log(2 * pi))",
        "d3 <- (-0.5 * ((x - 1)/2)^2 - 0.5 * log(2 * pi) - log(2))",
        "p1 <- pnorm(-x)",
        "p2 <- log(pnorm(-x, -m, 2))",
        "p3 <- pnorm(x, 0, 3)",
        "p4 <- pnorm(x, 0.5)",
        "q1 <- (-qnorm(pp))",
        "q2 <- (-qnorm(exp(lp), -m, 2))",
        "q3 <- qnorm(pp, 1, 2)",
        "q4 <- qnorm(exp(lp))"
      )
    )
    d <- data.frame(
      x = c(-1, 0, 3),
      m = c(0.2, 0.3, -1),
      pp = c(0.1, 0.5, 0.9),
      lp = log(c(0.1, 0.5, 0.9))
    )
    s <- suppressMessages(rxSolve(f, d, et(0)))
    expect_equal(s$d1, dnorm(d$x, d$m, 2))
    expect_equal(s$d2, dnorm(d$x, log = TRUE))
    expect_equal(s$d3, dnorm(d$x, 1, 2, log = TRUE))
    expect_equal(s$p1, pnorm(d$x, lower.tail = FALSE))
    expect_equal(s$p2, pnorm(d$x, d$m, 2, lower.tail = FALSE, log.p = TRUE))
    expect_equal(s$p3, pnorm(d$x, sd = 3))
    expect_equal(s$p4, pnorm(d$x, 0.5))
    expect_equal(s$q1, qnorm(d$pp, lower.tail = FALSE))
    expect_equal(s$q2, qnorm(d$lp, d$m, 2, lower.tail = FALSE, log.p = TRUE))
    expect_equal(s$q3, qnorm(d$pp, 1, 2))
    expect_equal(s$q4, qnorm(d$lp, log.p = TRUE))

    h <- function() {
      model({
        a <- pnorm(-x, lower.tail = FALSE)
        b <- qnorm(pp, -1, 1/3, lower.tail = FALSE)
        c <- pnorm(depth, 1/3)
        e <- pnorm(x, sd = sdn, lower.tail = FALSE)
      })
    }
    expect_equal(
      modelExtract(h()),
      c(
        "a <- pnorm(-(-x))",
        "b <- (-qnorm(pp, -(-1), 1/3))",
        "c <- pnorm(depth, 1/3)",
        "e <- pnorm(-x, 0, sdn)"
      )
    )
    s <- suppressMessages(suppressWarnings(rxSolve(
      h,
      data.frame(x = c(-1, 2), pp = c(0.2, 0.7), depth = c(0, 1), sdn = c(-1, 2)),
      et(0)
    )))
    expect_equal(s$e, suppressWarnings(pnorm(c(-1, 2), sd = c(-1, 2), lower.tail = FALSE)))
    expect_equal(s$a, pnorm(-c(-1, 2), lower.tail = FALSE))
    expect_equal(s$b, qnorm(c(0.2, 0.7), -1, 1 / 3, lower.tail = FALSE))
    expect_equal(s$c, pnorm(c(0, 1), 1 / 3))

    g <- function() {
      model({
        a <- pnorm(x, lower.tail = y)
      })
    }
    expect_error(g(), "lower.tail")
  })

  test_that("dnorm() residual error specification is unchanged", {
    g <- function() {
      ini({
        a <- 1
        add.sd <- 0.1
      })
      model({
        y <- a * t
        y ~ add(add.sd) + dnorm()
      })
    }
    expect_equal(as.character(g()$predDf$distribution), "dnorm")
  })
})
