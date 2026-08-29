## The C common-subexpression pass must agree with the R walker BYTE FOR BYTE.
##
## The stored fixture (test-optexpr-fixture.R) covers the small and medium
## cases.  This covers the one the C pass exists for: a second-order sensitivity
## model, where the same subexpressions repeat across hundreds of statements and
## the R walker's named-list counting is quadratic.  It is built here rather than
## stored because generating it takes minutes.
rxTest({
  .sens2 <- function(model, vars) {
    .env <- rxS(model)
    paste(c(model, .rxJacobian(.env), .rxSens(.env, vars),
            .rxSens(.env, vars, vars)), collapse = "\n")
  }

  test_that("the C pass reproduces the R walker on a second-order model", {
    skip_on_cran()
    .m <- paste(c(
      "circ0 <- exp(lcirc0 + e1)", "mtt <- exp(lmtt + e2)",
      "slope <- exp(lslope + e3)", "gamma <- exp(lgamma + e4)",
      "ktr <- 4/mtt",
      "fdbk <- (circ0/(circ + 1e-12))^gamma",
      "edrug <- slope*centr",
      "d/dt(centr) <- -ktr*centr",
      "d/dt(prol) <- ktr*prol*(1 - edrug)*fdbk - ktr*prol",
      "d/dt(tr1) <- ktr*prol - ktr*tr1",
      "d/dt(circ) <- ktr*tr1 - ktr*circ",
      "cp <- circ/circ0"), collapse = "\n")
    .txt <- .sens2(.m, c("lcirc0", "lmtt", "lslope", "lgamma"))
    .norm <- rxNorm(.txt)

    .c <- .rxOptExprC(.norm)
    expect_false(is.na(.c))          # this model must not decline

    withr::with_options(list(rxode2.optExprC = FALSE), {
      .r <- suppressMessages(rxOptExpr(.txt, "model"))
    })
    expect_identical(.c, .r)
  })

  test_that("the C pass is independent of the thread count", {
    ## the count runs per statement across threads and is merged by
    ## min(firstSeen); if that were wrong the rx_expr_ numbering would move
    ## with the number of threads
    skip_on_cran()
    .m <- paste(c("a <- exp(p1 + p2)", "b <- exp(p1 + p2) + exp(p3 + p4)",
                  "d/dt(x) <- -exp(p1 + p2)*x + exp(p3 + p4)*a",
                  "d/dt(y) <- exp(p3 + p4)*x - exp(p1 + p2)*y",
                  "cp <- x/a + y/b"), collapse = "\n")
    .txt <- .sens2(.m, c("p1", "p2", "p3"))
    .norm <- rxNorm(.txt)
    .old <- rxCores()
    on.exit(setRxThreads(.old), add = TRUE)
    .res <- vapply(c(1L, 2L, 4L), function(n) {
      setRxThreads(n)
      .o <- .rxOptExprC(.norm)
      if (is.na(.o)) NA_character_ else .o
    }, character(1))
    expect_false(any(is.na(.res)))
    expect_identical(.res[1], .res[2])
    expect_identical(.res[1], .res[3])
  })
})
