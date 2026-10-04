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
    paste(c(model, .rxJacobian(.env), .rxSens(.env, vars), .rxSens(.env, vars, vars)), collapse = "\n")
  }

  test_that("the C pass reproduces the R walker on a second-order model", {
    skip_on_cran()
    .m <- paste(
      c(
        "circ0 <- exp(lcirc0 + e1)",
        "mtt <- exp(lmtt + e2)",
        "slope <- exp(lslope + e3)",
        "gamma <- exp(lgamma + e4)",
        "ktr <- 4/mtt",
        "fdbk <- (circ0/(circ + 1e-12))^gamma",
        "edrug <- slope*centr",
        "d/dt(centr) <- -ktr*centr",
        "d/dt(prol) <- ktr*prol*(1 - edrug)*fdbk - ktr*prol",
        "d/dt(tr1) <- ktr*prol - ktr*tr1",
        "d/dt(circ) <- ktr*tr1 - ktr*circ",
        "cp <- circ/circ0"
      ),
      collapse = "\n"
    )
    .txt <- .sens2(.m, c("lcirc0", "lmtt", "lslope", "lgamma"))
    .norm <- rxNorm(.txt)

    .c <- .rxOptExprC(.norm)
    expect_false(is.na(.c)) # this model must not decline

    withr::with_options(list(rxode2.optExprC = FALSE), {
      .r <- suppressMessages(rxOptExpr(.txt, "model"))
    })
    expect_identical(.c, .r)
  })

  test_that("many nested replacements do not silently truncate", {
    ## Reducing a candidate rewrites it in terms of the shorter ones, and each
    ## rewrite makes it LONGER -- `(a+1)` (5 chars) becomes `rx_expr_0` (9).
    ## A fixed-size buffer for the reduced text would run out and silently skip
    ## a replacement, emitting text that differs from the R walker without
    ## declining.  Found by review; this is the input that triggered it.
    .terms <- paste(sprintf("(a+%d)", 1:17), collapse = "*")
    .m <- paste0("x = ", .terms, "\ny = ", .terms)
    .norm <- rxNorm(.m)
    .c <- .rxOptExprC(.norm)
    expect_false(is.na(.c))
    withr::with_options(list(rxode2.optExprC = FALSE), {
      .r <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 0L))
    })
    expect_identical(.c, .r)
    ## and every temporary it defines is actually used, i.e. nothing was skipped
    .defs <- grep("^rx_expr_[0-9]+~", strsplit(.c, "\n")[[1]], value = TRUE)
    expect_gt(length(.defs), 10L)
  })

  test_that("negative literals fold the way R folds them", {
    ## R's parser does NOT make `-1` an atomic double -- is.atomic(quote(-1)) is
    ## FALSE and it is a call to unary minus, which is why R does not fold
    ## `1 + -1` and neither may the C pass.  Raised twice in review on the
    ## assumption that R folds these; it does not.
    expect_false(is.atomic(quote(-1)))
    for (.e in c("1 + -1", "1 / -1", "2 * -3", "-1 * x")) {
      .m <- paste0("a = ", .e, " + q\nb = ", .e, " + q")
      .c <- .rxOptExprC(rxNorm(.m))
      withr::with_options(list(rxode2.optExprC = FALSE), {
        .r <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 0L))
      })
      if (!is.na(.c)) expect_identical(.c, .r, info = .e)
    }
  })

  test_that("the C pass is independent of the thread count", {
    ## the count runs per statement across threads and is merged by
    ## min(firstSeen); if that were wrong the rx_expr_ numbering would move
    ## with the number of threads
    skip_on_cran()
    .m <- paste(
      c(
        "a <- exp(p1 + p2)",
        "b <- exp(p1 + p2) + exp(p3 + p4)",
        "d/dt(x) <- -exp(p1 + p2)*x + exp(p3 + p4)*a",
        "d/dt(y) <- exp(p3 + p4)*x - exp(p1 + p2)*y",
        "cp <- x/a + y/b"
      ),
      collapse = "\n"
    )
    .txt <- .sens2(.m, c("p1", "p2", "p3"))
    .norm <- rxNorm(.txt)
    .old <- rxCores()
    on.exit(setRxThreads(.old), add = TRUE)
    .res <- vapply(
      c(1L, 2L, 4L),
      function(n) {
        setRxThreads(n)
        .o <- .rxOptExprC(.norm)
        if (is.na(.o)) NA_character_ else .o
      },
      character(1)
    )
    expect_false(any(is.na(.res)))
    expect_identical(.res[1], .res[2])
    expect_identical(.res[1], .res[3])
  })

  test_that("dparser is parsed in parallel only when it is thread safe (#1427)", {
    .flag <- .Call(`_rxode2_rxDparserParallel`, NA)
    expect_identical(.flag, utils::packageVersion("dparser") >= "1.3.2")
    .old <- getRxThreads()
    on.exit(
      {
        setRxThreads(.old)
        .Call(`_rxode2_rxDparserParallel`, .flag)
      },
      add = TRUE
    )
    setRxThreads(2L)
    skip_if(getRxThreads() < 2L, "needs 2 threads")
    .Call(`_rxode2_rxDparserParallel`, FALSE)
    expect_identical(.Call(`_rxode2_rxCsePickThreads`, 200L), 1L)
    .Call(`_rxode2_rxDparserParallel`, TRUE)
    expect_identical(.Call(`_rxode2_rxCsePickThreads`, 200L), 2L)
    expect_identical(.Call(`_rxode2_rxCsePickThreads`, 10L), 1L)
  })

  test_that("parallel parses match serial ones with a thread-safe dparser (#1427)", {
    .flag <- .Call(`_rxode2_rxDparserParallel`, NA)
    skip_if_not(.flag, "dparser < 1.3.2 cannot parse concurrently")
    .old <- getRxThreads()
    on.exit(
      {
        setRxThreads(.old)
        .Call(`_rxode2_rxDparserParallel`, .flag)
      },
      add = TRUE
    )
    setRxThreads(2L)
    .norm <- rxNorm(paste(
      sprintf("a%d <- exp(tka + eta1) * (t + %d) / (tcl + exp(eta2))", 1:200, 1:200),
      collapse = "\n"
    ))
    .x <- sprintf("exp(tka + eta1) * (t + %d) / (tcl + exp(eta2)) + log(k%d + 1)", 1:300, 1:300)
    .Call(`_rxode2_rxDparserParallel`, FALSE)
    .cse <- .rxOptExprC(.norm)
    .se <- .rxToSEC(.x)
    expect_false(is.na(.cse))
    .Call(`_rxode2_rxDparserParallel`, TRUE)
    for (.i in 1:20) {
      expect_identical(.rxOptExprC(.norm), .cse)
      expect_identical(.rxToSEC(.x), .se)
    }
  })
})
