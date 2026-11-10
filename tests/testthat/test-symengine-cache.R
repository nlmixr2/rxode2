rxTest({
  # rxToSE()/rxFromSE() remember their translations (src/seCache.h).  A cached
  # answer must be the answer, and anything the answer depends on must clear
  # the cache.

  .stats <- function() .Call(`_rxode2_seStatsR`, FALSE)

  test_that("a cached translation is the translation", {
    rxSymEngineCacheReset()
    .s <- "exp(-THETA_1_)*log(1.0 + exp(ETA_1_))*Derivative(abs0(a), a)"
    .h0 <- .stats()$fromSEhits
    .a <- rxFromSE(.s)
    .b <- rxFromSE(.s)
    expect_identical(.a, .b)
    expect_equal(.stats()$fromSEhits, .h0 + 1)
    withr::with_options(list(rxode2.symengineCache = FALSE), {
      expect_identical(rxFromSE(.s), .a)
    })
    withr::with_options(list(rxode2.symengineC = FALSE), {
      expect_identical(rxFromSE(.s), .a)
    })
  })

  test_that("an R-walker translation is cached as well", {
    rxSymEngineCacheReset()
    # a tuple Subs is not parsable as is; the R side rewrites it first
    .s <- "Subs(Derivative(atan2(rx_xi_1, rx_xi_2), rx_xi_1, rx_xi_2), (rx_xi_1, rx_xi_2), (a, b))"
    .a <- try(rxFromSE(.s), silent = TRUE)
    skip_if(inherits(.a, "try-error"))
    .h0 <- .stats()$fromSEhits
    expect_identical(rxFromSE(.s), .a)
    expect_equal(.stats()$fromSEhits, .h0 + 1)
  })

  test_that("the unknown-derivative mode is part of the key", {
    rxSymEngineCacheReset()
    # registered, but with no derivative: the finite-difference fallback
    .s <- "Derivative(logspace_add(a, b), a)"
    .f <- rxFromSE(.s, unknownDerivatives = "forward")
    .c <- rxFromSE(.s, unknownDerivatives = "central")
    expect_false(identical(.f, .c))
    expect_identical(rxFromSE(.s, unknownDerivatives = "forward"), .f)
    expect_error(rxFromSE(.s, unknownDerivatives = "error"))
  })

  test_that("changing the derivative or user-function table clears the cache", {
    rxSymEngineCacheReset()
    .s <- "Derivative(rxCacheF(a), a)"
    suppressMessages(rxFun("rxCacheF", "a", "double rxCacheF(double a) {return a;}"))
    on.exit(suppressMessages(suppressWarnings(rxRmFun("rxCacheF"))), add = TRUE)
    expect_error(rxFromSE(.s, unknownDerivatives = "error"))
    rxD("rxCacheF", list(function(a) paste0("rxCacheD1(", a, ")")))
    expect_identical(rxFromSE(.s, unknownDerivatives = "error"), "rxCacheD1(a)")
    suppressWarnings(rxD("rxCacheF", list(function(a) paste0("rxCacheD2(", a, ")"))))
    expect_identical(rxFromSE(.s, unknownDerivatives = "error"), "rxCacheD2(a)")
  })

  test_that("an answer that depends on the caller's R functions is not cached", {
    rxSymEngineCacheReset()
    # the decline code R keys on is the one C reports for an unknown function
    expect_identical(attr(.rxFromSEC("rxNoSuchFn(a)"), "why"), .rxSEwhyUnknownFn)
    expect_identical(attr(.rxToSEC("rxNoSuchFn(a)"), "why"), .rxSEwhyUnknownFn)
    rxCacheG <- function(x, y) 3L
    expect_equal(rxToSE("rxCacheG(x,y)"), "rxCacheG(x, y)")
    expect_error(rxFromSE("Derivative(rxCacheG(a,b),a)"), NA)
    rxCacheG <- function(x, ...) x
    expect_error(rxToSE("rxCacheG(x,y)"))
    expect_error(rxFromSE("Derivative(rxCacheG(a,b),a)"))
    # the user function comes AFTER something else C declines (round(1.5)
    # folds in R only), so the decline reason alone does not reveal it
    rxSymEngineCacheReset()
    rxCacheG <- function(x, y) 3
    .s <- "a*round(1.5)+rxCacheG(a,b)"
    expect_equal(rxFromSE(.s), "a*2+rxCacheG(a, b)")
    rxCacheG <- function(x, ...) x
    expect_error(rxFromSE(.s))
  })

  test_that("options(scipen=) is followed, and is part of the cache key", {
    rxSymEngineCacheReset()
    expect_equal(rxFromSE("a*100000"), "a*1e+05")
    withr::with_options(list(scipen = 999), {
      expect_equal(rxFromSE("a*100000"), "a*100000")
      expect_equal(rxToSE("a*100000"), "a*100000")
    })
    expect_equal(rxFromSE("a*100000"), "a*1e+05")
  })

  test_that("a reset or a new session empties the cache", {
    rxFromSE("exp(a)*b")
    expect_gt(.stats()$entries, 0)
    rxSymEngineCacheReset()
    expect_equal(.stats()$entries, 0)
    # rxS() starts a new session: what was cached before it is gone (rxS()
    # itself may cache its own translations)
    rxFromSE("exp(zzCache)*b")
    .h0 <- .stats()$fromSEhits
    rxS(rxode2("x = a*b"))
    rxFromSE("exp(zzCache)*b")
    expect_equal(.stats()$fromSEhits, .h0)
  })

  test_that("the byte cap empties the cache", {
    rxSymEngineCacheReset()
    withr::with_options(list(rxode2.symengineCacheMB = 1L), {
      .big <- paste(rep("exp(a)", 30000), collapse = "*")
      for (.i in 1:12) {
        .t <- paste0(.big, "*b", .i)
        rxFromSE(.t)
      }
      expect_lt(.stats()$bytes, 1024 * 1024)
    })
  })

  test_that("a large batch gives the same answer serial and threaded, cached or not", {
    .x <- sprintf("exp(-THETA_%d_)*log(1.0 + a%d^2) + Rx_pow_di(b, %d)/c", 1:400, 1:400, 1:400)
    rxSymEngineCacheReset()
    .t1 <- withr::with_options(list(rxode2.symengineCache = FALSE), {
      .old <- getRxThreads()
      on.exit(setRxThreads(.old))
      setRxThreads(1L)
      .rxFromSEvec(.x)
    })
    .tN <- .rxFromSEvec(.x)
    .tC <- .rxFromSEvec(.x)
    expect_identical(.tN, .t1)
    expect_identical(.tC, .t1)
  })
})
