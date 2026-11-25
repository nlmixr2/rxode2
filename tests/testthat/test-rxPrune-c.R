rxTest({
  .pruneC <- function(expr, strAssign = list(), capture = FALSE) {
    .Call(`_rxode2_rxPruneC`, expr, strAssign, capture)
  }

  test_that("the C rxPrune() walker matches the R walker on the model set", {
    for (.n in names(.rxPruneModelSet())) {
      .m <- .rxPruneModelSet()[[.n]]
      .p <- .rxPruneParse(.m)
      .c <- .pruneC(.p$expr, .p$strAssign, TRUE)
      expect_false(is.null(.c), info = .n)
      expect_identical(.c, .rxPruneR(.m), info = .n)
      expect_identical(rxPrune(.m), .c, info = .n)
    }
  })

  test_that("the C walker matches .rxPrune() for string levels and literals", {
    .cases <- list(
      list("{a<-'a'; b<-1}", list(a = "a")),
      list("{a<-'a'; b<-1}", list(a = c("b", "a"))),
      list("{a<-'a'; b<-1}", list(c = c("b", "a"))),
      list("{b <- (a == 'a')}", list(a = c("b", "a"))),
      list("{b <- ('a' == a)}", list(a = c("b", "a"))),
      list("{b <- (a != 'a')}", list(a = "a")),
      list("{b <- (a == 'z')}", list(a = c("b", "a"))),
      list("{b <- 'x\\\"y\\n'; c <- 1e5 + 1e-20 + 0.1 + TRUE}", list()),
      list("{b <- THETA[1] + eta[2]; c <- !(a > 1) & a <= 2 || a >= 3 && a < 4}", list()),
      list("{if (a > 1) {b <- 1} else if (a > 2) {b <- 2; c <- 1} else {c <- 3}; d <- -b + +c}", list())
    )
    for (.c in .cases) {
      .e <- str2lang(.c[[1]])
      expect_identical(.pruneC(.e, .c[[2]]), .rxPrune(.e, envir = new.env(), strAssign = .c[[2]]), info = .c[[1]])
    }
  })

  test_that("rxPrune() falls back to the R walker on constructs C does not handle", {
    expect_null(.pruneC(quote({
      f(, 1)
    })))
    expect_null(.pruneC(
      quote({
      a <- f(1)(2)
    }),
      capture = TRUE
    ))
  })
})
