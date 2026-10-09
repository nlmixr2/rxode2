rxTest({
  .rec <- new.env()
  .recordEvents <- function() {
    .rec$events <- list()
    rxEventListen("test", function(event, ...) {
      .rec$events[[length(.rec$events) + 1L]] <- list(event = event, payload = list(...))
    })
    withr::defer(rxEventUnlisten("test"), envir = parent.frame())
  }
  .names <- function() vapply(.rec$events, function(e) e$event, character(1))

  test_that("listen, emit, unlisten; no listeners is a no-op", {
    .recordEvents()
    expect_true("test" %in% rxEventListeners())
    rxEventEmit("a", x = 1)
    expect_equal(.names(), "a")
    expect_equal(.rec$events[[1]]$payload$x, 1)
    rxEventUnlisten("test")
    rxEventEmit("b")
    expect_equal(.names(), "a")
  })

  test_that("re-registering an id replaces the listener", {
    .n <- 0
    rxEventListen("r", function(event, ...) .n <<- .n + 1)
    rxEventListen("r", function(event, ...) .n <<- .n + 10)
    rxEventEmit("x")
    expect_equal(.n, 10)
    rxEventUnlisten("r")
  })

  test_that("only the outermost scope delivers", {
    .recordEvents()
    rxEventScope(rxEventEmit("inner"))
    expect_length(.rec$events, 0L)
    ## inner exit-emit is dropped (depth 1), outer exit-emit is delivered
    .rxEventEnter()
    rxEventScope(rxEventEmit("never"))
    .rxEventEnter(); .rxEventExit("alsoNever")
    .rxEventExit("first")
    expect_equal(.names(), "first")
    expect_equal(rxEventDepth(), 0L)
  })

  test_that("depth is restored on error and the env var is cleared", {
    expect_error(rxEventScope(stop("boom")), "boom")
    expect_equal(rxEventDepth(), 0L)
    expect_identical(Sys.getenv("RXODE2_EVENT_DEPTH"), "")
    rxEventScope(expect_identical(Sys.getenv("RXODE2_EVENT_DEPTH"), "1"))
  })

  test_that("a failing listener warns and the others still run", {
    .recordEvents()
    rxEventListen("bad", function(event, ...) stop("listener boom"))
    withr::defer(rxEventUnlisten("bad"))
    expect_warning(rxEventEmit("z"), "listener boom")
    expect_equal(.names(), "z")
  })

  test_that("events emitted by a listener are dropped; seq counts deliveries", {
    .recordEvents()
    rxEventListen("echo", function(event, ...) rxEventEmit("echoed"))
    withr::defer(rxEventUnlisten("echo"))
    .s0 <- rxEventSeq()
    rxEventEmit("once")
    expect_equal(.names(), "once")
    expect_equal(rxEventSeq(), .s0 + 1L)
    rxEventScope(rxEventEmit("dropped"))
    expect_equal(rxEventSeq(), .s0 + 1L)
  })

  test_that(".rxEventCall replaces the head and inlined values", {
    .c <- .rxEventCall(as.call(list(function(x) x, list(1, 2), quote(ev), 3)), fun = "rxSolve")
    expect_identical(.c[[1]], as.name("rxSolve"))
    expect_identical(.c[[2]], as.name("<value>"))
    expect_identical(.c[[3]], quote(ev))
    expect_identical(.c[[4]], 3)
    .c2 <- .rxEventCall(as.call(list(function(x) x, 1)))
    expect_identical(.c2[[1]], as.name("<fun>"))
    expect_identical(.rxEventCall(quote(rxode2::rxSolve(a))), quote(rxode2::rxSolve(a)))
    ## many inlined scalars (a spread control list) are all replaced
    .many <- as.call(c(list(as.name("rxSolve"), quote(fit)), as.list(setNames(1:8, letters[1:8]))))
    expect_identical(.rxEventCall(.many), quote(rxSolve(fit, `<...>`)))
  })

  test_that("rxSolve emits exactly once; rxControl and wrapped solves emit nothing", {
    mod <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    ev <- et(amt = 100) |> et(0:4)
    .p <- c(ka = 1, cl = 1, v = 10)
    .recordEvents()
    rxControl()
    expect_length(.rec$events, 0L)
    s <- rxSolve(mod, .p, ev)
    expect_equal(.names(), "solveComplete")
    .pl <- .rec$events[[1]]$payload
    expect_s3_class(.pl$result, "rxSolve")
    expect_identical(.pl$kind, "rxSolve")
    expect_identical(.pl$call[[1]], as.name("rxSolve"))
    .f <- function() rxEventScope(rxSolve(mod, .p, ev))
    .f()
    expect_length(.rec$events, 1L)
    expect_error(rxSolve(mod, c(ka = 1), ev))
    expect_length(.rec$events, 1L)
    expect_equal(rxEventDepth(), 0L)
    ## do.call inlines values: the recorded call stays small
    do.call(rxSolve, list(mod, .p, ev))
    expect_length(.rec$events, 2L)
    expect_lt(nchar(paste(deparse(.rec$events[[2]]$payload$call), collapse = "")), 200)
  })

  test_that("a child process started inside a scope inherits the depth", {
    skip_if_not_installed("callr")
    skip_on_cran()
    .lib <- .libPaths()
    .d <- rxEventScope(callr::r(function(lib) {
      .libPaths(lib)
      rxode2::rxEventDepth()
    }, list(.lib)))
    expect_gte(.d, 1L)
    .d0 <- callr::r(function(lib) {
      .libPaths(lib)
      rxode2::rxEventDepth()
    }, list(.lib))
    expect_equal(.d0, 0L)
  })
})
