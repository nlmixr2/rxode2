rxTest({
  # liblsoda updates its state vectors two elements at a time with a scalar
  # tail, so models with an odd and an even number of states, solved by both
  # the non-stiff (functional iteration) and the stiff (Newton) corrector,
  # must match the exact solution of a linear chain a -> b -> c -> ...
  .chainExact <- function(k, time, y0) {
    .a <- matrix(0, length(k), length(k))
    for (.i in seq_along(k)) {
      .a[.i, .i] <- -k[.i]
      if (.i > 1) .a[.i, .i - 1] <- k[.i - 1]
    }
    .e <- eigen(.a)
    .v <- .e$vectors
    .c <- solve(.v, y0)
    matrix(
      vapply(time, function(.t) Re(drop(.v %*% (.c * exp(.e$values * .t)))), numeric(length(k))),
      ncol = length(k),
      byrow = TRUE
    )
  }

  .chainModel <- function(n) {
    .lines <- vapply(
      seq_len(n),
      function(.i) {
        if (.i == 1) {
          "d/dt(s1) <- -k1 * s1"
        } else {
          sprintf("d/dt(s%d) <- k%d * s%d - k%d * s%d", .i, .i - 1, .i - 1, .i, .i)
        }
      },
      character(1)
    )
    rxode2(paste(.lines, collapse = "\n"))
  }

  test_that("liblsoda matches a linear chain of 1 to 5 states, non-stiff and stiff", {
    .time <- c(0.1, 0.5, 1, 2, 5, 10)
    for (.n in 1:5) {
      .m <- .chainModel(.n)
      for (.stiff in c(FALSE, TRUE)) {
        # distinct rates; the stiff chain adds rates up to 1e4 so liblsoda
        # switches to its BDF method and the Newton corrector
        .k <- if (.stiff) c(1, 1e4, 0.5, 2e3, 0.3)[seq_len(.n)] else c(1, 0.7, 0.5, 0.3, 0.2)[seq_len(.n)]
        names(.k) <- paste0("k", seq_len(.n))
        .y0 <- c(100, rep(0, .n - 1))
        .ev <- et(amt = 100, cmt = "s1") |> et(.time)
        .s <- rxSolve(.m, .ev, params = .k, method = "liblsoda", atol = 1e-12, rtol = 1e-10, returnType = "data.frame")
        .ex <- .chainExact(.k, .s$time, .y0)
        for (.i in seq_len(.n)) {
          expect_equal(
            .s[[paste0("s", .i)]],
            .ex[, .i],
            tolerance = 1e-7,
            label = sprintf("n = %d, stiff = %s, s%d", .n, .stiff, .i)
          )
        }
      }
    }
  })
})
