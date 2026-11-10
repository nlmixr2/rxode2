rxTest({
  # dop853 forms its stages with plain loops below four states and two
  # elements a pass, with a scalar tail, from four states on; a linear chain
  # a -> b -> c -> ... of 1 to 9 states covers both forms, odd and even, and
  # must match the exact solution
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

  test_that("dop853 matches a linear chain of 1 to 9 states", {
    .time <- c(0.1, 0.5, 1, 2, 5, 10)
    .rates <- c(1, 0.7, 0.5, 0.3, 0.2, 0.9, 0.4, 0.6, 0.25)
    for (.n in 1:9) {
      .m <- .chainModel(.n)
      .k <- .rates[seq_len(.n)]
      names(.k) <- paste0("k", seq_len(.n))
      .y0 <- c(100, rep(0, .n - 1))
      .ev <- et(amt = 100, cmt = "s1") |> et(.time)
      .s <- rxSolve(.m, .ev, params = .k, method = "dop853", atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
      .ex <- .chainExact(.k, .s$time, .y0)
      for (.i in seq_len(.n)) {
        expect_equal(.s[[paste0("s", .i)]], .ex[, .i], tolerance = 1e-9, label = sprintf("n = %d, s%d", .n, .i))
      }
    }
  })

  test_that("dop853's paired stage updates match the plain loop bit for bit", {
    # every stage element is the same expression as in the plain loop, so
    # forcing the plain loop for every size must not change a single bit
    .rates <- c(1, 0.7, 0.5, 0.3, 0.2, 0.9, 0.4, 0.6, 0.25)
    .ev <- et(amt = 100, cmt = "s1", ii = 6, addl = 3) |> et(c(0.1, 0.5, 1, 2, 5, 10, 20, 30))
    for (.n in c(4L, 5L, 8L, 9L, 20L)) {
      .m <- .chainModel(.n)
      .k <- rep(.rates, length.out = .n) * (1 + seq_len(.n) / 100)
      names(.k) <- paste0("k", seq_len(.n))
      .paired <- rxSolve(.m, .ev, params = .k, method = "dop853", returnType = "data.frame")
      .old <- .rxDop853PairsMin(.Machine$integer.max)
      .plain <- tryCatch(
        rxSolve(.m, .ev, params = .k, method = "dop853", returnType = "data.frame"),
        finally = .rxDop853PairsMin(.old)
      )
      expect_identical(.paired, .plain, label = sprintf("n = %d", .n))
    }
  })
})
