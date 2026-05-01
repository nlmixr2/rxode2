rxTest({

  test_that("plogis ui translation normalizes to expit expressions", {
    f <- function() {
      model({
        a <- plogis(x, scale = 10 + 1, location = a)
        b <- plogis(x, lower.tail = FALSE)
        c <- plogis(logitx, lower.tail = FALSE, log.p = TRUE)
      })
    }

    expect_equal(
      f() |> modelExtract(),
      c(
        "a <- expit((x - a)/11, 0, 1)",
        "b <- expit(-x, 0, 1)",
        "c <- log(expit(-logitx, 0, 1))"
      )
    )
  })

  test_that("plogis string parsing lowers to expit-compatible models", {
    mod <- function() {
      model({
        a=plogis(time, 1, 2, 0, 0)
        b=plogis(time, 1, 2, 1, 1)
      })
    }
    et <- et(seq(0, 8, length.out = 9))
    s <- suppressWarnings(rxSolve(mod, et))
    expect_equal(s$a, stats::plogis(s$time, location = 1, scale = 2, lower.tail = FALSE, log.p = FALSE))
    expect_equal(s$b, stats::plogis(s$time, location = 1, scale = 2, lower.tail = TRUE, log.p = TRUE))
  })

})
