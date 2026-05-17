rxTest({
  .isAltrep <- function(x) {
    rxIs(x, "altrep")
  }

  test_that("repeated simulation event columns use ALTREP repetition", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))

    p <- data.frame(id = 1:2, ka = c(1, 1), cl = c(1, 1), v = c(10, 10))
    out <- rxSolve(mod, p, ev,
                   addDosing = TRUE,
                   returnType = "data.frame")

    expect_true(.isAltrep(out$time))
    expect_true(.isAltrep(out$evid))
    expect_true(.isAltrep(out$amt))
  })

  test_that("runtime event mutation fallback keeps event columns materialized", {
    mod <- rxode2({
      d/dt(x) <- -x
      if (t < 1) evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    ev <- et(amt = 1, time = 0) |> et(seq(0, 1, by = 0.1))

    p <- data.frame(id = 1:2, x = c(1, 1))
    out <- rxSolve(mod, p, ev,
                   addDosing = TRUE, returnType = "data.frame")

    expect_false(.isAltrep(out$time))
  })
})
