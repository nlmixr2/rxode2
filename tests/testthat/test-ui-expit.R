if (!.Call(`_rxode2_isIntel`)) {
  test_that("markov llik", {

    markov <- function() {
      ini({
        logitp02 <- logit(0.2)
        label("Probablity of transition from 0 to 2")
        logitp20 <- logit(0.2)
        label("Probablity of transition from 2 to 0")
      })
      model({
        p02 <- expit(logitp02)
        p00 <- 1 - p02
        p20 <- expit(logitp20)
        p22 <- 1 - p20
        p <-
          p02*(PDV == 0 & DV == 2) +
          p00*(PDV == 0 & DV == 0) +
          p20*(PDV == 2 & DV == 0) +
          p22*(PDV == 2 & DV == 2)
        ll(err) ~ log(p)
      })
    }

    f <- rxode2(markov)

    expect_true(all(f$muRefCurEval$low == 0))
    expect_true(all(f$muRefCurEval$hi == 1))

  })
}
