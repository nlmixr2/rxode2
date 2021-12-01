rxode2Test(
  {
    context("deSolve event")
    test_that("deSolve events", {
      ## Data frame event from deSolve vignette

      derivs <- rxode2({
        v1(0) <- 1
        v2(0) <- 2
        d / dt(v1) <- 0
        d / dt(v2) <- 0
      })

      eventdat <- data.frame(
        var = c("v1", "v2", "v2", "v1"),
        time = c(1, 1, 5, 9),
        value = c(1, 2, 3, 4),
        method = c("add", "mult", "rep", "add")
      )

      ett <- etTrans(eventdat, derivs, keepDosingOnly = TRUE)

      ett2 <- etTrans(et(eventdat), derivs, keepDosingOnly = TRUE)

      expect_equal(ett$EVID, ett2$EVID)
      expect_equal(ett$AMT, ett2$AMT)
      expect_equal(ett$TIME, ett2$TIME)

      e <- et(eventdat) %>%
        et(seq(0, 10, by = 0.1))



      tmp1 <- rxSolve(derivs, e)

      eventdat2 <- data.frame(
        cmt = c("v1", "v2", "v2", "v1"),
        time = c(1, 1, 5, 9),
        amt = c(1, 2, 3, 4),
        evid = c(1, 6, 5, 1)
      )

      e <- et(eventdat2) %>%
        et(seq(0, 10, by = 0.1))

      tmp3 <- rxSolve(derivs, e)

      expect_equal(as.data.frame(tmp1), as.data.frame(tmp3))
    })
  },
  test = "lvl2"
)
