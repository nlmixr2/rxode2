rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    test_that("limCmt raises error", {

      M2 <- function() {
        ini({
          tD1 <- log(0.8)
          tcl <- log(51.2)
          tv <- log(344)
          eta.D1 ~ 0.4
          eta.cl ~ 0.4
          eta.v ~ 0.4
          add.err <- 1
          prop.err <- 0.2
        })
        model({
          D1 <- exp(tD1 + eta.D1)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          limCmt() ~ add(add.err) + prop(prop.err)
        })
      }

      expect_error(rxode2(M2), "linCmt")
    })
  }
})
