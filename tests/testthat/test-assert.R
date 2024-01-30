if (rxode2parse::.linCmtSens()) {
  test_that("assertRxUiRandomOnIdOnly", {
    one.cmt <- function() {
      ini({
        tka <- 0.45; label("Ka")
        tcl <- log(c(0, 2.7, 100)); label("Cl")
        tv <- 3.45; label("V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    expect_equal(
      assertRxUiRandomOnIdOnly(one.cmt),
      as.rxUi(one.cmt)
    )
  })
}
