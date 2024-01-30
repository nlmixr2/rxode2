if (rxode2parse::.linCmtSens()) {
  test_that("test $modelName", {
    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- c(-Inf, 0.993251773010283, 4.60517018598809)
        tv <- 3.45
        label("log V")
        add.sd <- c(0, 0.7)
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    f <- rxode2(one.cmt)

    expect_equal(f$modelName, "one.cmt")

    f <- one.cmt()

    expect_equal(f$modelName, "one.cmt")
  })
}
