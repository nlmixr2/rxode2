rxTest({

  test_that("test +dv()", {

    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        cp <- linCmt()
        cp ~ lnorm(add.sd)
      })
    }
    one.cmt <- one.cmt()

    v <- .handleSingleErrTypeNormOrTFoceiBase(one.cmt, one.cmt$predDf[1,])

    expect_true(identical(v[[6]],
                          quote(rx_pred_ ~ log(rx_pred_f_))))

    expect_true(identical(v[[1]],
                          quote(rx_yj_ ~ 3)))

    one.cmt <- one.cmt |>
      model(cp ~ lnorm(add.sd) + dv())

    v <- .handleSingleErrTypeNormOrTFoceiBase(one.cmt, one.cmt$predDf[1,])

    expect_true(identical(v[[6]],
                          quote(rx_pred_ ~ rx_pred_f_)))

    expect_true(identical(v[[1]],
                          quote(rx_yj_ ~ 3)))

    one.cmt <- one.cmt |>
      model(cp ~ logitNorm(add.sd))

    v <- .handleSingleErrTypeNormOrTFoceiBase(one.cmt, one.cmt$predDf[1,])

    expect_true(identical(v[[1]],
                          quote(rx_yj_ ~ 4)))

    expect_true(identical(v[[6]],
                          quote(rx_pred_ ~ rxTBS(rx_pred_f_, rx_lambda_, rx_yj_, rx_low_, rx_hi_))))


    one.cmt <- one.cmt |>
      model(cp ~ logitNorm(add.sd) + dv())

    v <- .handleSingleErrTypeNormOrTFoceiBase(one.cmt, one.cmt$predDf[1,])

    expect_true(identical(v[[6]],
                          quote(rx_pred_ ~ rx_pred_f_)))

    expect_true(identical(v[[1]],
                          quote(rx_yj_ ~ 4)))

  })


})
