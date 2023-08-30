rxTest({

  ocmt <- function() {
    ini({
      tka <- exp(0.45); label("Absorption rate")
      tcl <- exp(1); label("Clearance")
      tv <- exp(3.45); label("Volume of distribution")
      add.sd <- 0.7; label("Additive residual error")
    })
    model({
      ka <- tka
      cl <- tcl
      v <- tv
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }

  idr <- function() {
    ini({
      tkin <- log(1)
      tkout <- log(1)
      tic50 <- log(10)
      gamma <- fix(1)
      idr.sd <- 1
    })
    model({
      kin <- exp(tkin)
      kout <- exp(tkout)
      ic50 <- exp(tic50)
      d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
      eff ~ add(idr.sd)
    })
  }

  test_that("rxAppendModel", {
    expect_s3_class(
      object = rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr),
      class = "rxUi"
    )
    expect_error(
      object = rxAppendModel(ocmt, idr),
      regexp = "the first model does not have variables that are used by the second model"
    )
    expect_s3_class(
      object = rxAppendModel(ocmt, idr, requireShared = FALSE),
      class = "rxUi"
    )
  })
})
