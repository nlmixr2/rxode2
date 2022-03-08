test_that("rename for ui makes sense", {

  ocmt <- function() {
    ini({
      tka <- exp(0.45) # Ka
      tcl <- exp(1) # Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- exp(3.45) # log V
      ## the label("Label name") works with all models
      add.sd <- 0.7
    })
    model({
      ka <- tka
      cl <- tcl
      v <- tv
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }

})
