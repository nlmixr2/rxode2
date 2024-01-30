test_that("mu2 referencing", {

  f <- function() {
    ini({
      tka <- 0.45644793862596
      label("Log Ka")
      tcl <- 1.0158342238421
      label("Log Cl")
      tv <- 3.45168722356501
      label("Log V")
      add.sd <- c(0, 0.693865857699365)
      wt_cl <- fix(0.75)
      eta.ka ~ 0.413242125585966
      eta.cl ~ 0.0700506402691836
      eta.v ~ 0.0190586604340742
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl + wt_cl * log(WT/70.5))
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }

  f <- f()

  model.BASE <- function() {
    ini({
      tka <- log(1) # Log Ka
      tcl <- log(2) # Log Cl
      tv <- log(20)    # Log V
      eta.ka ~ 0.5
      eta.cl ~ 0.1
      eta.v ~ 0.1
      add.sd <- 0.5
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }


  m2 <- model.BASE %>%
    model(cl <- exp(tcl + eta.cl + wt_cl*log(WT/70.5))) %>%
    ini(wt_cl <- fix(0.75))

  expect_equal(f$mu2RefCovariateReplaceDataFrame,
               m2$mu2RefCovariateReplaceDataFrame)

})
