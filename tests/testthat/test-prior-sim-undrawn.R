rxTest({
  ## A covariance that takes precedence over the priors says which prior
  ## entries it will not draw (#1502)

  test_that("a meta thetaMat that misses a prior parameter says so (#1502)", {
    skipIfOldLotri()

    .ui <- .priorMapMod(meta = TRUE)
    assign("thetaMat", lotri::lotri(tcl ~ 0.02), envir = .ui$meta)
    expect_warning(
      rxSolve(.ui, .priorMapEv(), nSub = 2, nStud = 5),
      "does not draw 'tka'"
    )
  })

  test_that("a winning thetaMat says which omega element priors it leaves undrawn (#1502)", {
    skipIfOldLotri()

    .joint <- function() {
      rxode2(function() {
        ini({
          tcl <- 1.0
          add.sd <- 0.7
          eta.cl ~ 0.3
          tcl + om.eta.cl ~ c(0.02, 0.004, 0.005)
        })
        model({
          cl <- exp(tcl + eta.cl)
          v <- 1
          linCmt() ~ add(add.sd)
        })
      })
    }
    .m <- lotri::lotri(tcl + om.eta.cl ~ c(0.02, 0.004, 0.005))
    ## the om. column is pruned unless the separation draws it
    expect_warning(
      rxSolve(.joint(), .priorMapEv(), nSub = 2, nStud = 5, thetaMat = .m),
      "does not draw 'om.eta.cl'"
    )
    expect_warning(
      rxSolve(.joint(), .priorMapEv(), nSub = 2, nStud = 5, thetaMat = lotri::lotri(tcl ~ 0.02)),
      "does not draw 'om.eta.cl'"
    )
    ## under tnpri any spelling of the element is drawn
    for (.nm in c("om.eta.cl", "eta.cl", "omega1.1")) {
      .t <- lotri::lotri(tcl ~ 0.02)
      .t <- rbind(cbind(.t, 0), c(0, 0.005))
      dimnames(.t) <- list(c("tcl", .nm), c("tcl", .nm))
      expect_no_warning(
        withr::with_seed(3, {
          .s <- rxSolve(.joint(), .priorMapEv(), nSub = 2, nStud = 50, thetaMat = .t, omegaSeparation = "tnpri")
        }),
        message = "does not draw"
      )
      ## and the element really is drawn
      .cl <- vapply(.s$omegaList, function(m) m["eta.cl", "eta.cl"], double(1))
      expect_gt(sd(.cl), 0.01)
    }
    ## the thetaMat is read by its column names
    .t <- matrix(0.02, 1, 1, dimnames = list(NULL, "tcl"))
    expect_warning(
      rxSolve(.joint(), .priorMapEv(), nSub = 2, nStud = 5, thetaMat = .t),
      "does not draw 'om.eta.cl' from"
    )
  })
})
