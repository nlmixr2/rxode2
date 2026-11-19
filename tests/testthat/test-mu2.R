rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
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

      m2 <- model.BASE |>
        model(cl <- exp(tcl + eta.cl + wt_cl*log(WT/70.5))) |>
        ini(wt_cl <- fix(0.75))

      expect_equal(f$mu2RefCovariateReplaceDataFrame, m2$mu2RefCovariateReplaceDataFrame)
    })
  }

  test_that("a mu2 term that reaches a theta or eta through a model variable is not a covariate", {
    # 0.75*log(V1/exp(tV1)) has d/dtV1 = -0.75 (no theta), but V1 depends on
    # tV1 and eta.V1.  Recording it as a mu2 term made nlmixr2 rewrite it to
    # `nlmixrMuDerCov*tV1`, silently dropping the V1 dependence of CL
    # (nlmixr2/nlmixr2est#1437; guarded downstream by nlmixr2/nlmixr2est#1433).
    f <- function() {
      ini({
        tV1 <- log(40); tCL <- log(5); tV2 <- log(40); tQ <- log(3)
        wt.cl <- 0.75
        eta.CL ~ 0.1; eta.V1 ~ 0.1; eta.V2 ~ 0.1
        add.sd <- 1
      })
      model({
        V1 <- exp(tV1 + log(WT/70) + eta.V1)
        V2 <- exp(tV2 + log(WT/70) + eta.V2)
        CL <- exp(tCL + 0.75*log(V1/exp(tV1)) + wt.cl*log(WT/70) + eta.CL)
        Q <- exp(tQ + 0.75*log(V2/exp(tV2)))
        cp <- linCmt(CL, Q, V1, V2)
        cp ~ add(add.sd)
      })
    }
    f <- f()
    .df <- f$mu2RefCovariateReplaceDataFrame
    expect_false(any(grepl("exp(tV", .df$modelExpression, fixed = TRUE)))
    # the genuine theta*f(covariate) term on the same line is still detected
    expect_equal(.df$modelExpression, "wt.cl * log(WT/70)")
    expect_equal(.df$covariateParameter, "wt.cl")
  })

  test_that("mu2/mu3 terms depend only on covariates and constants, never on parameters", {
    .mu2 <- function(term, pre = "") {
      .f <- eval(parse(
        text = sprintf(
          "function() {
          ini({ tcl <- 1; cl.wt <- 0.75; tv <- 1; eta.cl ~ 0.1; eta.v ~ 0.1; add.sd <- 1 })
          model({ %s
            v <- exp(tv + eta.v)
            cl <- exp(tcl + %s + eta.cl)
            cp <- linCmt(cl, v)
            cp ~ add(add.sd) })
        }",
          pre,
          term
        )
      ))
      .f()$mu2RefCovariateReplaceDataFrame$covariate
    }
    # covariates and constants
    expect_equal(.mu2("cl.wt*log(WT/70)"), "log(0.0142857142857143 * WT)")
    expect_equal(.mu2("cl.wt*log(WT/70)*SEX"), "SEX * log(0.0142857142857143 * WT)")
    expect_equal(.mu2("cl.wt*log(70)"), "4.24849524204936")
    # mu3 (nlmixr2est evaluates these through symengine): model variables that
    # resolve to covariates and constants; see nlmixr2/nlmixr2est#1437 and #1433
    expect_equal(.mu2("cl.wt*lwt", "lwt <- log(WT/70)"), "lwt")
    expect_equal(.mu2("cl.wt*lwt2", "lwt <- log(WT/70); lwt2 <- lwt*SEX"), "lwt2")
    expect_equal(.mu2("cl.wt*lc", "lc <- 3"), "lc")
    # mu3 terms whose model variable reaches a theta or eta
    expect_length(.mu2("cl.wt*v"), 0L)
    expect_length(.mu2("cl.wt*lwv", "lwv <- log(WT/70)*v"), 0L)
    expect_length(.mu2("cl.wt*sv", "d/dt(depot) <- -depot; sv <- depot"), 0L)
    expect_length(.mu2("cl.wt*lv", "lv <- log(WT) + eta.v"), 0L)
    expect_length(.mu2("cl.wt*lwv2", "vv <- exp(tv + eta.v); lwv2 <- log(WT/70)*vv"), 0L)
    expect_length(.mu2("cl.wt*lif", "if (tv > 1) { lif <- WT } else { lif <- 70 }"), 0L)
    # theta-free derivative but not a product: rewriting to
    # nlmixrMuDerCov*cl.wt would drop the `+ log(WT/70)` remainder
    expect_length(.mu2("(cl.wt + 1)*log(WT/70)"), 0L)
    expect_length(.mu2("(cl.wt + 1)*lwt", "lwt <- log(WT/70)"), 0L)
  })
})
