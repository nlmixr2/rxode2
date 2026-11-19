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

  test_that("a theta-affine term with a theta-free remainder is not a mu2 covariate", {
    # 0.75*log(V1/exp(tV1)) has d/dtV1 = -0.75 (no theta), but it is not
    # -0.75*tV1: it also carries 0.75*log(V1).  Recording it as a mu2 term made
    # nlmixr2 rewrite it to `nlmixrMuDerCov*tV1`, silently dropping the V1
    # (weight and eta.V1) dependence of CL under SAEM / mu-referenced methods.
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
})
