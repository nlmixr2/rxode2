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
    # (nlmixr2/nlmixr2est#1437).
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
    # a constant coefficient is a valid mu2 term (nlmixr2/nlmixr2est#1433 would
    # wrongly drop it, so that guard is not needed with this rule)
    expect_equal(.mu2("cl.wt*log(70)"), "4.24849524204936")
    # mu3 (nlmixr2est evaluates these through symengine): model variables that
    # resolve to covariates and constants; see nlmixr2/nlmixr2est#1437
    expect_equal(.mu2("cl.wt*lwt", "lwt <- log(WT/70)"), "lwt")
    expect_equal(.mu2("cl.wt*lwt2", "lwt <- log(WT/70); lwt2 <- lwt*SEX"), "lwt2")
    expect_equal(.mu2("cl.wt*lc", "lc <- 3"), "lc")
    # a mu3 model variable is not resolved here: whether it reaches a theta,
    # eta or state is found when nlmixr2 evaluates it in the data
    expect_equal(.mu2("cl.wt*v"), "v")
    expect_equal(.mu2("cl.wt*lif", "if (tv > 1) { lif <- WT } else { lif <- 70 }"), "lif")
    # theta-free derivative but not a product: rewriting to
    # nlmixrMuDerCov*cl.wt would drop the `+ log(WT/70)` remainder
    expect_length(.mu2("(cl.wt + 1)*log(WT/70)"), 0L)
    expect_length(.mu2("(cl.wt + 1)*lwt", "lwt <- log(WT/70)"), 0L)
  })

  test_that("an if/else on covariates keeps mu3/mu4 covariate terms (#1770)", {
    .mu <- function(pre, cl, v) {
      .f <- eval(parse(
        text = sprintf(
          "function() {
          ini({ tka <- 0.45; tcl <- -1; tv <- 3.45; b <- 0; bv <- 0; add.sd <- 0.3; eta.cl ~ 0.1; eta.v ~ 0.1 })
          model({
            wt70 <- WT / 70
            %s
            ka <- exp(tka)
            cl <- exp(tcl + %s + eta.cl)
            v <- exp(tv + %s + eta.v)
            linCmt() ~ add(add.sd) })
        }",
          pre,
          cl,
          v
        )
      ))
      .df <- .f()$mu2RefCovariateReplaceDataFrame
      setNames(.df$covariate, .df$covariateParameter)
    }
    # mu3 unrelated to the branch, mu3 set in the branch
    expect_equal(
      .mu("if (wt70 > 1) { big <- 1 } else { big <- 0 }", "log(wt70)*b", "big*bv"),
      c(b = "log(wt70)", bv = "big")
    )
    # mu4: a string set in a branch on covariates
    expect_equal(
      .mu(
        "if (wt70 > 1) { wt70s <- \"larger\" } else { wt70s <- \"smaller\" }",
        "log(wt70)*b",
        "(wt70s == \"larger\")*bv"
      ),
      c(b = "log(wt70)", bv = "(wt70s == \"larger\")")
    )
    # ifelse() and a string comparison on a data covariate
    expect_equal(
      .mu("big <- ifelse(wt70 > 1, 1, 0)", "log(wt70)*b", "big*bv"),
      c(b = "log(wt70)", bv = "big")
    )
    expect_equal(
      .mu("if (SEX == \"F\") { big <- 1 } else { big <- 0 }", "log(wt70)*b", "big*bv"),
      c(b = "log(wt70)", bv = "big")
    )
  })

  # A recorded covariate as nlmixr2 evaluates it in the data: resolved through
  # the pruned model in symengine, with strings as their levels.
  .muCovResolve <- function(ui, cov) {
    .sa <- rxModelVars(ui)$strAssign
    .lev <- function(x) {
      if (!is.call(x)) {
        return(x)
      }
      if (
        (identical(x[[1]], quote(`==`)) || identical(x[[1]], quote(`!=`))) &&
          is.name(x[[2]]) &&
          is.character(x[[3]]) &&
          !is.null(.sa[[as.character(x[[2]])]])
      ) {
        x[[3]] <- as.numeric(which(.sa[[as.character(x[[2]])]] == x[[3]]))
        return(x)
      }
      as.call(c(x[[1]], lapply(x[-1], .lev)))
    }
    .se <- eval(str2lang(paste0("rxToSE(", deparse1(.lev(str2lang(cov))), ", NULL)")))
    .s <- rxS(rxPrune(ui))
    # get() is masked once symengine is loaded; capture via assign()
    .b <- eval(parse(text = paste0('assign("rxdummyMu", with(.s, ', .se, "), envir = .s)")))
    rxFromSE(.b)
  }

  .muResolveMod <- function(pre, v) {
    .f <- eval(parse(
      text = sprintf(
        "function() {
        ini({ tka <- 0.45; tcl <- -1; tv <- 3.45; b <- 0; bv <- 0; add.sd <- 0.3; eta.cl ~ 0.1; eta.v ~ 0.1 })
        model({
          wt70 <- WT / 70
          %s
          ka <- exp(tka)
          cl <- exp(tcl + log(wt70)*b + eta.cl)
          v <- exp(tv + %s + eta.v)
          d/dt(central) <- -cl / v * central
          cp <- central / v
          cp ~ add(add.sd) })
      }",
        pre,
        v
      )
    ))
    .f()
  }

  test_that("recorded mu3/mu4 covariates resolve to the data through rxS(rxPrune()) (#1770)", {
    .d <- data.frame(WT = c(50, 100))
    .eval <- function(ui) {
      .df <- ui$mu2RefCovariateReplaceDataFrame
      setNames(
        lapply(.df$covariate, function(cov) {
          .e <- .muCovResolve(ui, cov)
          expect_true(all(all.vars(str2lang(.e)) == "WT"), info = .e)
          as.numeric(with(.d, eval(str2lang(.e))))
        }),
        .df$covariateParameter
      )
    }
    # mu3 unrelated to the branch and mu4 set in it (the issue's model)
    .v <- .eval(.muResolveMod(
      "if (wt70 > 1) { wt70s <- \"larger\" } else { wt70s <- \"smaller\" }",
      "(wt70s == \"larger\")*bv"
    ))
    expect_equal(.v, list(b = log(.d$WT / 70), bv = c(0, 1)))
    # mu3 set in the branch
    .v <- .eval(.muResolveMod("if (wt70 > 1) { big <- 1 } else { big <- 0 }", "big*bv"))
    expect_equal(.v, list(b = log(.d$WT / 70), bv = c(0, 1)))
  })

  test_that("a mu3 covariate that reaches a theta or eta resolves to it, so nlmixr2 rejects it in the data", {
    .ui <- .muResolveMod("if (tv > 1) { big <- WT } else { big <- 70 }", "big*bv")
    .df <- .ui$mu2RefCovariateReplaceDataFrame
    expect_equal(.df$covariate, c("log(wt70)", "big"))
    expect_true("tv" %in% all.vars(str2lang(.muCovResolve(.ui, "big"))))
    .ui <- .muResolveMod("lv <- log(WT) + eta.cl", "lv*bv")
    expect_true("eta.cl" %in% all.vars(str2lang(.muCovResolve(.ui, "lv"))))
  })
})
