rxTest({
  # A piped ui must be exactly the ui built from scratch from its own model
  # function, on every stored binding.  This is the gate for any fast path that
  # rebuilds a piped ui without re-evaluating `ui$fun()`.  `model`/`modelName`
  # record the call and `meta` is an environment, so those are compared apart.
  .expectPipeEquivalent <- function(piped) {
    .p <- rxUiDecompress(piped)
    .f <- rxUiDecompress(suppressMessages(rxode2(.p$fun)))
    .skip <- c("meta", "model", "modelName")
    .nms <- setdiff(union(ls(.p, all.names = TRUE), ls(.f, all.names = TRUE)), .skip)
    for (.n in .nms) {
      expect_identical(get0(.n, .p), get0(.n, .f), label = paste0("piped$", .n))
    }
    expect_identical(
      sort(ls(.p$meta, all.names = TRUE)),
      sort(ls(.f$meta, all.names = TRUE))
    )
  }

  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }

  lin <- function() {
    ini({
      tka <- 0.45
      tcl <- log(2.7)
      tv <- 3.45
      cl.wt <- 0.75
      eta.cl ~ 0.3
      add.sd <- 0.7
      prop.sd <- 0.1
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl + eta.cl + cl.wt * log(WT / 70))
      v <- exp(tv)
      cp <- linCmt()
      cp ~ add(add.sd) + prop(prop.sd)
    })
  }

  udf <- function() {
    ini({
      tcl <- log(2.7)
      tv <- 3.45
      tp <- 0.1
      eta.cl ~ 0.3
      add.sd <- 0.7
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      p <- plogis(tp)
      d / dt(center) <- -cl / v * center
      cp <- center / v + p
      cp ~ add(add.sd)
    })
  }

  test_that("model() piping matches a from-scratch build", {
    .u <- rxode2(one.cmt)
    .expectPipeEquivalent(.u |> model(ka <- exp(tka + eta.ka + 0.1)))
    .expectPipeEquivalent(.u |> model(cl <- exp(tcl + eta.cl + wt * covwt)))
    .expectPipeEquivalent(.u |> model(cp2 <- cp * 2, append = TRUE))
    .expectPipeEquivalent(.u |> model(foo <- 3, append = NA))
    .expectPipeEquivalent(.u |> model(-cp))
    .expectPipeEquivalent(.u |> model(cp ~ add(add.sd) + prop(prop.sd)))
    .expectPipeEquivalent(.u |> model(v <- exp(tv + eta.v) * (WT / 70)^tvwt))
  })

  test_that("ini() piping that rebuilds matches a from-scratch build", {
    .u <- rxode2(one.cmt)
    .expectPipeEquivalent(.u |> ini(~eta.ka))
    .expectPipeEquivalent(.u |> ini(-eta.ka))
  })

  test_that("linCmt() and user-function models pipe like a from-scratch build", {
    .l <- rxode2(lin)
    .expectPipeEquivalent(.l |> model(v <- exp(tv + eta.v)))
    .expectPipeEquivalent(.l |> model(cl <- exp(tcl + eta.cl + cl.wt * log(WT / 70) + cl.age * AGE)))
    .d <- rxode2(udf)
    .expectPipeEquivalent(.d |> model(cl <- exp(tcl + eta.cl + 0.1)))
  })

  test_that("piping a line that calls a ui user function (plogis) works", {
    .d <- rxode2(udf)
    .p <- suppressMessages(.d |> model(p <- plogis(tp + eta.p)))
    expect_true("eta.p" %in% .p$iniDf$name)
    expect_equal(modelExtract(.p, p), "p <- expit(tp + eta.p, 0, 1)")
    .expectPipeEquivalent(.p)
    # nested inside another user function
    .p2 <- suppressMessages(.d |> model(p <- expit(plogis(tp + eta.p))))
    expect_true("eta.p" %in% .p2$iniDf$name)
  })

  test_that("promoting a covariate gives a from-scratch model whatever rxode2.verbose.pipe is", {
    cov <- function() {
      ini({
        tcl <- log(2.7)
        tv <- 3.45
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl + eta.cl + covwt * wt)
        v <- exp(tv + etav)
        d / dt(center) <- -cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    .u <- rxode2(cov)
    .etas <- list()
    for (.verbose in c(TRUE, FALSE)) {
      withr::with_options(list(rxode2.verbose.pipe = .verbose), {
        .theta <- suppressMessages(.u |> ini(covwt = 0.5))
        .etas[[length(.etas) + 1L]] <- rxUiDecompress(suppressMessages(.u |> ini(etav ~ 0.1)))
        if (!.verbose) {
          expect_silent(.u |> ini(covwt = 0.5))
          expect_silent(.u |> ini(etav ~ 0.1))
        }
      })
      expect_false("covwt" %in% .theta$covariates)
      expect_equal(.theta$muRefCovariateDataFrame$covariateParameter, "covwt")
      .expectPipeEquivalent(.theta)
    }
    for (.eta in .etas) {
      expect_false("etav" %in% .eta$covariates)
      expect_true("etav" %in% .eta$eta)
      expect_equal(.eta$muRefDataFrame$theta[.eta$muRefDataFrame$eta == "etav"], "tv")
      .expectPipeEquivalent(.eta)
    }
  })

  test_that("expanding piped ui functions leaves the parse state untouched", {
    .before <- as.list(.udfUiEnv)
    .rxUdfUiExpandPure(list(quote(p <- plogis(a))), NULL)
    expect_identical(as.list(.udfUiEnv)[sort(names(.before))], .before[sort(names(.before))])
    expect_setequal(ls(.udfUiEnv, all.names = TRUE), names(.before))
  })

  test_that("a ui function requesting model variables restores the parse state", {
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModM(~ x^2) + d
      })
    }
    .u <- f()
    expect_null(.udfUiEnv$methodCache)
    expect_false(.udfUiEnv$parsing)
    expect_null(.udfUiEnv$iniDf)
  })
})
