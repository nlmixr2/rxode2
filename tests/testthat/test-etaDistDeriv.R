## Symbolic derivatives of a declaration's argument expressions.
##
## The gradient of the declared prior needs d(arg)/d(theta).  It used to central
## difference the argument expression -- 2*nth*na interpreted RPN evaluations per
## record, 1e-6 accurate, under an acceptance test that compares two nearby
## objective values.  rxode2 already differentiates user functions symbolically
## with SymEngine and compiles the result into a model DLL (`rxD`,
## `.rxDtemplates`, `rxToSE`/`rxFromSE`), so the same tools serve here.
##
## These tests pin the derivative TEXT against central differences of the same
## expression, so a wrong emission is caught here rather than as a fit that
## quietly optimizes the wrong gradient.

rxTest({
  ## central difference of `txt` w.r.t. `th` at `p`
  .fd <- function(txt, th, p) {
    .h <- 1e-6 * max(abs(p[[th]]), 1)
    .pu <- p
    .pu[[th]] <- p[[th]] + .h
    .pl <- p
    .pl[[th]] <- p[[th]] - .h
    (eval(str2lang(txt), .pu) - eval(str2lang(txt), .pl)) / (2 * .h)
  }

  test_that(".rxEtaDistD matches central differences", {
    skip_if_not_installed("symengine")
    .p <- list(lclrv = -2.4, lclm = 1.63, bWT = 0.35, WT = 55)
    .cases <- list(
      list("1/exp(lclrv)", "lclrv"),
      list("1/(exp(lclrv)*exp(lclm + bWT*log(WT/70)))", "lclrv"),
      list("1/(exp(lclrv)*exp(lclm + bWT*log(WT/70)))", "lclm"),
      list("1/(exp(lclrv)*exp(lclm + bWT*log(WT/70)))", "bWT"),
      list("lclm + bWT*log(WT/70)", "bWT"),
      list("exp(lclm)*sqrt(exp(lclrv))", "lclrv")
    )
    for (.c in .cases) {
      .d <- .rxEtaDistD(.c[[1]], .c[[2]])
      expect_false(is.null(.d))
      .sym <- eval(str2lang(.d), .p)
      .num <- .fd(.c[[1]], .c[[2]], .p)
      expect_equal(.sym, .num, tolerance = 1e-5)
    }
  })

  test_that("a theta the expression does not mention gives an exact zero", {
    skip_if_not_installed("symengine")
    ## not merely small -- symengine returns the literal 0, which is what lets
    ## the emission drop the line instead of computing a constant per record
    expect_equal(gsub("[[:space:]]+", "", .rxEtaDistD("1/exp(lclrv)", "lclm")), "0")
  })

  test_that("derivative anchors are named and emitted per theta", {
    skip_if_not_installed("symengine")
    .anc <- .rxEtaDistAnchors(
      "dgamma(shape=1/exp(lclrv), rate=1/(exp(lclrv)*exp(lclm)))",
      "eta.cl",
      latent = NULL
    )
    skip_if(is.null(.anc))
    .l <- .rxEtaDistDerivLines(.anc, c("lclrv", "lclm"))
    ## shape involves only lclrv; rate involves both -- so three lines, not four
    expect_equal(length(.l), 3L)
    expect_true(all(grepl("^rxEdD[.]eta[.]cl[.]", .l)))
    expect_true(any(grepl("^rxEdD[.]eta[.]cl[.]shape[.]lclrv <-", .l)))
    expect_true(any(grepl("^rxEdD[.]eta[.]cl[.]rate[.]lclrv <-", .l)))
    expect_true(any(grepl("^rxEdD[.]eta[.]cl[.]rate[.]lclm <-", .l)))
    ## and the dropped one is the structural zero
    expect_false(any(grepl("shape[.]lclm", .l)))
  })

  test_that("the emitted lines parse and evaluate to the right numbers", {
    skip_if_not_installed("symengine")
    .anc <- .rxEtaDistAnchors(
      "dgamma(shape=1/exp(lclrv), rate=1/(exp(lclrv)*exp(lclm + bWT*log(WT/70))))",
      "eta.cl",
      latent = NULL
    )
    skip_if(is.null(.anc))
    .l <- .rxEtaDistDerivLines(.anc, c("lclrv", "lclm", "bWT"))
    .p <- list(lclrv = -2.4, lclm = 1.63, bWT = 0.35, WT = 55)
    .args <- c(shape = "1/exp(lclrv)", rate = "1/(exp(lclrv)*exp(lclm + bWT*log(WT/70)))")
    for (.ln in .l) {
      .eq <- str2lang(.ln)
      .nm <- deparse1(.eq[[2]])
      .role <- sub("^rxEdD[.]eta[.]cl[.]([^.]+)[.].*$", "\\1", .nm)
      .th <- sub("^.*[.]", "", .nm)
      expect_equal(eval(.eq[[3]], .p), .fd(.args[[.role]], .th, .p), tolerance = 1e-5)
    }
  })
})
