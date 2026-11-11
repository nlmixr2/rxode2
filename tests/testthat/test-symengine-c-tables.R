rxTest({
  # The C translator (src/seFromSEcalls.h) keeps its own copy of the name lists
  # .rxFromSE() dispatches on.  These tests hold the two to each other, so a
  # function added to one list in R cannot silently go missing in C.

  .tab <- as.data.frame(.Call(`_rxode2_seFromSEfnTable`), stringsAsFactors = FALSE)
  .flag <- function(nm, bit) {
    .w <- match(nm, .tab$name)
    !is.na(.w) & bitwAnd(.tab$flags[.w], bit) != 0L
  }

  test_that("C has every .SEsingle rewrite, spelled the same", {
    for (.n in names(.SEsingle)) {
      .w <- match(.n, .tab$name)
      expect_false(is.na(.w), info = .n)
      expect_equal(.tab$kind[.w], 4L, info = .n) # SE_K_SINGLE
      expect_equal(c(.tab$a[.w], .tab$b[.w]), .SEsingle[[.n]], info = .n)
    }
    expect_setequal(.tab$name[.tab$kind == 4L], names(.SEsingle))
  })

  test_that("C has every .SEdouble rewrite a symengine call can name", {
    # the operator spellings (==, &&, ...) cannot be a call name in symengine
    # output, so only the named entries need to be in C
    .named <- names(.SEdouble)[grepl("^[A-Za-z_]", names(.SEdouble))]
    for (.n in .named) {
      .w <- match(.n, .tab$name)
      expect_false(is.na(.w), info = .n)
      expect_equal(.tab$kind[.w], 5L, info = .n) # SE_K_DOUBLE
      expect_equal(c(.tab$a[.w], .tab$b[.w], .tab$c[.w]), .SEdouble[[.n]], info = .n)
    }
    expect_setequal(.tab$name[.tab$kind == 5L], .named)
  })

  test_that("C has .SE1p, .SE1m, .rxSEzeroD and .rxToSEDualVarFunction", {
    expect_true(all(.flag(names(.SE1p), 4L)))
    expect_equal(.tab$p[match(names(.SE1p), .tab$name)], unname(.SE1p))
    expect_true(all(.flag(names(.SE1m), 8L)))
    expect_equal(.tab$p[match(names(.SE1m), .tab$name)], unname(.SE1m))
    expect_true(all(.flag(.rxSEzeroD, 2L)))
    expect_setequal(.tab$name[bitwAnd(.tab$flags, 2L) != 0L], .rxSEzeroD)
    expect_true(all(.flag(.rxToSEDualVarFunction, 1L)))
    expect_setequal(.tab$name[bitwAnd(.tab$flags, 1L) != 0L], .rxToSEDualVarFunction)
  })

  test_that("C writes numbers exactly as as.character() does", {
    .x <- c(
      0, 1, -1, 0.5, 0.1, 1e-4, 1e-5, 1e-6, 1.5e-7, 1e5, 1e6, 123456, 1234567,
      1e15, 1e16, 1e22, 1234567890123456789, 0.30000000000000004, 1 / 3, 2 / 3,
      pi, exp(1), 100, 1e-300, 1e300, 99999.99999999999, 9.999999999999999e4,
      -1e5, -0.0001, 6.055454452393343e-06, .Machine$double.eps,
      .Machine$double.xmax, NaN, Inf, -Inf
    )
    withr::with_seed(42, {
      .x <- c(
        .x, stats::runif(200, -1e3, 1e3), 10^stats::runif(200, -20, 20),
        round(stats::runif(100, 0, 1e7))
      )
    })
    # NA = declined: R for Windows prints with its own trio library, so C
    # leaves numbers whose last digits trio may print differently to R
    .same <- function(.sp) {
      .c <- .Call(`_rxode2_seFormatDouble`, .x, .sp)
      .r <- as.character(.x)
      if (.Platform$OS.type != "windows") {
        expect_false(anyNA(.c), info = paste("scipen", .sp))
      }
      expect_identical(.c[!is.na(.c)], .r[!is.na(.c)], info = paste("scipen", .sp))
    }
    .same(0L)
    for (.sp in c(-5L, 3L, 999L)) {
      withr::with_options(list(scipen = .sp), .same(.sp))
    }
  })

  test_that("number rendering in rxFromSE() matches the R walker", {
    .rTxt <- function(s) {
      .rxFromSE(eval(parse(text = paste0("quote({", s, "})"))))
    }
    for (.s in c("a*100000", "a*0.0001", "100000*a", "a^100000", "a + 0.0001",
                 "a*1234567890123456789", "log(1e-04 + a)")) {
      .c <- .rxFromSEC(.s, 1L)
      if (is.na(.c) && .Platform$OS.type == "windows") next # trio: src/seNum.h
      expect_identical(.c, .rTxt(.s), info = .s)
    }
  })
})
