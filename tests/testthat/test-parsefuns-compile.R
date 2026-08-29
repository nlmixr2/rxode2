rxTest({
  # rxode2's parser accepts any name in .parseFuns, and code generation emits
  # that name VERBATIM as the C name.  So a function works only if a C symbol
  # of the same name happens to exist -- in libm, in Rmath.h, or in rxode2's
  # own headers -- and nothing checks that anywhere.
  #
  # Two entries were broken when this was written: ceiling() was missing from
  # the tables entirely (it fell through to the user-function path and hit
  # formals() of a primitive), and loggamma() parsed and then failed at the
  # compiler because C has no loggamma().  gammafn() and lgammafn() sit in the
  # same table and work purely because they coincide with Rmath.h.
  #
  # This asserts every fixed-arity entry parses AND compiles.  All of them go
  # into ONE model, so the passing case is a single compile; on failure it
  # bisects to name the offenders rather than reporting "something broke".

  .fns <- .parseEnv$.parseFuns
  .num <- .parseEnv$.parseNum
  .sng <- names(.rxSEsingle)
  .dbl <- names(.rxSEdouble)

  # .parseNum covers most; the rest take their arity from the symengine
  # single/double rewrite tables
  .arity <- function(f) {
    if (f %in% names(.num)) {
      .k <- .num[[f]]
      if (!is.na(.k)) return(as.integer(.k))
    }
    if (f %in% .sng) return(1L)
    if (f %in% .dbl) return(2L)
    NA_integer_
  }

  # first()/last() read a covariate, not a computed lhs ("only 'lag(a,1)' and
  # 'diff(a,1)' supported" otherwise), so give them one
  .covArg <- c("first", "last")
  # `%%` is an infix operator, not a callable name
  .notCallable <- "%%"

  .testable <- Filter(function(f) {
    !(f %in% .notCallable) && !is.na(.arity(f)) &&
      .arity(f) >= 1L && .arity(f) <= 4L
  }, .fns)

  .mkTxt <- function(fs) {
    .decl <- "a1 = t + 1.1\na2 = t + 2.2\na3 = t + 3.3\na4 = t + 4.4\n"
    .body <- vapply(seq_along(fs), function(.i) {
      .k <- .arity(fs[.i])
      .a <- if (fs[.i] %in% .covArg) "cov" else paste0("a", seq_len(.k), collapse = ", ")
      sprintf("y%d = %s(%s)", .i, fs[.i], .a)
    }, character(1))
    paste0(.decl, paste(.body, collapse = "\n"), "\nd/dt(x) = -x")
  }

  .parses <- function(f) {
    tryCatch({ rxode2::rxModelVars(.mkTxt(f)); TRUE }, error = function(e) FALSE)
  }
  .compiles <- function(fs) {
    if (length(fs) == 0L) return(TRUE)
    tryCatch({ rxode2::rxode2(.mkTxt(fs)); TRUE }, error = function(e) FALSE)
  }
  .bisect <- function(fs) {
    if (length(fs) == 1L) return(if (.compiles(fs)) character(0) else fs)
    .m <- length(fs) %/% 2L
    .l <- fs[seq_len(.m)]
    .r <- fs[(.m + 1L):length(fs)]
    c(if (.compiles(.l)) character(0) else .bisect(.l),
      if (.compiles(.r)) character(0) else .bisect(.r))
  }

  test_that("every .parseFuns entry the parser accepts also parses", {
    skip_on_cran()
    expect_gt(length(.testable), 150L)   # it must actually be testing something
    .bad <- .testable[!vapply(.testable, .parses, logical(1))]
    expect_equal(length(.bad), 0L,
                 info = paste0("these are in .parseFuns but do not parse: ",
                               paste(.bad, collapse = ", ")))
  })

  test_that("every .parseFuns entry compiles to C", {
    skip_on_cran()
    .ok <- .testable[vapply(.testable, .parses, logical(1))]
    if (.compiles(.ok)) {
      succeed()
    } else {
      # name them rather than reporting that something, somewhere, broke
      .bad <- .bisect(.ok)
      fail(paste0(
        "in .parseFuns but generate C that does not compile: ",
        paste(.bad, collapse = ", "),
        ".  Code generation emits the rxode2 name verbatim as the C name, so ",
        "each needs a C function of that name -- add a wrapper to ",
        "inst/include/rxode2_model_shared.h the way ceiling() and loggamma() ",
        "have one."))
    }
  })
})
