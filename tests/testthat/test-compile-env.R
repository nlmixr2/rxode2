test_that("model builds survive leaked compiler environment variables", {
  skip_on_cran()
  # rstan::stan_model permanently leaks PKG_CPPFLAGS (with a forced C++
  # -include), PKG_LIBS and USE_CXX17 into the session; R CMD SHLIB then
  # applies them to rxode2's C compile, which dies with "compilation
  # terminated".  The load-time snapshot + per-build swap makes the build
  # immune.  Simulate the leak with a forced include of a C++-only header
  # and a bogus library.
  .old <- Sys.getenv(c("PKG_CPPFLAGS", "PKG_LIBS", "USE_CXX17"),
                     unset = NA_character_)
  on.exit({
    for (.v in names(.old)) {
      if (is.na(.old[[.v]])) {
        Sys.unsetenv(.v)
      } else {
        do.call(Sys.setenv, stats::setNames(list(.old[[.v]]), .v))
      }
    }
  }, add = TRUE)
  .hdr <- tempfile(fileext = ".hpp")
  writeLines("template <typename T> struct rxLeakProbe { T x; };", .hdr)
  Sys.setenv(PKG_CPPFLAGS = paste0("-include '", .hdr, "'"),
             PKG_LIBS = "-lrxNoSuchLibrary",
             USE_CXX17 = "1")
  # a model text unique to this run so a cached DLL cannot mask the compile
  .txt <- sprintf("d/dt(a) = -%s*a", format(stats::runif(1, 0.1, 0.2),
                                            digits = 10))
  .m <- rxode2(.txt)
  expect_s3_class(.m, "rxode2")
  # and the session's (leaked) values are untouched after the build --
  # the swap is scoped to the compile only
  expect_equal(Sys.getenv("PKG_LIBS"), "-lrxNoSuchLibrary")
})
