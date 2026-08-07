# A missing DESCRIPTION Imports: package must produce an R condition, never a
# signal.  These run in a child Rscript because a SIGSEGV/SIGABRT would kill the
# testthat process itself, so the assertion is on the child's exit status.
#
# The two regressions guarded here:
#   rxode2ll missing -> SIGSEGV (139) on the first solve of any compiled model.
#     Every generated model resolves rxode2ll's symbols with R_GetCCallable()
#     from its R_init(); that errors out of R_init leaving the model's globals
#     NULL, but R keeps the dll loaded, so the half-initialized model is
#     accepted and the first solve calls through NULL.
#   cli missing -> SIGABRT (134) inside library(rxode2), from a file-scope
#     loadNamespace("cli") running in dlopen() where the R error becomes an
#     Rcpp::LongjumpException with no handler.

rxTest({

  .allVisiblePackages <- function() {
    .out <- character(0)
    for (.l in setdiff(.libPaths(), .Library)) {
      .d <- list.dirs(.l, recursive = FALSE)
      .d <- .d[file.exists(file.path(.d, "Meta", "package.rds"))]
      .nm <- basename(.d)
      .new <- !(.nm %in% names(.out))
      .out <- c(.out, stats::setNames(.d[.new], .nm[.new]))
    }
    .out
  }

  # Library of symlinks to every installed package except `drop`.  Read-only
  # with respect to the real libraries.
  .shadowLib <- function(drop, dir) {
    .lib <- file.path(dir, paste0("lib-no-", drop))
    dir.create(.lib, recursive = TRUE, showWarnings = FALSE)
    .src <- .allVisiblePackages()
    .src <- .src[names(.src) != drop]
    if (!all(file.symlink(unname(.src), file.path(.lib, names(.src))))) {
      return(NULL)
    }
    .lib
  }

  .runWithout <- function(drop, code, dir) {
    .lib <- .shadowLib(drop, dir)
    if (is.null(.lib)) return(NULL)
    .f <- file.path(dir, paste0("child-", drop, ".R"))
    writeLines(c('.libPaths(Sys.getenv("RX_LIB"), include.site = FALSE)', code), .f)
    .log <- file.path(dir, paste0("child-", drop, ".log"))
    .status <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(.f)),
      env = c(paste0("R_LIBS=", .lib), paste0("R_LIBS_USER=", .lib),
              paste0("R_LIBS_SITE=", file.path(dir, "no-site")),
              paste0("RX_LIB=", .lib), "NOT_CRAN=true"),
      stdout = .log, stderr = .log))
    list(status = .status, log = paste(readLines(.log, warn = FALSE), collapse = "\n"))
  }

  # Only meaningful against a real installed rxode2 (a load_all() tree cannot be
  # library()'d from a child process).
  .installed <- tryCatch(file.exists(file.path(find.package("rxode2"), "Meta", "package.rds")),
                         error = function(e) FALSE)

  for (.pkg in c("rxode2ll", "cli")) {
    local({
      pkg <- .pkg
      test_that(paste0("a missing '", pkg, "' gives an R condition, not a signal"), {
        skip_on_cran()
        skip_on_os("windows")
        skip_if_not(.installed, "needs an installed (not load_all) rxode2")
        skip_if_not(pkg %in% names(.allVisiblePackages()),
                    paste0("'", pkg, "' is not installed, nothing to hide"))
        dir <- tempfile("rxMissing"); dir.create(dir)
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        res <- .runWithout(pkg, c(
          'ok <- tryCatch({ suppressMessages(library(rxode2)); TRUE },',
          '               error = function(e) { cat("CAUGHT:", conditionMessage(e), "\n"); FALSE })',
          'if (ok) {',
          '  tryCatch({',
          '    m <- rxode2::rxode2("d/dt(x) <- -0.5*x;")',
          '    ev <- rxode2::et(rxode2::et(0:3), amt = 1, cmt = "x")',
          '    print(as.data.frame(rxode2::rxSolve(m, ev, cores = 1L))$x[2])',
          '  }, error = function(e) cat("CAUGHT:", conditionMessage(e), "\n"))',
          '}',
          'cat("REACHED-END\n")'), dir)
        skip_if(is.null(res), "symlinks unsupported here")
        # The regression: exit 139 (SIGSEGV) for rxode2ll, 134 (SIGABRT) for cli.
        expect_lt(res$status, 128L)
        expect_false(grepl("caught segfault", res$log, fixed = TRUE))
        expect_false(grepl("terminate called", res$log, fixed = TRUE))
        expect_match(res$log, "REACHED-END", fixed = TRUE)
        # Whatever happens, the child has to finish under R's control: either it
        # completes the workload, or it raises a condition naming what is gone.
        # (A package only reached lazily need not be wanted by this workload at
        # all, so an uneventful run is a pass.)
        if (grepl("CAUGHT:", res$log, fixed = TRUE)) {
          expect_match(res$log, pkg, fixed = TRUE)
        }
      })
    })
  }
})
