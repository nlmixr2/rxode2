rxTest({
  # The extra-dosing pools (ignoredDoses/pendingDoses/extraDose*) are indexed by
  # _rxTid(), which is bounded by op->cores, but they were only ever sized once
  # at load from omp_get_max_threads().  omp_get_max_threads() honors
  # OMP_NUM_THREADS, while `rxSolve(cores=)` overrides it through the
  # num_threads clause -- so an environment that sets OMP_NUM_THREADS below the
  # requested cores left every thread past that count writing off the end of
  # those arrays, corrupting the heap.  CRAN check machines set
  # OMP_NUM_THREADS=2, which is how this reached a reverse dependency.
  #
  # The pools are sized at load, so this has to run in a fresh process with the
  # environment variable already set; it cannot be provoked in-process.
  test_that("solving with more cores than OMP_NUM_THREADS does not corrupt the heap", {
    skip_on_cran()
    if (!isTRUE(rxode2::rxCores() > 2L)) {
      skip("needs more than 2 available cores")
    }
    # resolved HERE, not in the child: inside the child OMP_NUM_THREADS caps
    # rxCores() at 2, which is exactly the mismatch we need to avoid asking for
    .cores <- as.integer(rxode2::rxCores())
    .script <- tempfile(fileext = ".R")
    on.exit(unlink(.script), add = TRUE)
    writeLines(c(
      sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
      "suppressMessages(library(rxode2))",
      "m <- rxode2({",
      "  ka <- exp(lka + eta.ka)",
      "  cl <- exp(lcl + eta.cl)",
      "  v <- exp(lv)",
      "  d/dt(depot) <- -ka*depot",
      "  d/dt(cent) <- ka*depot - cl/v*cent",
      "  Cc <- cent/v",
      "})",
      "ev <- et(et(amt=100, ii=24, addl=6), seq(0, 168, by=2))",
      "om <- lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.09)",
      "th <- c(lka=0.5, lcl=1, lv=3)",
      "tm <- diag(3)*0.01",
      "dimnames(tm) <- list(names(th), names(th))",
      "set.seed(1)",
      "for (i in 1:40) {",
      "  s <- rxSolve(m, ev, params=th, omega=om, thetaMat=tm, nStud=4,",
      sprintf("               nSub=25, simVariability=TRUE, cores=%dL)", .cores),
      "  d <- as.data.frame(s)",
      # allocate hard right after each solve -- a corrupted heap dies here
      "  for (k in 1:20) invisible(sum(rnorm(nrow(d)))) ",
      "}",
      "cat('RXODE2-OK\\n')"
    ), .script)
    .out <- suppressWarnings(
      system2(file.path(R.home("bin"), "Rscript"),
              args = c("--vanilla", shQuote(.script)),
              env = c("OMP_NUM_THREADS=2", "MKL_NUM_THREADS=2",
                      "NOT_CRAN=true"),
              stdout = TRUE, stderr = TRUE))
    .status <- attr(.out, "status")
    # a corrupted heap shows up as a non-zero exit plus glibc's own complaint
    # ("free(): invalid pointer" / "malloc(): ...") or an outright segfault
    expect_true(any(grepl("RXODE2-OK", .out, fixed = TRUE)),
                info = paste(utils::tail(.out, 15), collapse = "\n"))
    expect_false(any(grepl("free\\(\\)|malloc\\(\\)|corrupt|segfault", .out)),
                 info = paste(utils::tail(.out, 15), collapse = "\n"))
    expect_true(is.null(.status) || identical(as.integer(.status), 0L))
  })
})
