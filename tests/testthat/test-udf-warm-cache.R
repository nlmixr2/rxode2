test_that("a cached model with an R user function solves in a new session", {
  skip_on_cran()
  .dir <- tempfile("rxcache")
  dir.create(.dir)
  on.exit(unlink(.dir, recursive = TRUE), add = TRUE)
  .script <- tempfile(fileext = ".R")
  on.exit(unlink(.script), add = TRUE)
  writeLines(
    c(
      "suppressMessages(library(rxode2))",
      "myf <- function(x) x * 2",
      "ev <- et(amt = 100) |> et(0:5)",
      "m <- rxode2('cc <- myf(tcl); d/dt(x) <- -tcl*x; y <- x + cc')",
      "s <- suppressWarnings(rxSolve(m, ev, params = c(tcl = 0.2)))",
      "cat('RXODE2-OK', s$cc[1], '\\n')",
      # the function lives in a calling function's frame, not the global env
      "f <- function() {",
      "  myg <- function(x) x * 3",
      "  m <- rxode2('cc <- myg(tcl); d/dt(x) <- -tcl*x; y <- x + cc')",
      "  suppressWarnings(rxSolve(m, ev, params = c(tcl = 0.2)))",
      "}",
      "cat('RXODE2-LOCAL', f()$cc[1], '\\n')"
    ),
    .script
  )
  .run <- function() {
    suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", shQuote(.script)),
      env = c(
        paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep)),
        paste0("rxTempDir=", .dir),
        "MC_CORES=1"
      ),
      stdout = TRUE,
      stderr = TRUE
    ))
  }
  # the second process finds the model in the cache of the first
  for (.i in 1:2) {
    .out <- .run()
    .info <- paste(utils::tail(.out, 10), collapse = "\n")
    expect_true(any(grepl("RXODE2-OK 0.4", .out, fixed = TRUE)), info = .info)
    expect_true(any(grepl("RXODE2-LOCAL 0.6", .out, fixed = TRUE)), info = .info)
  }
})
