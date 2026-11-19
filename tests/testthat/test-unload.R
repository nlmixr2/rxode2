rxTest({
  ## .onUnload() must release the frames kept for finding user functions
  ## (by `$`, rxToSE(), rxFromSE() and a found R user function) before its
  ## gc(), while the DLL is still loaded.  Runs in a child process, since it
  ## unloads rxode2.  heldUdf() runs first: its rxode2() resets the search
  ## list, which would free the `$` frame early.
  test_that("unloading rxode2 releases the frames kept for user functions", {
    skip_on_cran()
    if (!is.null(asNamespace("rxode2")$.__DEVTOOLS__)) {
      skip("the child process loads the installed rxode2")
    }
    .script <- tempfile(fileext = ".R")
    on.exit(unlink(.script), add = TRUE)
    writeLines(
      c(
        sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
        "suppressMessages(library(rxode2))",
        "m <- function() {",
        "  ini({",
        "    tka <- 0.45",
        "    tcl <- 1",
        "    tv <- 3.45",
        "    add.sd <- 0.7",
        "  })",
        "  model({",
        "    ka <- exp(tka)",
        "    cl <- exp(tcl)",
        "    v <- exp(tv)",
        "    linCmt() ~ add(add.sd)",
        "  })",
        "}",
        "u <- rxode2(m)",
        "onFree <- function(e) {",
        "  dll <- 'rxode2' %in% names(getLoadedDLLs())",
        "  cat(sprintf('RXODE2-FREED %s dll=%s\\n', e$tag, dll))",
        "}",
        "tagged <- function(tag) {",
        "  e <- new.env()",
        "  e$tag <- tag",
        "  reg.finalizer(e, onFree)",
        "  e",
        "}",
        "heldUdf <- function() {",
        "  e <- tagged('udf')",
        "  udfPlusOne <- function(x) x + 1",
        "  suppressMessages(suppressWarnings(rxode2({",
        "    y <- udfPlusOne(t)",
        "  })))",
        "  NULL",
        "}",
        "heldUi <- function() {",
        "  e <- tagged('ui')",
        "  u$iniDf",
        "  invisible()",
        "}",
        "heldToSE <- function() {",
        "  e <- tagged('toSE')",
        "  rxToSE('a + b')",
        "  invisible()",
        "}",
        "heldFromSE <- function() {",
        "  e <- tagged('fromSE')",
        "  rxFromSE('a + b')",
        "  invisible()",
        "}",
        "invisible(heldUdf())",
        "invisible(heldUi())",
        "invisible(heldToSE())",
        "invisible(heldFromSE())",
        "invisible(gc())",
        "unloadNamespace('rxode2')",
        "cat('RXODE2-UNLOADED\\n')"
      ),
      .script
    )
    .out <- suppressWarnings(
      system2(
        file.path(R.home("bin"), "Rscript"),
        args = c("--vanilla", shQuote(.script)),
        stdout = TRUE,
        stderr = TRUE
      )
    )
    .info <- paste(utils::tail(.out, 15), collapse = "\n")
    ## finalizers run in no set order, so the freed lines are sorted (C order)
    .lines <- grep("^RXODE2-", .out, value = TRUE)
    .n <- length(.lines)
    expect_identical(
      c(sort(.lines[-.n], method = "radix"), .lines[.n]),
      c(
        "RXODE2-FREED fromSE dll=TRUE",
        "RXODE2-FREED toSE dll=TRUE",
        "RXODE2-FREED udf dll=TRUE",
        "RXODE2-FREED ui dll=TRUE",
        "RXODE2-UNLOADED"
      ),
      info = .info
    )
  })
})
