rxTest({
  # The parser keeps md5/me_code for the later `_rxode2_codegen` call; they
  # used to point into the argument CHARSXPs, which a GC could collect in
  # between (#1421).
  .codegenMd5 <- function() digest::digest("rxode2 issue 1421 file md5")
  .codegenMe <- function() paste0("/* issue 1421 ", .codegenMd5(), " */")

  test_that("codegen reads the parser md5/me_code after a gc", {
    .ret <- .Call(
      `_rxode2_trans`,
      "d/dt(cgMd5A) = -kCgMd5A * cgMd5A",
      "",
      .codegenMd5(),
      1L,
      0L,
      .codegenMe(),
      .rxSupportedFuns(),
      FALSE
    )
    .parsedMd5 <- digest::digest("rxode2 issue 1421 parsed md5")
    .ret$md5 <- c(file_md5 = "", parsed_md5 = .parsedMd5)
    .ret[[17]] <- list()
    for (.i in 1:3) gc(full = TRUE)
    .cFile <- tempfile("rx_cgmd5_", fileext = ".c")
    on.exit(unlink(.cFile), add = TRUE)
    .lib <- gsub("[.]c$", "", basename(.cFile))
    .codegen(
      .cFile, "rx_cgmd5_", c(.lib, .lib), .parsedMd5, .ret,
      .rxSupportedFuns()
    )
    expect_identical(.ret$md5[[1]], .codegenMd5())
    expect_identical(.ret$model[[2]], .codegenMe())
    # generated symbols are keyed by the parsed md5 handed to codegen
    .def <- grep("^#define _getRxSolve_ ", readLines(.cFile), value = TRUE)
    expect_length(.def, 1L)
    expect_true(grepl(.parsedMd5, .def, fixed = TRUE))
    expect_false(grepl(.codegenMd5(), .def, fixed = TRUE))
  })

  test_that("a malformed model md5 is blanked, not left over from the last parse", {
    .ret <- .Call(
      `_rxode2_trans`,
      "d/dt(cgMd5B) = -kCgMd5B * cgMd5B",
      "",
      "not-an-md5",
      1L,
      0L,
      "",
      .rxSupportedFuns(),
      FALSE
    )
    .parsedMd5 <- digest::digest("rxode2 issue 1421 bad md5")
    .ret$md5 <- c(file_md5 = "x", parsed_md5 = .parsedMd5)
    .ret[[17]] <- list()
    .cFile <- tempfile("rx_cgmd5b_", fileext = ".c")
    on.exit(unlink(.cFile), add = TRUE)
    .lib <- gsub("[.]c$", "", basename(.cFile))
    .codegen(
      .cFile, "rx_cgmd5b_", c(.lib, .lib), .parsedMd5, .ret,
      .rxSupportedFuns()
    )
    expect_identical(.ret$md5[[1]], "")
    .def <- grep("^#define _getRxSolve_ ", readLines(.cFile), value = TRUE)
    expect_true(grepl(.parsedMd5, .def, fixed = TRUE))
  })
})
