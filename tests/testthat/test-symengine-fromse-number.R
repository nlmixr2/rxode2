rxTest({
  # The C translator renders numbers as R's as.character() does (#1673).
  test_that("C renders numbers exactly as the R walker does", {
    .x <- c(
      1e5,
      123456,
      1e15,
      1e-4,
      0.001,
      123456789012345678,
      1 / 3,
      0.1,
      100,
      1e-5,
      12345678901,
      1234567890123456,
      2^60,
      1e-300,
      1.5e300,
      1e100,
      0.12345678901234567,
      1e16,
      99999.5,
      1e5 + 0.1,
      3e-6,
      314159.26535,
      1.0000000000000002,
      0.5,
      1e-15,
      123.456e10
    )
    .txt <- sprintf("%.17g", .x)
    .txt <- c(.txt, paste0("a*", .txt), paste0("a + ", .txt), paste0("log(a + ", .txt, ")"))
    .c <- .rxFromSEC(.txt)
    .r <- vapply(.txt, function(x) .rxFromSE(str2lang(x)), character(1), USE.NAMES = FALSE)
    expect_identical(.c, .r)
  })
})
