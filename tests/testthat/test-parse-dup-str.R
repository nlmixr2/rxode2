test_that("a parse that makes many name copies keeps the earlier ones valid", {
  # every function call copies its name; a name held while more copies were made
  # used to point into a buffer that had moved (a use-after-free under ASan)
  .n <- 1500
  .txt <- c(
    sprintf("a%d <- exp(b%d) + log(c%d + 1) + pow(d%d, 2) + abs(e%d)", seq_len(.n), seq_len(.n),
      seq_len(.n), seq_len(.n), seq_len(.n)),
    "d/dt(x) <- -a1 * x"
  )
  .mv <- rxModelVars(paste(.txt, collapse = "\n"))
  expect_equal(length(.mv$lhs), .n)
  expect_true(all(sprintf("b%d", seq_len(.n)) %in% .mv$params))
  # the same text parses to the same model again
  expect_identical(rxModelVars(paste(.txt, collapse = "\n"))$md5, .mv$md5)
})
