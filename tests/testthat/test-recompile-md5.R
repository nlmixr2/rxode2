rxTest({
  test_that("a recompile after rxDelete keeps the md5 the model was made with", {
    .mod <- "
      d/dt(x) <- -exp(tk)*x
      cp <- x * 3
    "
    .ev <- et(amt = 100, cmt = 1) |> et(0:24)
    for (.lvl in c("none", "slice")) {
      .other <- setdiff(c("none", "slice"), .lvl)
      .m <- withr::with_options(list(rxode2.fullOpt = .lvl), rxode2(.mod))
      .md5 <- rxModelVars(.m)$md5[["parsed_md5"]]
      rxDelete(.m)
      # recompiles under a different option; used to segfault on a warm cache
      .s <- withr::with_options(
        list(rxode2.fullOpt = .other),
        rxSolve(.m, .ev, params = c(tk = 0.1))
      )
      expect_equal(nrow(.s), 25L)
      expect_identical(rxModelVars(.m)$md5[["parsed_md5"]], .md5)
    }
  })
})
