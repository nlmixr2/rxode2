rxTest({
  test_that("searchReplace", {
    model <- readModelDb("PK_1cmt")
    result <- searchReplace(model, find = "lka", replace = "lka + etalka")
    expect_equal(
      findBlock(result, "model")[[2]],
      str2lang("ka <- exp(lka + etalka)")
    )
  })

  test_that(".replaceMult will keep -kel*central", {
    expect_equal(
      .replaceMult(
        list(str2lang("d/dt(central) <- -kel * central")),
        "Ek",
        "Cc",
        "Emax*Cc/(Cc+EC50)"
      ),
      list(str2lang("d/dt(central) <- -kel * central"))
    )
  })

  test_that("searchReplace replaces a whole call when find matches exactly", {
    model <- readModelDb("PK_1cmt")
    result <- searchReplace(model, find = "exp(lka)", replace = "exp(lka + etalka)")
    deparsed <- paste(deparse(functionBody(result)), collapse = "\n")
    expect_match(deparsed, "exp\\(lka \\+ etalka\\)", fixed = FALSE)
    expect_false(grepl("ka <- exp(lka)", deparsed, fixed = TRUE))
  })
})
