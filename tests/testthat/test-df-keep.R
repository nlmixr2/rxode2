rxTest({
  # rxSolve()'s keep columns and factor templates, read through the
  # function-pointer table (slots 108-112) as a downstream package laying out
  # its own rows reads them.
  .keepMod <- rxode2({
    ka <- 1.2
    cl <- 3 * (WT / 70)
    v <- 30
    d / dt(depot) <- -ka * depot
    d / dt(center) <- ka * depot - cl / v * center
    cp <- center / v
    grp <- "low"
    if (cp > 1) grp <- "high"
  })

  .keepData <- function() {
    .d <- data.frame(
      id = rep(1:2, each = 6),
      time = rep(c(0, 1, 2, 4, 8, 12), 2),
      evid = rep(c(1, 0, 0, 0, 0, 0), 2),
      amt = rep(c(100, 0, 0, 0, 0, 0), 2),
      cmt = rep(c(1, 2, 2, 2, 2, 2), 2),
      WT = c(70, 70, 72, 72, 75, 75, 60, 60, 61, 61, 62, 62),
      num = seq(0.5, 6, by = 0.5),
      sex = factor(rep(c("F", "M", "F"), 4)),
      chr = rep(c("a", "b", "c", NA), 3),
      flag = rep(c(TRUE, FALSE, NA), 4),
      int = 1:12
    )
    .d
  }

  # WT is a model covariate: rxSolve() outputs it as one, not as a keep column
  .keepCols <- c("num", "sex", "chr", "flag", "int", "WT")
  .keepData0 <- c("int", "flag", "chr", "sex", "num")

  for (.ad in list(FALSE, TRUE)) {
    test_that(paste0("the keep slots give rxSolve()'s keep columns, addDosing=", .ad), {
      .d <- .keepData()
      .ref <- as.data.frame(rxSolve(.keepMod, .d, keep = .keepCols, addDosing = .ad, returnType = "data.frame"))
      rxSolve(.keepMod, .d, keep = .keepCols, addDosing = .ad, .setupOnly = 1L)
      on.exit(rxSolveFree(), add = TRUE)
      .w <- .Call("_rxTestDfKeepSlots", .rxode2ptrs(), if (.ad) 3L else 0L, "grp", PACKAGE = "rxode2")
      expect_named(.w$keep, .keepData0)
      for (.c in .keepData0) {
        expect_identical(.w$keep[[.c]], .ref[[.c]], label = .c)
      }
      # the string lhs: getDfLevels() is the factor rxSolve() returns it as
      expect_identical(levels(.w$levels), levels(.ref$grp))
      expect_s3_class(.w$levels, "factor")
    })
  }

  test_that("the keep and level slots occupy 108-112", {
    .n <- names(.rxode2ptrs())
    expect_identical(
      .n[109:113],
      c("rxode2rxDfKeepSetup", "rxode2rxDfKeepFirst", "rxode2rxDfKeepRow", "rxode2rxDfKeepCols", "rxode2getDfLevels")
    )
  })
})
