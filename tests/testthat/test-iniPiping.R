rxTest({
  # Tests for the ini()/model() piping helpers.
  #
  # The point of these helpers is that this package never builds an `iniDf` row
  # itself.  That data frame's columns belong to lotri and they change -- lotri
  # 1.0.5 added `prior`, which is what broke rxode2 5.1.6 -- so a hand-built row
  # is a latent failure waiting on the next upstream release.  These tests pin
  # the behavior that replaced it.

  test_that(".iniAddTheta sets the estimate, bounds and label", {
    ui <- readModelDb("PK_1cmt_des")
    ui <- rxUiDecompress(as.rxUi(ui))
    model(ui) <- c(list(str2lang("newp <- exp(lnewp)")), ui$lstExpr)
    # before ini(), an undefined parameter is only a covariate
    expect_true("lnewp" %in% ui$allCovs)

    out <- .iniAddTheta(ui, "lnewp", est = 0.25, lower = 0, upper = 10, label = "a new parameter")
    row <- out$iniDf[out$iniDf$name == "lnewp", ]
    expect_equal(nrow(row), 1L)
    expect_equal(row$est, 0.25)
    expect_equal(row$lower, 0)
    expect_equal(row$upper, 10)
    expect_equal(row$label, "a new parameter")
    expect_true(is.na(row$neta1))
    expect_false("lnewp" %in% out$allCovs)
  })

  test_that(".iniAddTheta leaves bounds alone when they are infinite", {
    ui <- readModelDb("PK_1cmt_des")
    ui <- rxUiDecompress(as.rxUi(ui))
    model(ui) <- c(list(str2lang("newp <- exp(lnewp)")), ui$lstExpr)
    out <- .iniAddTheta(ui, "lnewp", est = 0.1)
    row <- out$iniDf[out$iniDf$name == "lnewp", ]
    expect_equal(row$est, 0.1)
    expect_equal(row$lower, -Inf)
    expect_equal(row$upper, Inf)
    expect_true(is.na(row$label))
  })

  test_that(".iniAddTheta accepts a label that still carries a name", {
    # ifelse() keeps the names of its input, so a label can arrive as
    # c(kel = "elimination"); label() needs a bare string
    ui <- readModelDb("PK_1cmt_des")
    ui <- rxUiDecompress(as.rxUi(ui))
    model(ui) <- c(list(str2lang("newp <- exp(lnewp)")), ui$lstExpr)
    out <- .iniAddTheta(ui, "lnewp", label = c(newp = "elimination"))
    expect_equal(out$iniDf$label[out$iniDf$name == "lnewp"], "elimination")
  })

  test_that(".iniAddTheta rejects a bad name or label", {
    ui <- as.rxUi(readModelDb("PK_1cmt_des"))
    expect_error(.iniAddTheta(ui, c("a", "b")))
    expect_error(.iniAddTheta(ui, "lka", label = c("one", "two")))
  })

  test_that("a parameter added to the model does not inherit another's ini metadata", {
    # the replaced implementation copied an existing theta row as a template, so
    # a backTransform (or condition, or prior) on that row leaked onto every
    # parameter added afterwards
    ui <- as.rxUi(readModelDb("PK_1cmt_des"))
    ui <- ini(ui, lka <- backTransform("exp"))
    expect_equal(ui$iniDf$backTransform[ui$iniDf$name == "lka"], "exp")

    out <- as.rxUi(addWeibullAbs(ui))
    new <- out$iniDf[out$iniDf$name %in% c("lwa", "lwb"), ]
    expect_equal(nrow(new), 2L)
    expect_true(all(is.na(new$backTransform)))
    expect_true(all(is.na(new$condition)))
  })

  test_that("adding parameters survives an unknown column in iniDf", {
    # the regression this whole change is about: an upstream release adds a
    # column to iniDf that this package has never heard of
    ui <- rxUiDecompress(as.rxUi(readModelDb("PK_2cmt_no_depot")))
    ini <- ui$iniDf
    ini$someNewUpstreamColumn <- NA_character_
    ui$iniDf <- ini
    expect_true("someNewUpstreamColumn" %in% names(ui$iniDf))

    out <- as.rxUi(addDepot(ui))
    expect_true("lka" %in% out$iniDf$name)
    expect_equal(out$iniDf$est[out$iniDf$name == "lka"], 0.1)
  })
})
