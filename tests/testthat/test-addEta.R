rxTest({
  test_that("addEta named parameter", {
    model <- readModelDb("PK_1cmt")

    suppressMessages(modelUpdate <- addEta(model, eta = "lka"))

    # initial conditions are added
    expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etaLka ~ 0.1"))
    # eta is added
    expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etaLka)"))

    if (requireNamespace("withr", quietly = TRUE)) {
      withr::with_options(list(rxode2.etaCombineType = "snake"), {
        suppressMessages(modelUpdate <- addEta(model, eta = "lka"))
        expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("eta_lka ~ 0.1"))
        expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + eta_lka)"))
      })

      withr::with_options(list(rxode2.etaCombineType = "dot"), {
        suppressMessages(modelUpdate <- addEta(model, eta = "lka"))
        expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("eta.lka ~ 0.1"))
        expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + eta.lka)"))
      })

      withr::with_options(list(rxode2.etaCombineType = "blank"), {
        suppressMessages(modelUpdate <- addEta(model, eta = "lka"))
        expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etalka ~ 0.1"))
        expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etalka)"))
      })

      withr::with_options(list(rxode2.etaCombineType = "camel"), {
        suppressMessages(modelUpdate <- addEta(model, eta = "lka"))
        expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etaLka ~ 0.1"))
        expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etaLka)"))
      })

      withr::with_options(list(rxode2.etaCombineType = 4), {
        # invalid value falls back to camel; modelUpdate is from the camel block above
        expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etaLka ~ 0.1"))
        expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etaLka)"))
      })
    }
  })

  test_that("addEta mu-ref parameter", {
    model <- readModelDb("PK_1cmt")
    suppressMessages(modelUpdate <- addEta(model, eta = "ka"))
    # initial conditions are added
    expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etaKa ~ 0.1"))
    # eta is added
    expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etaKa)"))
  })

  test_that("addEta multiple parameter, mu-ref and not", {
    model <- readModelDb("PK_1cmt")
    suppressMessages(modelUpdate <- addEta(model, eta = c("lvc", "ka")))
    # initial conditions are added
    expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etaLvc ~ 0.1"))
    expect_equal(findBlock(modelUpdate, "ini")[[11]], str2lang("etaKa ~ 0.1"))
    # etas are added
    expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etaKa)"))
    expect_equal(findBlock(modelUpdate, "model")[[4]], str2lang("vc <- exp(lvc + etaLvc)"))
  })

  test_that("addEta named parameter", {
    model <- readModelDb("PK_1cmt")
    suppressMessages(modelUpdate <- addEta(model, eta = "lka"))
    # initial conditions are added
    expect_equal(findBlock(modelUpdate, "ini")[[10]], str2lang("etaLka ~ 0.1"))
    # eta is added
    expect_equal(findBlock(modelUpdate, "model")[[2]], str2lang("ka <- exp(lka + etaLka)"))
  })

  test_that("addEta non-existent parameter", {
    model <- readModelDb("PK_1cmt")
    suppressMessages(expect_error(
      addEta(model, eta = "foo")
    ))
  })

  test_that("compiled ui object", {
    model <- readModelDb("PK_1cmt")
    model <- rxode2(model)
    expect_true(inherits(model, "rxUi"))
    suppressMessages(modelUpdate <- addEta(model, eta = "lka"))
    expect_true(inherits(modelUpdate, "rxUi"))
  })

  test_that("addEta() correctly adds IIV when there is a covariate (#27)", {
    model <- function() {
      ini({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- c(0, 0.5)
      allo_cl <- 0.75
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl + allo_cl * WT)
      vc <- exp(lvc)
      cp <- linCmt()
      cp ~ prop(propSd)
    })
    }
    # Update the model detecting the correct parameter for cl
    suppressMessages(newEtaRemap <- addEta(model, "cl", priorName = FALSE))
    # Update the model where the correct parameter for cl is given explicitly
    suppressMessages(newEta <- addEta(model, "lcl", priorName = FALSE))
    expect_equal(newEtaRemap, newEta, ignore_function_env = TRUE)
  })
})
