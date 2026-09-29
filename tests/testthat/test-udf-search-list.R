## What the user function search list (`.udfEnv$searchList`) keeps alive: it is
## bounded, and `$` on an rxUi does not record rxode2's own frames in it.

rxTest({
  test_that("the user function search list is bounded", {
    .mod <- function() {
      ini({
        tkaSearch <- 0.5
        addSdSearch <- 0.7
      })
      model({
        kaSearch <- exp(tkaSearch)
        d/dt(depotSearch) <- -kaSearch * depotSearch
        cpSearch <- depotSearch
        cpSearch ~ add(addSdSearch)
      })
    }
    .ui <- rxode2(.mod)
    # every call records the (fresh, and immediately dead) frame of `.theta()`;
    # the list used to keep all of them
    .theta <- function(ui) invisible(ui$theta)
    for (.i in seq_len(50)) {
      .theta(.ui)
    }
    expect_lte(length(.udfEnv$searchList), .udfSearchListMax())

    withr::with_options(list(rxode2.udfSearchLimit = 5), {
      for (.i in seq_len(20)) {
        .theta(.ui)
      }
      expect_lte(length(.udfEnv$searchList), 5L)
    })
    # a bad option value falls back to the default rather than erroring
    withr::with_options(list(rxode2.udfSearchLimit = "many"), {
      expect_equal(.udfSearchListMax(), 20L)
    })
  })

  test_that("rxode2's own frames are not kept for finding user functions", {
    skipIfOldLotri()
    .ui <- rxUiDecompress(rxode2(function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cp <- ka
        cp ~ add(add.sd)
      })
    }))
    .ini <- .ui$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "tka"] <- "dnorm(0, 10)"
    assign("iniDf", .ini, envir = .ui)
    # theta is held only by rxPriorLogDensity()'s own frame, which reads ui$iniDf
    .acc <- new.env()
    .acc$freed <- FALSE
    .onFree <- function(e) .acc$freed <- TRUE
    .marked <- function() {
      .e <- new.env()
      reg.finalizer(.e, .onFree)
      structure(c(tka = 0.1, add.sd = 0.5), marker = .e)
    }
    invisible(rxPriorLogDensity(.ui, theta = .marked()))
    invisible(gc())
    expect_true(.acc$freed)
  })
})
