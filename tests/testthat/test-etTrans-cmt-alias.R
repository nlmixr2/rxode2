rxTest({
  .m <- function() {
    ini({
      ka <- 1
      cl <- 1
      v <- 10
    })
    model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
  }
  .d <- data.frame(
    id = 1,
    time = c(0, 1, 2, 4),
    amt = c(100, 0, 0, 0),
    evid = c(1, 0, 0, 0),
    cmt = c("depot", "central", "central", "central")
  )

  test_that("an exact 'cmt' column wins over an upper-case 'CMT' column (#1410)", {
    .ref <- rxSolve(.m, .d)
    .d2 <- cbind(data.frame(CMT = 9), .d)
    .s <- rxSolve(.m, .d2)
    expect_equal(.s$cp, .ref$cp)
    expect_equal(etTrans(.d2, .m)$CMT, etTrans(.d, .m)$CMT)
  })

  test_that("an exact 'cmt' column wins over 'ytype', 'state' and 'var' (#1410)", {
    .ref <- rxSolve(.m, .d)
    for (.n in c("YTYPE", "ytype", "state", "VAR")) {
      .d2 <- .d
      .d2[[.n]] <- 3
      expect_equal(rxSolve(.m, .d2)$cp, .ref$cp)
    }
  })

  test_that("two compartment columns without an exact 'cmt' still error (#1410)", {
    .d2 <- .d
    names(.d2)[names(.d2) == "cmt"] <- "CMT"
    .d2$ytype <- 1
    expect_error(
      rxSolve(.m, .d2),
      "can only specify either 'cmt', 'ytype', 'state' or 'var'"
    )
    .d3 <- cbind(.d, cmt = 1)
    expect_error(
      rxSolve(.m, .d3),
      "can only specify either 'cmt', 'ytype', 'state' or 'var'"
    )
  })

  test_that("keep= names the 'cmt' and 'CMT' columns separately (#1410)", {
    .d2 <- .d
    .d2$CMT <- c(9, 8, 7, 6)
    expect_warning(.s <- rxSolve(.m, .d2, keep = "CMT"), NA)
    expect_equal(.s$CMT, c(8, 7, 6))
    expect_false("cmt" %in% names(.s))
    .s <- rxSolve(.m, .d2, keep = "cmt")
    expect_equal(as.character(.s$cmt), rep("central", 3))
    expect_false("CMT" %in% names(.s))
  })

  test_that(".etTransCmtCol() picks the column etTrans() reads (#1410)", {
    expect_equal(.etTransCmtCol(c("id", "CMT", "cmt")), 3L)
    expect_equal(.etTransCmtCol(c("id", "YTYPE", "cmt")), 3L)
    expect_equal(.etTransCmtCol(c("id", "YTYPE")), 2L)
    expect_equal(.etTransCmtCol(c("id", "time")), NA_integer_)
  })
})
