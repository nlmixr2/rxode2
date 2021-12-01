rxode2Test(
  {
    context("Testing solving with ID(s) in the dataset")

    test_that("simple solving with ID(s) in the dataset", {
      theoSd <- readRDS("theoSd.rds")
      d <- theoSd[, names(theoSd) != "EVID"]
      d <- d[d$ID != 10, ]

      mod <- rxode2({
        tka <- 1
        tcl <- 2
        tv <- 3
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        cp <- linCmt()
      })

      d$ID <- paste(d$ID)

      tmp <- rxSolve(mod, d)
      expect_true(inherits(tmp$id, "factor"))
      ## Notice 10 is missing.
      expect_equal(levels(tmp$id), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "11", "12"))

      tmp <- rxSolve(mod, d, idFactor = FALSE)
      expect_false(inherits(tmp$id, "factor"))

      d2 <- d
      d2$ID <- factor(d2$ID, c(1:10, 12), letters[c(1:10, 12)])

      tmp <- rxSolve(mod, d2)

      tmp <- rxSolve(mod, d, idFactor = FALSE)

      expect_false(inherits(tmp$id, "factor"))
    })

    test_that("Test giving IDs to data-frames", {
      d <- readRDS("theoSd.rds")
      d$ID <- paste(d$ID)

      mod <- rxode2({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        cp <- linCmt()
      })

      set.seed(100)
      parData <- data.frame(
        id = 1:12, tka = 1 + rnorm(12, sd = 0.01),
        tcl = 2 + rnorm(12, sd = 0.01),
        tv = 3 + rnorm(12, sd = 0.01)
      )

      parData2 <- parData[order(-parData$id), ]

      tmp1 <- rxSolve(mod, d, parData)

      tmp2 <- rxSolve(mod, d, parData2)

      parData3 <- parData[, names(parData) != "id"]

      expect_warning(rxSolve(mod, d, parData3))
      expect_warning(rxSolve(mod, d, parData3, warnIdSort = FALSE), regexp = NA)

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(levels(tmp1$id), levels(tmp1$params$id))
      expect_equal(levels(tmp2$id), levels(tmp2$params$id))

      ## Now drop an id from the dataset
      d <- d[d$ID != 10, ]

      tmp1 <- expect_warning(rxSolve(mod, d, parData))

      tmp2 <- expect_warning(rxSolve(mod, d, parData2))

      ## Now add an id to the dataset

      d <- readRDS("theoSd.rds")
      d$ID <- paste(d$ID)

      parData <- data.frame(
        id = 1:13, tka = 1 + rnorm(13, sd = 0.01),
        tcl = 2 + rnorm(13, sd = 0.01),
        tv = 3 + rnorm(13, sd = 0.01)
      )

      parData2 <- parData[order(-parData$id), ]

      tmp1 <- expect_warning(rxSolve(mod, d, parData))

      tmp2 <- expect_warning(rxSolve(mod, d, parData2))

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(levels(tmp1$id), levels(tmp1$params$id))
      expect_equal(levels(tmp2$id), levels(tmp2$params$id))

      ## Now try letters
      d <- readRDS("theoSd.rds")
      d$ID <- letters[d$ID]

      parData <- data.frame(
        id = 1:13, tka = 1 + rnorm(13, sd = 0.01),
        tcl = 2 + rnorm(13, sd = 0.01),
        tv = 3 + rnorm(13, sd = 0.01)
      )

      parData2 <- parData[order(-parData$id), ]

      expect_error(rxSolve(mod, d, parData))

      expect_error(rxSolve(mod, d, parData2))

      parData$id <- letters[parData$id]
      parData2$id <- letters[parData2$id]

      tmp1 <- expect_warning(rxSolve(mod, d, parData))

      tmp2 <- expect_warning(rxSolve(mod, d, parData2))

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(levels(tmp1$id), levels(tmp1$params$id))
      expect_equal(levels(tmp2$id), levels(tmp2$params$id))

      ## Last case, though rare

      parData <- data.frame(
        id = 1:12, tka = 1 + rnorm(12, sd = 0.01),
        tcl = 2 + rnorm(12, sd = 0.01),
        tv = 3 + rnorm(12, sd = 0.01)
      )

      parData$ID <- letters[parData$id]

      tmp1 <- expect_warning(rxSolve(mod, d, parData))

      parData2 <- parData[order(-parData$id), ]

      tmp2 <- expect_warning(rxSolve(mod, d, parData2))

      expect_false(all(tmp1$params$tka == tmp2$params$tka))
    })

    test_that("test iCov ID", {
      d <- readRDS("theoSd.rds")
      d$ID <- paste(d$ID)

      mod <- rxode2({
        tka <- 1
        tcl <- 2
        tv <- 3
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        cwt <- wt
        cp <- linCmt()
      })

      set.seed(100)
      iCov <- data.frame(id = 1:12, wt = 70 + rnorm(12, sd = 3))
      iCov2 <- iCov[order(-iCov$id), ]

      expect_error(rxSolve(mod, d, iCov = iCov, keep = "wt"))

      d <- d[, names(d) != "WT"]
      tmp1 <- rxSolve(mod, d, iCov = iCov, keep = "wt")
      tmp2 <- rxSolve(mod, d, iCov = iCov2, keep = "wt")

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(tmp1$cwt, tmp2$wt)
      expect_equal(tmp2$cwt, tmp2$wt)

      ## Now drop an id from the dataset
      d <- d[d$ID != "10", ]

      tmp1 <- expect_warning(rxSolve(mod, d, iCov = iCov, keep = "wt"))
      tmp2 <- expect_warning(rxSolve(mod, d, iCov = iCov2, keep = "wt"))

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(tmp1$cwt, tmp2$wt)
      expect_equal(tmp2$cwt, tmp2$wt)

      ## Now add an id to the dataset

      d <- readRDS("theoSd.rds")
      d$ID <- paste(d$ID)
      d <- d[, names(d) != "WT"]

      iCov <- data.frame(id = 1:13, wt = 70 + rnorm(13, sd = 3))

      iCov2 <- iCov[order(-iCov$id), ]

      tmp1 <- expect_warning(rxSolve(mod, d, iCov = iCov, keep = "wt"))

      tmp2 <- expect_warning(rxSolve(mod, d, iCov = iCov2, keep = "wt"))

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(tmp1$cwt, tmp2$wt)
      expect_equal(tmp2$cwt, tmp2$wt)

      ## Now try letters
      d <- readRDS("theoSd.rds")
      d <- d[, names(d) != "WT"]
      d$ID <- letters[d$ID]

      iCov <- data.frame(id = 1:13, wt = 70 + rnorm(13, sd = 0.01))

      iCov2 <- iCov[order(-iCov$id), ]

      expect_error(rxSolve(mod, d, iCov = iCov))

      expect_error(rxSolve(mod, d, iCov = iCov2))

      iCov$id <- letters[iCov$id]
      iCov2$id <- letters[iCov2$id]

      tmp1 <- expect_warning(rxSolve(mod, d, iCov = iCov, keep = "wt"))
      tmp2 <- expect_warning(rxSolve(mod, d, iCov = iCov2, keep = "wt"))

      expect_equal(data.frame(tmp1), data.frame(tmp2))
      expect_equal(data.frame(tmp1$params), data.frame(tmp2$params))

      expect_equal(tmp1$cwt, tmp2$wt)
      expect_equal(tmp2$cwt, tmp2$wt)

      ## Last case, though rare
      iCov <- data.frame(id = 1:12, wt = 70 + rnorm(12, sd = 0.01))
      iCov$ID <- letters[iCov$id]

      expect_error(rxSolve(mod, d, iCov = iCov, keep = "wt"))

      iCov2 <- iCov[order(-iCov$id), ]

      expect_error(rxSolve(mod, d, iCov = iCov2, keep = "wt"))
    })

    test_that("id is retained as an integer", {
      theoSd <- readRDS("theoSd.rds")
      d <- theoSd[, names(theoSd) != "EVID"]
      d <- d[d$ID != 10, ]

      d$ID <- as.integer(d$ID)

      mod <- rxode2({
        tka <- 1
        tcl <- 2
        tv <- 3
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        cp <- linCmt()
      })

      tmp <- rxSolve(mod, d)

      expect_true(is.integer(d$ID))
      expect_true(is.integer(tmp$id))

      ## Now integerish
      d$ID <- as.numeric(d$ID)

      tmp <- rxSolve(mod, d)

      expect_true(is.numeric(d$ID))
      expect_true(is.integer(tmp$id))

      d$ID <- as.numeric(d$ID) + 0.1

      ## Now non integerish

      tmp <- rxSolve(mod, d)

      expect_true(is.numeric(d$ID))
      expect_true(is.factor(tmp$id))
    })
  },
  test = "lvl2"
)
