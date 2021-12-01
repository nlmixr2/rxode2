rxodeTest(
  {
    context("mdv tests")
    test_that("mdv means EVID=2 when amt=0", {
      theoSd <- readRDS("theoSd.rds")
      d <- theoSd[, names(theoSd) != "EVID"]
      d$MDV <- ifelse(d$AMT == 0, 0, 1)
      d <- d[, names(d) != "WT"]

      d2 <- expand.grid(ID = 1:12, TIME = c(0.125, 25), DV = 0, AMT = 0, CMT = 2, MDV = 1)

      d <- rbind(d, d2)

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

      expect_true(any(tmp$time == 0.125))
      expect_true(any(tmp$time == 25))
    })
  },
  test = "lvl2"
)
