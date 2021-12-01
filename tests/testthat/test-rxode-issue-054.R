rxode2Test(
  {
    context("Conditional statements")
    ## else if is actually already supported...
    test_that("else if", {
      m <- rxode2({
        if (cnd <= 1) {
          a <- 1.0
        } else if (cnd <= 2) {
          a <- 2.0
        } else if (cnd <= 3) {
          a <- 3.
        } else {
          a <- 100
        }
        tmp <- cnd
      })

      ## The prefered syntax is only if / else but it still works...
      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)
    })

    test_that("ifelse", {
      m <- rxode2({
        a <- ifelse(cnd <= 1, 1.0, ifelse(cnd <= 2, 2, ifelse(cnd <= 3, 3, 100)))
        tmp <- cnd
      })

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)
    })

    test_that("embedded logical expressions", {
      m <- rxode2({
        a <- (cnd == 1) * 1.0 + (cnd == 2) * 2 + (cnd == 3) * 3
        tmp <- cnd
      })

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 0)
    })

    test_that("ifelse with assignments", {
      m <- rxode2({
        ifelse(cnd <= 1, a = 1.0, a = 2.0)
        tmp <- cnd
      })

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      m <- rxode2({
        ifelse(cnd <= 1, a <- 1.0, a <- 2.0)
        tmp <- cnd
      })

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)
    })
    ##
    context("Pruning checks")
    test_that("prune checks", {
      tmp <- "C2=centr/V;\nC3=peri/V2;\nd/dt(depot)=-KA*depot;\nd/dt(centr)=KA*depot-CL*C2-Q*C2+Q*C3;\nd/dt(peri)=Q*C2-Q*C3;\nC4=CMT;\nif(CMT==1){\nprd=depot;\n}\nif(CMT==2){\nprd=centr;\n}\nif(CMT==3){\nprd=peri;\n}\n"
      expect_equal(rxPrune(tmp), "C2=centr/V\nC3=peri/V2\nd/dt(depot)=-KA*depot\nd/dt(centr)=KA*depot-CL*C2-Q*C2+Q*C3\nd/dt(peri)=Q*C2-Q*C3\nC4=CMT\nprd=(CMT==1)*(depot)\nprd=(CMT==2)*(centr)+(1-((CMT==2)))*(prd)\nprd=(CMT==3)*(peri)+(1-((CMT==3)))*(prd)")

      ## Advanced context pruining:
      m <- rxode2({
        if (cnd <= 1) {
          a <- 1.0
        } else if (cnd <= 2) {
          a <- 2.0
        } else if (cnd <= 3) {
          a <- 3.
        } else {
          a <- 100
        }
        tmp <- cnd
      })

      m <- rxode2(rxPrune(m))

      ## The prefered syntax is only if / else but it still works...
      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)

      m <- rxode2(rxOptExpr(rxNorm(m)))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)

      m <- rxode2({
        a <- 100
        if (cnd <= 1) {
          a <- 1.0
        }
        if (cnd > 1 && cnd <= 2) {
          a <- 2.0
        }
        if (cnd > 2 && cnd <= 3) {
          a <- 3.
        }
        tmp <- cnd
      })

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)

      m <- rxode2(rxPrune(m))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)

      m <- rxode2(rxOptExpr(rxNorm(m)))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)

      m <- rxode2({
        a <- ifelse(cnd <= 1, 1.0, ifelse(cnd <= 2, 2, ifelse(cnd <= 3, 3, 100)))
        tmp <- cnd
      })

      m <- rxode2(rxPrune(m))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)

      m <- rxode2(rxOptExpr(rxNorm(m)))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)


      m <- rxode2({
        ifelse(cnd <= 1, a = 1.0, a = 2.0)
        tmp <- cnd
      })

      m <- rxode2(rxPrune(m))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      m <- rxode2(rxOptExpr(rxNorm(m)))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      m <- rxode2({
        ifelse(cnd <= 1, a <- 1.0, a <- 2.0)
        tmp <- cnd
      })

      m <- rxode2(rxPrune(m))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      m <- rxode2(rxOptExpr(rxNorm(m)))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      m <- rxode2({
        a <- (cnd == 1) * 1.0 + (cnd == 2) * 2 + (cnd == 3) * 3
        tmp <- cnd
      })

      m <- rxode2(rxPrune(m))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 0)

      m <- rxode2(rxOptExpr(rxNorm(m)))

      tmp <- rxSolve(m, c(cnd = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 0)

      m <- rxode2({
        if (cnd <= 1) {
          a <- theta[1]
        } else if (cnd <= 2) {
          a <- 2.0
        } else if (cnd <= 3) {
          a <- 3.
        } else {
          a <- 100
        }
        tmp <- cnd
      })

      m <- rxode2(rxOptExpr(rxPrune(m)))

      tmp <- rxSolve(m, c(cnd = 1, `THETA[1]` = 1), et(0.1))
      expect_equal(tmp$tmp, 1)
      expect_equal(tmp$a, 1)

      tmp <- rxSolve(m, c(cnd = 2, `THETA[1]` = 1), et(0.1))
      expect_equal(tmp$tmp, 2)
      expect_equal(tmp$a, 2)

      tmp <- rxSolve(m, c(cnd = 3, `THETA[1]` = 1), et(0.1))
      expect_equal(tmp$tmp, 3)
      expect_equal(tmp$a, 3)

      tmp <- rxSolve(m, c(cnd = 4, `THETA[1]` = 1), et(0.1))
      expect_equal(tmp$tmp, 4)
      expect_equal(tmp$a, 100)
    })

    context("cimet pruning checks")

    test_that("cimet pruning checks", {
      cimet.1 <- rxode2({
        dose <- 300
        eta.ka <- 0
        eta.cl <- 0
        eta.v <- 0
        eta.tgap <- 0
        eta.rkeb <- 0
        add.err <- 0
        tka <- log(0.5)
        tcl <- log(60)
        tv <- log(25)
        ttgap <- log(2)
        trkeb <- log(0.5)
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        tgap <- exp(ttgap + eta.tgap)
        rkeb <- exp(trkeb + eta.rkeb)
        #
        bile <- 1
        if (t < tgap) {
          bile <- 0
        }
        #
        ha <- exp(-(cl / v) * tgap) / ((cl / v) - ka)
        hb <- exp(-ka * tgap) * (cl / v) / ka / ((cl / v) - ka)
        tote <- ka * dose * (1 / ka + ha - hb)
        #
        hc <- exp(-(cl / v) * t) - exp(-ka * t)
        timh <- bile * (t - tgap)
        hd <- exp(-(cl / v) * timh) - exp(-ka * timh)
        #
        cp <- dose / v * ka / (ka - (cl / v)) * hc + bile * rkeb * tote / v * ka / (ka - (cl / v)) * hd
        #
        cp <- cp + add.err # + prop(prop.err)
      })

      et <- et(seq(0, 24, length.out = 90))

      s1 <- rxSolve(cimet.1, et)

      cimet.2 <- rxode2(rxPrune(cimet.1))

      s2 <- rxSolve(cimet.2, et)

      expect_equal(s1$cp, s2$cp)

      cimet.3 <- rxode2(rxOptExpr(rxPrune(cimet.1)))

      s3 <- rxSolve(cimet.3, et)

      expect_equal(s1$cp, s3$cp)
    })
  },
  test = "parsing"
)
