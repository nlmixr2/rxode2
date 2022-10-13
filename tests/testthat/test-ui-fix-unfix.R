rxTest({
  test_that("Now test fix and unfixing", {

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl + eta.v ~ c(0.3,
                           0.01, 0.1)
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        f(depot) <- 3
        cp <- center / v
        cp2 <- cp * cl
        cp ~ add(add.err)
      })
    }

    f2 <- one.compartment %>% ini(tka=fix)
    w <- which(f2$iniDf$name == "tka")
    expect_true(f2$iniDf$fix[w])

    f3 <- f2 %>% ini(tka=unfix)
    w <- which(f3$iniDf$name == "tka")
    expect_false(f3$iniDf$fix[w])

    f2 <- one.compartment %>% ini(tka=fix(3))
    w <- which(f2$iniDf$name == "tka")
    expect_true(f2$iniDf$fix[w])

    f3 <- f2 %>% ini(tka=unfix(3))
    w <- which(f3$iniDf$name == "tka")
    expect_false(f3$iniDf$fix[w])


    # should fix the entire block
    f2 <- one.compartment %>% ini(eta.cl=fix)
    expect_equal(f2$iniDf[!is.na(f2$iniDf$neta1), "fix"], c(FALSE, TRUE, TRUE, TRUE))

    # should unfix the entire block
    f3 <- f2 %>% ini(eta.cl=unfix)
    expect_equal(f3$iniDf[!is.na(f3$iniDf$neta1), "fix"], c(FALSE, FALSE, FALSE, FALSE))

    f2 <- one.compartment %>% ini(fix(eta.cl))
    expect_equal(f2$iniDf[!is.na(f2$iniDf$neta1), "fix"], c(FALSE, TRUE, TRUE, TRUE))

    f3 <- f2 %>% ini(unfixed(eta.cl))
    expect_equal(f3$iniDf[!is.na(f3$iniDf$neta1), "fix"], c(FALSE, FALSE, FALSE, FALSE))

  })
})
