if (!.Call(`_rxode2_isIntel`)) {
  test_that("ini piping string", {
    one.compartment <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        F(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    tmp <- rxode2(one.compartment)

    theta <- tmp$theta

    fixItems <- setNames(rep("fix",length(theta)), names(theta))
    suppressMessages(
      ui <- tmp %>% ini(fixItems)
    )

    expect_true(all(ui$iniDf$fix[!is.na(ui$iniDf$ntheta)]))

    unfixItems <- setNames(rep("unfix",length(theta)), names(theta))

    suppressMessages(
      ui2 <- ui %>% ini(unfixItems)
    )

    expect_true(all(!ui2$iniDf$fix[!is.na(ui2$iniDf$ntheta)]))
  })
}
