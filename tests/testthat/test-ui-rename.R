rxTest({
  test_that("rename for ui makes sense", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- exp(3.45) # log V
        ## the label("Label name") works with all models
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd)
      })
    }


    expect_error(rxRename(ocmt, d/dt(cp1) ~ cp))
    expect_error(rxRename(ocmt, cp1 ~ cp))
    expect_error(rxRename(ocmt, cp1=3))
    expect_error(rxRename(ocmt, cp=cp1))
    expect_error(rxRename(ocmt, cp2=cp1))

    f <- rxRename(ocmt, cpParent=cp, parentDepot=depot, parentCentral=center, parentKa=ka,
                  parentTka=tka, parent.sd=add.sd)

    expect_true("parent.sd" %in% f$iniDf$name)
    expect_true("parentTka" %in% f$iniDf$name)
    expect_true("parentKa" %in% f$mv0$lhs)
    expect_true("parentCentral" %in% f$mv0$state)
    expect_true("parentDepot" %in% f$mv0$state)
    expect_true("cpParent" %in% f$mv0$lhs)
    expect_true("cpParent" %in% f$predDf$var)
    expect_true("cpParent" %in% f$predDf$cond)

    f <- dplyr::rename(ocmt, cp.parent=cp)
    expect_true("cp.parent" %in% f$mv0$lhs)
    expect_true("cp.parent" %in% f$predDf$var)
    expect_true("cp.parent" %in% f$predDf$cond)

    f2 <- dplyr::rename(f, depot.parent=depot)
    expect_true("depot.parent" %in% f2$mv0$state)

    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- exp(3.45) # log V
        ## the label("Label name") works with all models
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        cp ~ add(add.sd) | parent
      })
    }

    f <- rxRename(ocmt, metabolite=parent)

    expect_true("metabolite" %in% f$predDf$cond)

    # now test that functions remain intact

    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- exp(3.45) # log V
        lag <- 1
        ## the label("Label name") works with all models
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        lag(depot) <- lag
        cp = center / v
        cp ~ add(add.sd) | parent
      })
    }

    f <- dplyr::rename(ocmt, alag=lag)

    expect_equal(f$lstExpr[[6]], quote(lag(depot) <- alag))

    # now test that d/dt(item) remains intact


    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- exp(3.45) # log V
        d <- 1
        ## the label("Label name") works with all models
        add.sd <- 0.7
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        lag(depot) <- d
        cp = center / v
        cp ~ add(add.sd) | parent
      })
    }

    f <- dplyr::rename(ocmt, alag=d)
    expect_equal(f$lstExpr[[6]], quote(lag(depot) <- alag))

  })
})
