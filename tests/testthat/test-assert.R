if (!.Call(`_rxode2_isIntel`)) {
  test_that("assertRxUiRandomOnIdOnly", {
    one.cmt <- function() {
      ini({
        tka <- 0.45; label("Ka")
        tcl <- log(c(0, 2.7, 100)); label("Cl")
        tv <- 3.45; label("V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    expect_equal(
      assertRxUiRandomOnIdOnly(one.cmt),
      as.rxUi(one.cmt)
    )
  })

  test_that("assert/test Compartment/Var", {

    mod <- function() {
      ini({
        cl <- 1.1
        v <- 20
        ka <- 1.5
      })
      model({
        d/dt(depot) <- -ka*depot
        d/dt(central) <- ka*depot - (cl/v)*central
        f(central) <- bioav
        if (mode == 1) rate(central) <- rat2
        if (mode == 2) dur(central) <- dur2
        cp <- central/(v/1000)
      })
    }

    mod <- mod()

    expect_error(assertCompartmentExists(mod, "central"), NA)
    expect_error(assertCompartmentNew(mod, "central"),
                 "compartment 'central' already exists in the model")
    expect_true(testCompartmentExists(mod, "central"))
    expect_error(assertCompartmentExists(mod, "funny"),
                 "'funny' compartment is not in the model")
    expect_error(assertCompartmentNew(mod, "funny"), NA)
    expect_false(testCompartmentExists(mod, "funny"))
    expect_error(assertCompartmentExists(mod, funny),
                 "'funny' compartment is not in the model")
    expect_error(assertCompartmentNew(mod, funny), NA)
    expect_false(testCompartmentExists(mod, funny))
    expect_error(assertCompartmentExists(mod, central), NA)
    expect_error(assertCompartmentNew(mod, central),
                 "compartment 'central' already exists in the model")
    expect_true(testCompartmentExists(mod, central))

    # now variables
    expect_error(assertVariableExists(mod, "cp"), NA)
    expect_error(assertExists(mod, "cp"), NA)
    expect_true(testVariableExists(mod, "cp"))
    expect_true(testExists(mod, "cp"))
    expect_error(assertVariableNew(mod, "cp"),
                 "variable 'cp' is already in the model")

    expect_error(assertVariableExists(mod, cp), NA)
    expect_true(testVariableExists(mod, cp))
    expect_error(assertVariableNew(mod, cp),
                 "variable 'cp' is already in the model")

    expect_error(assertVariableExists(mod,"funny"),
                 "variable 'funny' is not in the model")
    expect_false(testVariableExists(mod, "funny"))
    expect_error(assertVariableNew(mod, "funny"),NA)

    expect_error(assertVariableExists(mod,funny),
                 "variable 'funny' is not in the model")
    expect_false(testVariableExists(mod, funny))
    expect_error(assertVariableNew(mod, funny),NA)
  })

  test_that("assertCompartmentName", {
    expect_equal(assertCompartmentName("x"), "x")
    expect_equal(assertCompartmentName("x.y"), "x.y")
    expect_equal(assertCompartmentName("x_y"), "x_y")
    # This is a valid R variable name, but it does not work with rxode2
    ## f <- rxode2("d/dt(.) = . + 3")
    ## rxode2 model syntax error:
    ## ===================================================
    ## rxode2 syntax error:
    ## :001: d/dt(.) = . + 3
    ##            ^
    expect_error(assertCompartmentName("."))
    expect_error(assertCompartmentName("9"))
    expect_error(assertCompartmentName(9))
    expect_error(assertCompartmentName(NULL))
    expect_error(assertCompartmentName(c("A", "B")))
  })

  test_that("assertVariableName", {
    expect_equal(assertVariableName("x"), "x")
    expect_equal(assertVariableName("x.y"), "x.y")
    expect_equal(assertVariableName("x_y"), "x_y")
    # This is a valid R variable name, but it does not work
    # with rxode2
    # rxode2 model syntax error:
    # ===================================================
    # rxode2 syntax error:
    # :001: . = 3
    #       ^

    expect_error(assertVariableName("."))
    expect_error(assertVariableName("9"))
    expect_error(assertVariableName(9))
    expect_error(assertVariableName(NULL))
    expect_error(assertVariableName(c("A", "B")))
  })

  test_that("assertParameterValue", {
    expect_equal(assertParameterValue(1), 1)
    expect_equal(assertParameterValue(-9), -9)
    expect_equal(assertParameterValue(0), 0)
    expect_error(assertParameterValue(Inf))
    expect_error(assertParameterValue(NA))
  })
}
