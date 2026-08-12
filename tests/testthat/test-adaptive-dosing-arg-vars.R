rxTest({

  # The adaptive dosing statements consume their arguments as raw text and skip
  # the parse-tree children, so an identifier that appears *only* there is never
  # registered as a model variable.  That used to surface as generated C
  # referencing an undeclared symbol, reported as a compiler failure that looks
  # like a broken toolchain.  It is now a parse-time error.  See #1231.

  test_that("a covariate used only as a dosing argument is a parse error", {
    expect_error(rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 0) infuseDur(DOSE, TINF, cmt = 1, ii = TAU, addl = 3)
      })
    }))
  })

  test_that("every adaptive dosing function reports an undeclared argument", {
    mk <- function(call) {
      f <- function() {
        ini({ ka <- 0.8; cl <- 15; v <- 100 })
        model({
          d/dt(depot) <- -ka * depot
          d/dt(central) <- ka * depot - (cl / v) * central
          cp <- central / v
          if (t <= 0) bolus(1, cmt = 1)
        })
      }
      body(f)[[3]][[2]][[5]][[3]] <- str2lang(call)
      f
    }
    for (cur in c("bolus(DOSE, cmt=1)",
                  "infuse(DOSE, RATE, cmt=1)",
                  "infuseDur(DOSE, TINF, cmt=1)",
                  "replace(DOSE, cmt=1)",
                  "multiply(DOSE, cmt=1)",
                  "phantom(DOSE, cmt=1)",
                  "evid_(t, 1, DOSE, 1, 0, 24, 3, 0)")) {
      expect_error(rxode2(mk(cur)), info = cur)
    }
  })

  test_that("assigning the covariate first is accepted, wherever it is assigned", {
    # assigned above the dosing statement
    expect_error(rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        amtI <- DOSE
        durI <- TINF
        iiI <- TAU
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 0) infuseDur(amtI, durI, cmt = 1, ii = iiI, addl = 3)
      })
    }), NA)
    # ...and below it, since the check runs once the whole model is parsed
    expect_error(rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 0) infuseDur(amtI, durI, cmt = 1)
        amtI <- DOSE
        durI <- TINF
      })
    }), NA)
  })

  test_that("literals, builtins, states and THETA[] are not flagged", {
    expect_error(rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        # 1e-7 must not be read as an identifier 'e'
        if (t <= 0) evid_(t + 1e-7, 1, 100 * exp(-0.5), depot, 0, 24, 3, 0)
      })
    }), NA)
    # a compartment name as the amount, and as the cmt
    expect_error(rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 12) replace(depot * 0 + 25, depot)
      })
    }), NA)
    expect_error(rxode2(function() {
      model({
        d/dt(depot) <- -THETA[1] * depot
        cp <- depot
        if (t <= 0) bolus(THETA[2], cmt = depot)
      })
    }), NA)
  })

  test_that("the error names the variable, the argument and the function", {
    expect_error(rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        cp <- depot
        if (t <= 0) bolus(DOSE, cmt = 1)
      })
    }))
    msg <- utils::capture.output(
      try(rxode2(function() {
        ini({ ka <- 0.8; cl <- 15; v <- 100 })
        model({
          d/dt(depot) <- -ka * depot
          cp <- depot
          if (t <= 0) bolus(DOSE, cmt = 1)
        })
      }), silent = TRUE))
    msg <- paste(msg, collapse = "\n")
    expect_match(msg, "'DOSE'")
    expect_match(msg, "'amt'")
    expect_match(msg, "'bolus\\(\\)'")
  })

})
