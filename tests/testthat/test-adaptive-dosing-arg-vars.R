rxTest({
  # The adaptive dosing statements consume their arguments as raw text and skip
  # the parse-tree children, so an identifier that appears *only* there used to
  # never be registered as a model variable, and the generated C referenced an
  # undeclared symbol (#1231).  Such an identifier is now registered as a model
  # input, as if it were on a right-hand side, so an argument can be any
  # expression -- `bolus(DOSE * 30)` or `infuseDur(100 * bsa, 1, central)`.

  test_that("a covariate used only as a dosing argument becomes a model input", {
    .u <- rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 0) infuseDur(DOSE, TINF, cmt = 1, ii = TAU, addl = 3)
      })
    })
    expect_true(all(c("DOSE", "TINF", "TAU") %in% .u$mv0$params))
    expect_true(all(c("DOSE", "TINF", "TAU") %in% .u$allCovs))
  })

  test_that("an expression argument doses like the same expression assigned first", {
    .expr <- rxode2({
      d/dt(depot) <- -0.1 * depot
      if (t >= 1 && t < 1.5) bolus(DOSE * 30 + b0 / 2, 1, 0, 0, 0)
    })
    .pre <- rxode2({
      d/dt(depot) <- -0.1 * depot
      amtI <- DOSE * 30 + b0 / 2
      if (t >= 1 && t < 1.5) bolus(amtI, 1, 0, 0, 0)
    })
    .ev <- et(seq(0, 5, by = 1))
    .p <- c(DOSE = 2, b0 = 8)
    .a <- rxSolve(.expr, .ev, params = .p)
    .b <- rxSolve(.pre, .ev, params = .p)
    expect_equal(.a$depot, .b$depot)
    expect_equal(.a$depot[.a$time == 2], 64 * exp(-0.1 * 1), tolerance = 1e-4)
  })

  test_that("every adaptive dosing function accepts an argument used only there", {
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
    for (cur in c(
      "bolus(DOSE * 2, cmt=1)",
      "infuse(DOSE, RATE / 2, cmt=1)",
      "infuseDur(DOSE, TINF, cmt=1)",
      "replace(DOSE, cmt=1)",
      "multiply(DOSE, cmt=1)",
      "phantom(DOSE, cmt=1)",
      "evid_(t + TAU, 1, DOSE, 1, 0, 24, 3, 0)"
    )) {
      .u <- rxode2(mk(cur))
      expect_true("DOSE" %in% .u$mv0$params, info = cur)
    }
  })

  test_that("assigning the covariate first is accepted, wherever it is assigned", {
    # assigned above the dosing statement
    expect_error(
      rxode2(function() {
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
    }),
      NA
    )
    # ...and below it, since the check runs once the whole model is parsed
    expect_error(
      rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 0) infuseDur(amtI, durI, cmt = 1)
        amtI <- DOSE
        durI <- TINF
      })
    }),
      NA
    )
  })

  test_that("literals, builtins, states and THETA[] are not flagged", {
    expect_error(
      rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        # 1e-7 must not be read as an identifier 'e'
        if (t <= 0) evid_(t + 1e-7, 1, 100 * exp(-0.5), depot, 0, 24, 3, 0)
      })
    }),
      NA
    )
    # a compartment name as the amount, and as the cmt
    expect_error(
      rxode2(function() {
      ini({ ka <- 0.8; cl <- 15; v <- 100 })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - (cl / v) * central
        cp <- central / v
        if (t <= 12) replace(depot * 0 + 25, depot)
      })
    }),
      NA
    )
    expect_error(
      rxode2(function() {
      model({
        d/dt(depot) <- -THETA[1] * depot
        cp <- depot
        if (t <= 0) bolus(THETA[2], cmt = depot)
      })
    }),
      NA
    )
  })

  test_that("the model echoed after a post-parse error keeps its first character", {
    .out <- capture.output(try(
      rxModelVars("cl <- 3\nv <- 10\ncp <- linCmt()\ny <- tad(depot)"),
      silent = TRUE
    ))
    expect_true(any(grepl("^:001: cl <- 3$", .out)))
    expect_true(any(grepl("^:004: y <- tad\\(depot\\)$", .out)))
    # a leading blank line keeps the numbering
    .out <- capture.output(try(
      rxModelVars("\ncl <- 3\nv <- 10\ncp <- linCmt()\ny <- tad(depot)"),
      silent = TRUE
    ))
    expect_true(any(grepl("^:002: cl <- 3$", .out)))
  })
})
