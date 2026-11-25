rxTest({
  # Add transit ----

  test_that("addTransit adds transit compartment", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(3)
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- addTransit(modelTest, 1))
    # check for lktr1 in ini block
    temp <- rxode2::assertRxUi(modelUpdate)
    temp2 <- temp$iniDf
    expect_equal("lktr" %in% temp2$name, TRUE)

    # check for ktr in model block
    suppressMessages(kaLine <- modelExtract(modelUpdate, "ktr", lines = TRUE))
    expect_equal(grepl("\\s*^ktr", kaLine), TRUE)
  })

  # Test if the function throws an error when invalid input for 'central' is provided
  test_that("addTransit throws an error with invalid 'central'", {
    expect_error(addTransit(readModelDb("PK_2cmt_des"), 3, "cent", "depot"), "'cent' compartment is not in the model")
  })

  # Test if the function throws an error when invalid input for 'transit' is provided
  test_that("addTransit throws an error with invalid 'transit'", {
    expect_error(addTransit(readModelDb("PK_2cmt_des"), -1), "Assertion on 'ntransit' failed: Element 1 is not >= 1")
  })

  # Test if the function adds transit compartments correctly when 'depot' is present
  test_that("addTransit adds transit compartments correctly with 'depot'", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(3)
    modelTest <- rxode2::assertRxUi(modelTest)
    modelUpdate <- addTransit(modelTest, 3)
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("transit1" %in% mv$state, TRUE)
    expect_equal("transit2" %in% mv$state, TRUE)
    expect_equal("transit3" %in% mv$state, TRUE)
  })

  # Test if the function adds transit compartments  when 'depot' is not present
  test_that("addTransit does not add transit compartments  without 'depot'", {
    expect_warning(addTransit(readModelDb("PK_2cmt_no_depot"), 1), "'depot' added to model for transit model")
  })

  test_that("extreme model cases", {
    f <- function() {
      model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    expect_error(f |> addTransit(4), NA)
    expect_warning(f |> addTransit(4), NA)

    f <- function() {
      ini({
      e ~ 0.1
    })
      model({
      ka <- exp(e)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    omega <- f$omega

    expect_error(f |> addTransit(4), NA)
    expect_warning(f |> addTransit(4), NA)

    tmp <- f |> addTransit(4)

    expect_equal(omega, tmp$omega)
  })

  # Remove transit ----

  test_that("removeTransit throws error in model with no transit compartment", {
    expect_error(
      # nolint: line_length_linter.
      removeTransit(readModelDb("PK_2cmt_des"), "central", transit = "transit"),
      "Assertion on 'ntransit' failed: Must be of type 'integerish', not 'character'"
    )
    expect_error(
      removeTransit(modelTest, ntransit = "A"),
      regexp = "Assertion on 'ntransit' failed: Must be of type 'integerish', not 'character'",
      fixed = TRUE
    )
  })

  test_that("removeTransit removes transit compartment", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(1)
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeTransit(modelTest, central = "central", depot = "depot", transit = "transit"))
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("transit1" %in% mv$state, FALSE)
  })

  test_that("removeTransit removes ktr in model block", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(1)
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeTransit(modelTest, central = "central", depot = "depot", transit = "transit"))
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("ktr" %in% mv$lhs, FALSE)
  })

  test_that("removeTransit modifies ODE for central compartment", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(1)
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeTransit(modelTest, central = "central", depot = "depot"))
    suppressMessages(centralLine <- modelExtract(modelUpdate, "d/dt(central)", lines = TRUE))
    expect_equal(grepl(".*<-\\s*", centralLine), TRUE)
  })

  test_that("removeTransit removes lktr in ini block", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(1)
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeTransit(modelTest, central = "central", depot = "depot"))
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("lktr1" %in% mv$params, FALSE)
  })

  test_that("remove some but not all compartments", {
    modelTest <- readModelDb("PK_1cmt_des") |> addTransit(2)

    # Expected blocks only -- avoids comparing the metadata preamble
    # (description, reference, units, dosing) which is not what this test checks.
    expected <- function() {
      ini({
      lka <- 0.45
      label("Absorption rate (Ka)")
      lcl <- 1
      label("Clearance (CL)")
      lvc <- 3.45
      label("Central volume of distribution (V)")
      propSd <- c(0, 0.5)
      label("Proportional residual error (fraction)")
      lktr <- 0.1
      label("First order transition rate (ktr)")
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      ktr <- exp(lktr)
      d / dt(depot) <- -ktr * depot
      d / dt(transit1) <- ktr * depot - ka * transit1
      d / dt(central) <- ka * transit1 - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    f <- as.function(removeTransit(modelTest, ntransit = 1))
    expect_equal(findBlock(f, "ini"), findBlock(expected, "ini"))
    expect_equal(findBlock(f, "model"), findBlock(expected, "model"))
    expect_warning(
      removeTransit(modelTest, ntransit = 4),
      regexp = "reset ntransit to 2"
    )
  })

  test_that("extreme model cases", {
    f <- function() {
      model({
      ktr <- exp(lktr)
      d / dt(depot) <- -ktr * depot
      d / dt(transit1) <- ktr * depot - ktr * transit1
      d / dt(transit2) <- ktr * transit1 - ktr * transit2
      d / dt(transit3) <- ktr * transit2 - ktr * transit3
      d / dt(transit4) <- ktr * transit3 - ka * transit4
      d / dt(central) <- ka * transit4 - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    expect_error(f |> removeTransit(), NA)
    expect_warning(f |> removeTransit(), NA)
    tmp <- f |> removeTransit()
    expect_true(length(tmp$iniDf$name) == 0)

    f <- function() {
      ini({
      lktr <- 1
    })
      model({
      ktr <- exp(lktr)
      d / dt(depot) <- -ktr * depot
      d / dt(transit1) <- ktr * depot - ktr * transit1
      d / dt(transit2) <- ktr * transit1 - ktr * transit2
      d / dt(transit3) <- ktr * transit2 - ktr * transit3
      d / dt(transit4) <- ktr * transit3 - ka * transit4
      d / dt(central) <- ka * transit4 - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    expect_error(f |> removeTransit(), NA)
    expect_warning(f |> removeTransit(), NA)
    tmp <- f |> removeTransit()
    expect_true(length(tmp$iniDf$name) == 0)

    f <- function() {
      ini({
      lktr ~ 1
    })
      model({
      ktr <- exp(lktr)
      d / dt(depot) <- -ktr * depot
      d / dt(transit1) <- ktr * depot - ktr * transit1
      d / dt(transit2) <- ktr * transit1 - ktr * transit2
      d / dt(transit3) <- ktr * transit2 - ktr * transit3
      d / dt(transit4) <- ktr * transit3 - ka * transit4
      d / dt(central) <- ka * transit4 - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    expect_error(f |> removeTransit(), NA)
    expect_warning(f |> removeTransit(), NA)
    tmp <- f |> removeTransit()
    expect_true(length(tmp$iniDf$name) == 0)

    f <- function() {
      ini({
      ka <- 1
      lktr ~ 1
    })
      model({
      ktr <- exp(lktr)
      d / dt(depot) <- -ktr * depot
      d / dt(transit1) <- ktr * depot - ktr * transit1
      d / dt(transit2) <- ktr * transit1 - ktr * transit2
      d / dt(transit3) <- ktr * transit2 - ktr * transit3
      d / dt(transit4) <- ktr * transit3 - ka * transit4
      d / dt(central) <- ka * transit4 - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    expect_error(f |> removeTransit(), NA)

    tmp <- f |> removeTransit()

    expect_equal(tmp$iniDf$name, "ka")
    expect_equal(tmp$iniDf$ntheta, 1)

    f <- function() {
      ini({
      ka ~ 1
      lktr ~ 1
    })
      model({
      ktr <- exp(lktr)
      d / dt(depot) <- -ktr * depot
      d / dt(transit1) <- ktr * depot - ktr * transit1
      d / dt(transit2) <- ktr * transit1 - ktr * transit2
      d / dt(transit3) <- ktr * transit2 - ktr * transit3
      d / dt(transit4) <- ktr * transit3 - ka * transit4
      d / dt(central) <- ka * transit4 - kel * central
      Cc <- central / vc
    })
    }

    f <- rxode2(f)

    expect_error(f |> removeTransit(), NA)

    tmp <- f |> removeTransit()

    expect_equal(tmp$iniDf$name, "ka")
    expect_equal(tmp$iniDf$neta1, 1)
  })

  # Issue 77 / 78 -- addTransit preserves source order of existing lines ----

  test_that("addTransit preserves endpoint placement when d/dt(central) is the last line", {
    m <- function() {
      ini({
      lka <- 0
      lcl <- 1
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      central ~ prop(propSd)
      d / dt(depot) <- -exp(lka) * depot
      d / dt(central) <- exp(lka) * depot - exp(lcl) / exp(lvc) * central
    })
    }
    res <- addTransit(m, 2)
    expect_s3_class(res, "rxUi")
    expect_true("lktr" %in% res$iniDf$name)
    lines <- vapply(res$lstExpr, function(e) paste(deparse(e), collapse = " "), character(1))
    normalized <- gsub("\\s+", " ", trimws(lines))
    # Original endpoint must stay at its original position (first line).
    expect_true(grepl("^central ~ prop", normalized[[1]]))
    # ktr helper and depot ODE should sit adjacent to the now-modified depot
    # line, not at the top of the model.
    wKtr <- which(normalized == "ktr <- exp(lktr)")
    wDepot <- which(grepl("^d/dt\\(depot\\)", normalized))
    expect_length(wKtr, 1L)
    expect_length(wDepot, 1L)
    expect_equal(wKtr + 1L, wDepot)
    # Two transit compartments were added.
    expect_true(any(grepl("^d/dt\\(transit1\\)", normalized)))
    expect_true(any(grepl("^d/dt\\(transit2\\)", normalized)))
  })

  test_that("addTransit preserves source order when an assignment line sits above d/dt(depot)", {
    m <- function() {
      ini({
      lka <- 0
      lcl <- 1
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      kel <- exp(lcl) / exp(lvc)
      d / dt(depot) <- -exp(lka) * depot
      d / dt(central) <- exp(lka) * depot - kel * central
      Cc <- central / exp(lvc)
      Cc ~ prop(propSd)
    })
    }
    res <- addTransit(m, 2)
    expect_s3_class(res, "rxUi")
    lines <- vapply(res$lstExpr, function(e) paste(deparse(e), collapse = " "), character(1))
    normalized <- gsub("\\s+", " ", trimws(lines))
    # Every pre-existing line (except d/dt(depot) whose RHS was rewritten and
    # d/dt(central) which the first splice modifies) stays in its original
    # relative order.
    kept <- c("kel <- exp(lcl)/exp(lvc)", "Cc <- central/exp(lvc)", "Cc ~ prop(propSd)")
    pos <- vapply(kept, function(k) match(TRUE, normalized == k), integer(1))
    expect_false(any(is.na(pos)))
    expect_true(all(diff(pos) > 0))
    # Endpoint remains last.
    expect_true(grepl("^Cc ~ prop", normalized[[length(normalized)]]))
    # ktr helper sits immediately before d/dt(depot), not at the top of the model.
    wKtr <- which(normalized == "ktr <- exp(lktr)")
    wDepot <- which(grepl("^d/dt\\(depot\\)", normalized))
    expect_equal(wKtr + 1L, wDepot)
  })
})
