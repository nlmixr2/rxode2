## A solve's draws (residuals, thetas, per-study lists) are published in
## .rxModels for that solve to read and moved into its result when it
## finishes; a solve that stops first must not leave them for the next one.

.drawnModel <- function() {
  rxode2({
    cl <- tcl
    v <- 30
    d / dt(central) <- -cl / v * central
    cp <- central / v
    y <- cp + err
  })
}

.drawnEvents <- function() {
  et(amt = 100) |> et(seq(1, 24, by = 1))
}

# The draws left in .rxModels
.drawnLeft <- function() {
  intersect(.rxSolveDrawn, ls(rxModels_(), all.names = TRUE))
}

# A simulation that draws the residuals and simulated thetas, then stops:
# its parameters lack tcl
.drawnFailingSolve <- function(m) {
  try(
    suppressWarnings(rxSolve(
      m,
      c(tcl0 = 3),
      .drawnEvents(),
      sigma = lotri::lotri(err ~ 1),
      thetaMat = lotri::lotri(tcl0 ~ 0.1),
      nStud = 2
    )),
    silent = TRUE
  )
}

# The listener that runs the failing simulation inside a finished solve's
# event
.drawnListener <- function(event, ...) {
  .drawnFailingSolve(.drawnModel())
  invisible(NULL)
}

rxTest({
  test_that("a simulation that stops leaves no draws for the next solve", {
    .m <- .drawnModel()
    .fresh <- as.data.frame(rxSolve(.m, c(tcl = 3, err = 0), .drawnEvents()))
    .r <- .drawnFailingSolve(.m)
    expect_true(inherits(.r, "try-error"))
    expect_identical(
      conditionMessage(attr(.r, "condition")),
      "The following parameter(s) are required for solving: tcl"
    )
    expect_identical(.drawnLeft(), character(0))
    # the next solve, with no residuals of its own, reads none of the
    # failed one's
    .after <- as.data.frame(rxSolve(.m, c(tcl = 3, err = 0), .drawnEvents()))
    expect_identical(.after, .fresh)
  })

  test_that("a simulation that stops inside a finished solve's event leaves no draws", {
    .m <- .drawnModel()
    .fresh <- as.data.frame(rxSolve(.m, c(tcl = 3, err = 0), .drawnEvents()))
    rxEventListen("drawnTest", .drawnListener)
    on.exit(rxEventUnlisten("drawnTest"), add = TRUE)
    .outer <- as.data.frame(rxSolve(.m, c(tcl = 3, err = 0), .drawnEvents()))
    rxEventUnlisten("drawnTest")
    expect_identical(.outer, .fresh)
    expect_identical(.drawnLeft(), character(0))
    .after <- as.data.frame(rxSolve(.m, c(tcl = 3, err = 0), .drawnEvents()))
    expect_identical(.after, .fresh)
  })

  test_that("a finished simulation keeps its draws in its result", {
    .m <- .drawnModel()
    .s <- suppressWarnings(rxSolve(
      .m,
      c(tcl = 3),
      .drawnEvents(),
      sigma = lotri::lotri(err ~ 1),
      thetaMat = lotri::lotri(tcl ~ 0.1),
      nStud = 2
    ))
    expect_identical(.drawnLeft(), character(0))
    # each study has its own drawn tcl, and the residuals were applied
    expect_identical(length(unique(.s$params$tcl)), 2L)
    expect_false(isTRUE(all.equal(.s$y, .s$cp)))
  })
})
