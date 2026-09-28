## A solve keeps its state in rxode2's globals, and a user function the
## running solve calls could start another solve that freed them, crashing R.
## The shapes that crashed run in a child process.

.nestedSolveMsg <- "rxSolve() cannot be called while another rxSolve() is running"

.nestedSolveChild <- function(lines) {
  .script <- tempfile(fileext = ".R")
  on.exit(unlink(.script), add = TRUE)
  writeLines(
    c(
      sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
      "suppressMessages(library(rxode2))",
      "msgOf <- function(expr) tryCatch({ expr; 'no error' }, error = conditionMessage)",
      lines,
      "cat('CHILD-DONE\\n')"
    ),
    .script
  )
  suppressWarnings(
    system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", shQuote(.script)),
      stdout = TRUE,
      stderr = TRUE
    )
  )
}

.nestedSolveLine <- function(out, tag) {
  sub(paste0("^", tag, " "), "", grep(paste0("^", tag, " "), out, value = TRUE))
}

rxTest({
  test_that("rxSolve() from a user function stops instead of crashing R", {
    skip_on_cran()
    if (!is.null(asNamespace("rxode2")$.__DEVTOOLS__)) {
      skip("the child process loads the installed rxode2")
    }
    .out <- .nestedSolveChild(c(
      "nest <- new.env()",
      "nest$solve <- TRUE",
      ## the inner model calls a user function of its own (this crashed R)
      "udfA <- function(x) {",
      "  if (!nest$solve) return(2 * x)",
      "  udfIn <- function(a) 2 * a",
      "  inner <- rxode2({ w <- udfIn(p) })",
      "  rxSolve(inner, et(0), params = c(p = x))$w[1]",
      "}",
      ## the inner model has no user function (this gave a STRING_ELT error)
      "udfB <- function(x) {",
      "  if (!nest$solve) return(2 * x)",
      "  inner <- rxode2({ w <- p * 2 })",
      "  rxSolve(inner, et(0), params = c(p = x))$w[1]",
      "}",
      ## a function-style model dispatches to another rxSolve() method
      "udfC <- function(x) {",
      "  inner <- function() { ini({ p <- 1 }); model({ w <- p * 2 }) }",
      "  rxSolve(inner, et(0), params = c(p = x))$w[1]",
      "}",
      "d <- data.frame(time = 1:3, x = 1:3)",
      "outerA <- rxode2({ z <- udfA(x) })",
      "outerB <- rxode2({ z <- udfB(x) })",
      "outerC <- rxode2({ z <- udfC(x) })",
      "cat('NESTED-A', msgOf(rxSolve(outerA, d)), '\\n')",
      "cat('NESTED-B', msgOf(rxSolve(outerB, d)), '\\n')",
      "cat('NESTED-C', msgOf(rxSolve(outerC, d)), '\\n')",
      ## the same outer models, in the same session, once they stop nesting
      "nest$solve <- FALSE",
      "cat('AFTER-A', rxSolve(outerA, d)$z, '\\n')",
      "cat('AFTER-B', rxSolve(outerB, d)$z, '\\n')"
    ))
    .info <- paste(utils::tail(.out, 20), collapse = "\n")
    expect_null(attr(.out, "status"), info = .info)
    expect_true("CHILD-DONE" %in% .out, info = .info)
    for (.tag in c("NESTED-A", "NESTED-B", "NESTED-C")) {
      .msg <- .nestedSolveLine(.out, .tag)
      expect_length(.msg, 1L)
      expect_match(.msg, .nestedSolveMsg, fixed = TRUE, info = .info)
      expect_match(.msg, "solve the inner model before or after the outer solve", fixed = TRUE, info = .info)
    }
    ## .udfCall() names the user function the error came from
    expect_match(.nestedSolveLine(.out, "NESTED-A"), "'udfA(1)': ", fixed = TRUE)
    expect_identical(.nestedSolveLine(.out, "AFTER-A"), "2 4 6 ")
    expect_identical(.nestedSolveLine(.out, "AFTER-B"), "2 4 6 ")
  })

  test_that("a `$<-` re-solve whose user function calls rxSolve() stops", {
    skip_on_cran()
    if (!is.null(asNamespace("rxode2")$.__DEVTOOLS__)) {
      skip("the child process loads the installed rxode2")
    }
    .out <- .nestedSolveChild(c(
      "nest <- new.env()",
      "nest$solve <- FALSE",
      "inner <- rxode2({ w <- p * 2 })",
      "udfOut <- function(x) {",
      "  if (!nest$solve) return(2 * x)",
      "  rxSolve(inner, et(0), params = c(p = x))$w[1]",
      "}",
      "outer <- rxode2({",
      "  d/dt(depot) <- -depot",
      "  z <- udfOut(x) + k",
      "})",
      ## `s$x <-` writes into the data frame it was solved from, so each solve
      ## gets its own
      "d <- function() data.frame(time = 1:3, x = 1:3)",
      "s <- rxSolve(outer, d(), params = c(k = 0))",
      "cat('BEFORE', s$z, '\\n')",
      "nest$solve <- TRUE",
      "cat('UPDATE-params', msgOf(s$params <- c(k = 1)), '\\n')",
      "cat('UPDATE-par', msgOf(s$k <- 1), '\\n')",
      "cat('UPDATE-cov', msgOf(s$x <- 4:6), '\\n')",
      "cat('UPDATE-inits', msgOf(s$depot0 <- 1), '\\n')",
      "cat('UPDATE-bracket', msgOf(s[, 'k'] <- 1), '\\n')",
      "cat('UPDATE-dbracket', msgOf(s[['k']] <- 1), '\\n')",
      ## a user function that re-solves a solved object with `$<-`
      "solved <- rxSolve(inner, et(0), params = c(p = 1))",
      "udfUpd <- function(x) {",
      "  solved$p <- x",
      "  solved$w[1]",
      "}",
      "outerUpd <- rxode2({ z <- udfUpd(x) })",
      "cat('NESTED-UPDATE', msgOf(rxSolve(outerUpd, d())), '\\n')",
      "nest$solve <- FALSE",
      "s <- rxSolve(outer, d(), params = c(k = 1))",
      "cat('AFTER', s$z, '\\n')",
      "s$k <- 2",
      "cat('AFTER-UPDATE', s$z, '\\n')"
    ))
    .info <- paste(utils::tail(.out, 20), collapse = "\n")
    expect_null(attr(.out, "status"), info = .info)
    expect_true("CHILD-DONE" %in% .out, info = .info)
    expect_identical(.nestedSolveLine(.out, "BEFORE"), "2 4 6 ")
    for (.tag in c(
      "UPDATE-params",
      "UPDATE-par",
      "UPDATE-cov",
      "UPDATE-inits",
      "UPDATE-bracket",
      "UPDATE-dbracket",
      "NESTED-UPDATE"
    )) {
      .msg <- .nestedSolveLine(.out, .tag)
      expect_length(.msg, 1L)
      expect_match(.msg, .nestedSolveMsg, fixed = TRUE, info = paste(.tag, .info))
    }
    expect_identical(.nestedSolveLine(.out, "AFTER"), "3 5 7 ")
    expect_identical(.nestedSolveLine(.out, "AFTER-UPDATE"), "4 6 8 ")
  })

  test_that("user functions that do not solve still work", {
    ## recursion
    udfFact <- function(n) if (n <= 1) 1 else n * udfFact(n - 1)
    .s <- rxSolve(rxode2({ y <- udfFact(t) }), et(1:4))
    expect_equal(.s$y, c(1, 2, 6, 24))
    ## a control list, which goes through rxSolve(NULL, ...)
    udfCtl <- function(x) {
      rxControl(atol = 1e-6)
      3 * x
    }
    .s <- rxSolve(rxode2({ y <- udfCtl(t) }), et(1:3))
    expect_equal(.s$y, c(3, 6, 9))
    ## building a model
    udfBuild <- function(x) {
      rxode2({ w <- p * 2 })
      x + 1
    }
    .s <- rxSolve(rxode2({ y <- udfBuild(t) }), et(1:3))
    expect_equal(.s$y, c(2, 3, 4))
    ## a `$<-` on a solved object that does not re-solve
    .solved <- rxSolve(rxode2({ w <- p * 2 }), et(0), params = c(p = 1))
    udfCol <- function(x) {
      .tmp <- .solved
      .tmp$note <- x
      .tmp$note[1]
    }
    .s <- rxSolve(rxode2({ y <- udfCol(t) }), et(1:3))
    expect_equal(.s$y, c(1, 2, 3))
  })

  test_that("the refusal holds after a nested user function call returns", {
    skip_on_cran()
    if (!is.null(asNamespace("rxode2")$.__DEVTOOLS__)) {
      skip("the child process loads the installed rxode2")
    }
    .out <- .nestedSolveChild(c(
      "udfCall <- utils::getFromNamespace('.udfCall', 'rxode2')",
      "inner <- rxode2({ w <- p * 2 })",
      "udfInner <- function(x) x + 1",
      "udfFail <- function(x) stop('inner failure')",
      ## a user function calls two others the way a solve does (one returns,
      ## one fails), then tries to solve; the refusal is caught, so the outer
      ## solve goes on and must still be right
      "udfOuter <- function(x) {",
      "  a <- udfCall('udfInner', list(x))",
      "  try(udfCall('udfFail', list(x)), silent = TRUE)",
      "  r <- msgOf(rxSolve(inner, et(0), params = c(p = x)))",
      "  if (grepl('cannot be called while another rxSolve() is running', r, fixed = TRUE)) 10 * a else -1",
      "}",
      "s <- rxSolve(rxode2({ z <- udfOuter(x) }), data.frame(time = 1:3, x = 1:3))",
      "cat('CAUGHT', s$z, '\\n')"
    ))
    .info <- paste(utils::tail(.out, 20), collapse = "\n")
    expect_null(attr(.out, "status"), info = .info)
    expect_true("CHILD-DONE" %in% .out, info = .info)
    expect_identical(.nestedSolveLine(.out, "CAUGHT"), "20 30 40 ")
  })

  test_that("an error or interrupt in a user function leaves later solves working", {
    udfBoom <- function(x) stop("boom")
    expect_error(rxSolve(rxode2({ y <- udfBoom(t) }), et(1:2)), "boom")
    udfIntr <- function(x) {
      stop(structure(
        class = c("interrupt", "condition"),
        list(message = "interrupted", call = NULL)
      ))
    }
    .r <- tryCatch(
      rxSolve(rxode2({ y <- udfIntr(t) }), et(1:2)),
      interrupt = function(i) "interrupted"
    )
    expect_identical(.r, "interrupted")
    ## and the next solve runs
    udfTwice <- function(x) 2 * x
    .s <- rxSolve(rxode2({ y <- udfTwice(t) }), et(1:3))
    expect_equal(.s$y, c(2, 4, 6))
  })

  test_that("`obj$t <-` still re-solves through rxSolve()", {
    udfThrice <- function(x) 3 * x
    .s <- rxSolve(rxode2({ y <- udfThrice(t) }), et(1:3))
    expect_equal(.s$y, c(3, 6, 9))
    ## the new times are added to the old ones
    .s$t <- c(4, 5)
    expect_equal(.s$time, c(1, 2, 3, 4, 5))
    expect_equal(.s$y, c(3, 6, 9, 12, 15))
  })
})
