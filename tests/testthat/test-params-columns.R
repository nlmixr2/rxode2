.paramsColsModel <- function() {
  rxode2({
    d / dt(central) <- -k * central
    cp <- central / v
  })
}

.paramsColsData <- function() {
  data.frame(
    id = rep(c(11, 12), each = 3),
    time = rep(c(0, 1, 2), 2),
    evid = rep(c(1, 0, 0), 2),
    amt = rep(c(10, 0, 0), 2),
    cmt = "central"
  )
}

.paramsColsErr <- function(expr) {
  tryCatch(
    {
      force(expr)
      NA_character_
    },
    error = conditionMessage
  )
}

rxTest({
  test_that("a params column no parameter reads may be of any type", {
    m <- .paramsColsModel()
    d <- .paramsColsData()
    .p <- data.frame(k = c(0.5, 0.6), v = c(2, 3))
    .want <- as.data.frame(rxSolve(m, d, params = .p))$cp
    expect_false(anyNA(.want))
    # a character label and a character id are not read
    expect_identical(as.data.frame(rxSolve(m, d, params = cbind(.p, label = c("a", "b"))))$cp, .want)
    expect_identical(as.data.frame(rxSolve(m, d, params = cbind(id = c("11", "12"), .p)))$cp, .want)
    # nor is a factor id, as a solve's own $params has
    s <- rxSolve(m, d, params = .p)
    expect_true(is.factor(s$params$id))
    expect_identical(as.data.frame(rxSolve(m, d, params = s$params))$cp, .want)
  })

  test_that("a character params column a parameter reads is refused by name", {
    m <- .paramsColsModel()
    d <- .paramsColsData()
    expect_identical(
      .paramsColsErr(rxSolve(m, d, params = data.frame(k = c("0.5", "0.6"), v = c(2, 3)))),
      "'params' column 'k' is character; a model parameter needs numbers"
    )
    # rxode2 solves normally afterwards
    expect_false(anyNA(as.data.frame(rxSolve(m, d, params = data.frame(k = c(0.5, 0.6), v = c(2, 3))))$cp))
  })
})
