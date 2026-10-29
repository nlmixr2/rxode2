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

  test_that("a one-row params data frame may carry a column no parameter reads", {
    m <- .paramsColsModel()
    d <- .paramsColsData()
    # the one row is used for both subjects
    .want <- as.data.frame(rxSolve(m, d, params = data.frame(k = 0.5, v = 2)))$cp
    expect_false(anyNA(.want))
    expect_identical(as.data.frame(rxSolve(m, d, params = data.frame(k = 0.5, v = 2, label = "a")))$cp, .want)
    # and for the subjects nSub asks for
    .ev <- et(amt = 10, cmt = "central") |> et(c(1, 2))
    .ns <- suppressWarnings(as.data.frame(rxSolve(m, .ev, params = data.frame(k = 0.5, v = 2), nSub = 2)))$cp
    expect_identical(
      suppressWarnings(as.data.frame(rxSolve(m, .ev, params = data.frame(k = 0.5, v = 2, label = "a"), nSub = 2)))$cp,
      .ns
    )
    # a character column a parameter reads is refused there too
    expect_identical(
      .paramsColsErr(rxSolve(m, d, params = data.frame(k = "0.5", v = 2))),
      "'params' column 'k' is character; a model parameter needs numbers"
    )
  })

  test_that("a params column no parameter reads may be of any type when simulating", {
    m <- rxode2({
      d / dt(central) <- -k * exp(eta.k) * central
      cp <- central / v
    })
    .ev <- et(amt = 10, cmt = "central") |> et(c(1, 2))
    .om <- lotri(eta.k ~ 0.1)
    # a one-row frame, and a frame recycled over nSub, carrying a label
    for (.p in list(data.frame(k = 0.5, v = 2), data.frame(k = c(0.5, 0.6), v = c(2, 3)))) {
      set.seed(42)
      .want <- as.data.frame(rxSolve(m, .ev, params = .p, omega = .om, nSub = 4))$cp
      expect_false(anyNA(.want))
      set.seed(42)
      .got <- as.data.frame(rxSolve(m, .ev, params = cbind(.p, label = "a"), omega = .om, nSub = 4))$cp
      expect_identical(.got, .want)
    }
    expect_identical(
      .paramsColsErr(rxSolve(m, .ev, params = data.frame(k = "0.5", v = 2), omega = .om, nSub = 3)),
      "'params' column 'k' is character; a model parameter needs numbers"
    )
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
