# Subjects 11-13 get different doses and weights, so a subject solved with
# another subject's inputs gives a different concentration.
.paramsRowsData <- function(ids = c(11, 12, 13)) {
  .n <- length(ids)
  .dose <- data.frame(id = ids, time = 0, evid = 1, amt = 10 * seq_len(.n), cmt = "central", wt = seq_len(.n))
  .obs <- data.frame(
    id = rep(ids, each = 2),
    time = rep(c(1, 2), .n),
    evid = 0,
    amt = 0,
    cmt = "central",
    wt = rep(seq_len(.n), each = 2)
  )
  .d <- rbind(.dose, .obs)
  .d[order(match(.d$id, ids), .d$time, -.d$evid), ]
}

.paramsRowsModel <- function() {
  rxode2({
    d / dt(central) <- -k * central
    cp <- central / (v * wt)
  })
}

# Each subject solved on its own with the named parameter vector.
.paramsRowsPerId <- function(m, d, p) {
  .ret <- numeric(0)
  for (.id in unique(d$id)) {
    .ret <- c(.ret, as.data.frame(rxSolve(m, d[d$id == .id, ], params = p))$cp)
  }
  .ret
}

.paramsRowsErr <- function(expr) {
  tryCatch(
    {
      force(expr)
      NA_character_
    },
    error = conditionMessage
  )
}

.paramsRowsMsg <- function(id) {
  paste0(
    "'params' has one row, for id ",
    id,
    ", but the data has 3 subjects\nsubjects: 11, 12, 13\n",
    "give one row per subject, or drop the id column to use the row for all"
  )
}

rxTest({
  test_that("a one-row params data frame or matrix is used for every subject", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData()
    p <- c(k = 0.5, v = 2)
    .want <- .paramsRowsPerId(m, d, p)
    expect_equal(length(.want), 6L)
    expect_false(anyNA(.want))
    expect_equal(as.data.frame(rxSolve(m, d, params = p))$cp, .want)
    expect_equal(as.data.frame(rxSolve(m, d, params = data.frame(k = 0.5, v = 2)))$cp, .want)
    expect_equal(as.data.frame(rxSolve(m, d, params = cbind(k = 0.5, v = 2)))$cp, .want)
    # an unnamed matrix in the model's parameter order; the data's wt is used,
    # as it is over a named vector's
    expect_equal(rxModelVars(m)$params, c("k", "v", "wt"))
    expect_equal(as.data.frame(rxSolve(m, d, params = c(k = 0.5, v = 2, wt = 100)))$cp, .want)
    expect_equal(as.data.frame(rxSolve(m, d, params = matrix(c(0.5, 2, 100), 1)))$cp, .want)
    expect_equal(as.data.frame(rxSolve(m, d, params = data.frame(k = 0.5, v = 2), cores = 2))$cp, .want)
  })

  test_that("a one-row params is reported as one set, as a named vector is", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData()
    sv <- rxSolve(m, d, params = c(k = 0.5, v = 2))
    sd <- rxSolve(m, d, params = data.frame(k = 0.5, v = 2))
    expect_equal(sd$params, data.frame(k = 0.5, v = 2))
    expect_equal(sd$params, sv$params)
    expect_equal(names(sd$counts), c("slvr", "dadt", "jac"))
    expect_equal(nrow(sd$counts), 3L)
    # .DollarNames() builds them in C before `$` reaches the R builder
    sm <- rxSolve(m, d, params = cbind(k = 0.5, v = 2))
    expect_true("params" %in% utils::.DollarNames(sm, ""))
    expect_equal(sm$params, data.frame(k = 0.5, v = 2))
    expect_equal(names(sm$counts), c("slvr", "dadt", "jac"))
    expect_equal(nrow(sm$counts), 3L)
  })

  test_that("a one-row params with an id column refuses more subjects", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData()
    expect_equal(.paramsRowsErr(rxSolve(m, d, params = data.frame(id = 12, k = 0.5, v = 2))), .paramsRowsMsg("12"))
    expect_equal(.paramsRowsErr(rxSolve(m, d, params = data.frame(ID = 2, k = 0.5, v = 2))), .paramsRowsMsg("2"))
    expect_equal(.paramsRowsErr(rxSolve(m, d, params = cbind(k = 0.5, Id = 7, v = 2))), .paramsRowsMsg("7"))
    expect_equal(.paramsRowsErr(rxSolve(m, d, params = data.frame(id = "a", k = 0.5, v = 2))), .paramsRowsMsg("a"))
    expect_equal(
      .paramsRowsErr(rxSolve(m, d, params = data.frame(id = factor("b"), k = 0.5, v = 2))),
      .paramsRowsMsg("b")
    )
    # the error leaves rxode2 able to solve
    expect_equal(as.data.frame(rxSolve(m, d, params = c(k = 0.5, v = 2)))$cp, .paramsRowsPerId(m, d, c(k = 0.5, v = 2)))
  })

  test_that("the refusal lists the first ten subjects", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData(ids = 1:40)
    expect_equal(
      .paramsRowsErr(rxSolve(m, d, params = data.frame(id = 2, k = 0.5, v = 2))),
      paste0(
        "'params' has one row, for id 2, but the data has 40 subjects\n",
        "subjects: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ... (30 more)\n",
        "give one row per subject, or drop the id column to use the row for all"
      )
    )
  })

  test_that("one subject takes its one row whatever its id", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData(ids = 11)
    .want <- .paramsRowsPerId(m, d, c(k = 0.5, v = 2))
    expect_equal(as.data.frame(rxSolve(m, d, params = data.frame(id = 5, k = 0.5, v = 2)))$cp, .want)
  })
})
