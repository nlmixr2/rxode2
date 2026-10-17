# Subject i gets a dose of 10 * i and a weight of 1 (2 for every third), so no
# two of the first few subjects share a concentration: a subject solved with
# another's inputs gives a different cp.
.paramsRowsData <- function(ids = c(11, 12, 13)) {
  .n <- length(ids)
  .wt <- 1 + (seq_len(.n) %% 3 == 0)
  .dose <- data.frame(id = ids, time = 0, evid = 1, amt = 10 * seq_len(.n), cmt = "central", wt = .wt)
  .obs <- data.frame(
    id = rep(ids, each = 2),
    time = rep(c(1, 2), .n),
    evid = 0,
    amt = 0,
    cmt = "central",
    wt = rep(.wt, each = 2)
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

# The model with a random effect on k, for omega = simulation
.paramsRowsEtaModel <- function() {
  rxode2({
    k <- kpop * exp(eta.k)
    d / dt(central) <- -k * central
    cp <- central / (v * wt)
  })
}

# The model as a ui with an eta, so rxSolve() simulates from its omega
.paramsRowsUi <- function() {
  ini({
    tk <- log(0.5)
    tv <- log(2)
    eta.k ~ 0.1
  })
  model({
    k <- exp(tk + eta.k)
    v <- exp(tv)
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

.paramsRowsMsg <- function(id, subjects = c(11, 12, 13)) {
  paste0(
    "'params' has one row, for id ",
    id,
    ", but the data has ",
    length(subjects),
    " subjects\nsubjects: ",
    paste(subjects, collapse = ", "),
    "\n",
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
    # the subjects differ, so a solve that gave every subject the first's
    # inputs would not match
    expect_true(all(abs(diff(.want[c(1, 3, 5)])) > 1))
    expect_identical(as.data.frame(rxSolve(m, d, params = p))$cp, .want)
    expect_identical(as.data.frame(rxSolve(m, d, params = data.frame(k = 0.5, v = 2)))$cp, .want)
    expect_identical(as.data.frame(rxSolve(m, d, params = cbind(k = 0.5, v = 2)))$cp, .want)
    # either argument order
    expect_identical(as.data.frame(rxSolve(m, data.frame(k = 0.5, v = 2), d))$cp, .want)
    expect_identical(as.data.frame(rxSolve(m, d, data.frame(k = 0.5, v = 2)))$cp, .want)
    # an unnamed matrix in the model's parameter order; the data's wt is used,
    # as it is over a named vector's
    expect_equal(rxModelVars(m)$params, c("k", "v", "wt"))
    expect_identical(as.data.frame(rxSolve(m, d, params = c(k = 0.5, v = 2, wt = 100)))$cp, .want)
    expect_identical(as.data.frame(rxSolve(m, d, params = matrix(c(0.5, 2, 100), 1)))$cp, .want)
    expect_identical(as.data.frame(rxSolve(m, d, params = data.frame(k = 0.5, v = 2), cores = 2))$cp, .want)
    # two subjects are recycled to as well
    .d2 <- .paramsRowsData(ids = c(11, 12))
    expect_identical(
      as.data.frame(rxSolve(m, .d2, params = data.frame(k = 0.5, v = 2)))$cp,
      .paramsRowsPerId(m, .d2, p)
    )
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

  test_that("a multi-row params is reported with its id (and sim.id) columns", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData()
    .p3 <- data.frame(k = c(0.5, 0.6, 0.7), v = 2)
    .p6 <- data.frame(k = seq(0.5, 1, by = 0.1), v = 2)
    # the R builder, behind `$`
    sp <- rxSolve(m, d, params = .p3)
    expect_identical(names(sp$params), c("id", "k", "v"))
    expect_identical(as.character(sp$params$id), c("11", "12", "13"))
    expect_identical(names(sp$counts), c("slvr", "dadt", "jac"))
    sp6 <- rxSolve(m, d, params = .p6)
    expect_identical(names(sp6$params), c("sim.id", "id", "k", "v"))
    expect_identical(sp6$params$sim.id, rep(1:2, each = 3))
    # the C builder, which .DollarNames() runs first, puts id (and sim.id) in
    # $counts as well
    sm <- rxSolve(m, d, params = .p3)
    expect_true("params" %in% utils::.DollarNames(sm, ""))
    expect_identical(names(sm$params), c("id", "k", "v"))
    expect_identical(names(sm$counts), c("id", "slvr", "dadt", "jac"))
    sm6 <- rxSolve(m, d, params = .p6)
    expect_true("params" %in% utils::.DollarNames(sm6, ""))
    expect_identical(names(sm6$params), c("sim.id", "id", "k", "v"))
    expect_identical(names(sm6$counts), c("sim.id", "id", "slvr", "dadt", "jac"))
    expect_identical(nrow(sm6$counts), 6L)
  })

  test_that("a one-row params with an id column refuses more subjects", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData()
    expect_identical(.paramsRowsErr(rxSolve(m, d, params = data.frame(id = 12, k = 0.5, v = 2))), .paramsRowsMsg("12"))
    expect_identical(.paramsRowsErr(rxSolve(m, d, params = data.frame(ID = 2, k = 0.5, v = 2))), .paramsRowsMsg("2"))
    expect_identical(.paramsRowsErr(rxSolve(m, d, params = cbind(k = 0.5, Id = 7, v = 2))), .paramsRowsMsg("7"))
    expect_identical(.paramsRowsErr(rxSolve(m, d, params = data.frame(id = "a", k = 0.5, v = 2))), .paramsRowsMsg("a"))
    expect_identical(
      .paramsRowsErr(rxSolve(m, d, params = data.frame(id = factor("b"), k = 0.5, v = 2))),
      .paramsRowsMsg("b")
    )
    # either argument order
    expect_identical(.paramsRowsErr(rxSolve(m, data.frame(id = 12, k = 0.5, v = 2), d)), .paramsRowsMsg("12"))
    expect_identical(.paramsRowsErr(rxSolve(m, d, data.frame(id = 12, k = 0.5, v = 2))), .paramsRowsMsg("12"))
    # two subjects
    expect_identical(
      .paramsRowsErr(rxSolve(m, .paramsRowsData(ids = c(11, 12)), params = data.frame(id = 12, k = 0.5, v = 2))),
      .paramsRowsMsg("12", c(11, 12))
    )
    # the subjects are the data's labels, character or factor
    .dc <- .paramsRowsData(ids = c("a", "b", "c"))
    expect_identical(
      .paramsRowsErr(rxSolve(m, .dc, params = data.frame(id = "b", k = 0.5, v = 2))),
      .paramsRowsMsg("b", c("a", "b", "c"))
    )
    .df <- .dc
    .df$id <- factor(.df$id, levels = c("a", "b", "c"))
    expect_identical(
      .paramsRowsErr(rxSolve(m, .df, params = data.frame(id = "b", k = 0.5, v = 2))),
      .paramsRowsMsg("b", c("a", "b", "c"))
    )
    # the error leaves rxode2 able to solve
    expect_identical(
      as.data.frame(rxSolve(m, d, params = c(k = 0.5, v = 2)))$cp,
      .paramsRowsPerId(m, d, c(k = 0.5, v = 2))
    )
  })

  test_that("the refusal holds when the solve simulates from the row", {
    d <- .paramsRowsData()
    # a ui with an eta simulates its omega around the row
    expect_identical(
      .paramsRowsErr(rxSolve(.paramsRowsUi, d, params = data.frame(id = 12, tk = log(0.5), tv = log(2)))),
      .paramsRowsMsg("12")
    )
    # and so does a model given omega =
    expect_identical(
      .paramsRowsErr(rxSolve(
        .paramsRowsEtaModel(),
        d,
        params = data.frame(id = 12, kpop = 0.5, v = 2),
        omega = lotri::lotri(eta.k ~ 0.1)
      )),
      .paramsRowsMsg("12")
    )
  })

  test_that("a chunked solve checks the row against every subject", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData()
    for (.cs in 1:2) {
      expect_identical(
        .paramsRowsErr(rxSolve(
          m,
          d,
          params = data.frame(id = 12, k = 0.5, v = 2),
          file = tempfile("pr"),
          chunkSize = .cs
        )),
        .paramsRowsMsg("12"),
        label = .cs
      )
    }
  })

  test_that("the refusal lists the first ten subjects", {
    m <- .paramsRowsModel()
    d <- .paramsRowsData(ids = 1:40)
    expect_identical(
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
    expect_identical(as.data.frame(rxSolve(m, d, params = data.frame(id = 5, k = 0.5, v = 2)))$cp, .want)
  })
})
