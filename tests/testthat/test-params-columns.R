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

.paramsColsSexModel <- function() {
  function() {
    ini({
      k <- 0.5
    })
    model({
      kk <- k
      if (sex == "male") kk <- k * 2
      d / dt(central) <- -kk * central
    })
  }
}

# central at t=1 for each subject, by id
.paramsColsCentral <- function(s) {
  s <- as.data.frame(s)
  s <- s[s$time == 1, ]
  s$central[order(as.numeric(as.character(s$id)))]
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
  })

  test_that("a character or factor params column is compared to strings as a data column is", {
    m <- .paramsColsSexModel()
    .ev <- et(amt = 10) |> et(1) |> et(id = 1:2)
    .sex <- c("male", "female")
    .d <- as.data.frame(.ev)
    .d$sex <- .sex[.d$id]
    .want <- .paramsColsCentral(rxSolve(m, .d))
    expect_equal(.want, 10 * exp(-c(1, 0.5)), tolerance = 1e-5)
    expect_identical(.paramsColsCentral(rxSolve(m, .ev, iCov = data.frame(id = 1:2, sex = .sex))), .want)
    # neither warns that no data column was matched to 'sex'
    expect_no_warning(.p <- rxSolve(m, .ev, params = data.frame(k = 0.5, sex = .sex)))
    expect_identical(.paramsColsCentral(.p), .want)
    expect_no_warning(.p <- rxSolve(m, .ev, params = data.frame(k = 0.5, sex = factor(.sex))))
    expect_identical(.paramsColsCentral(.p), .want)
    # a one-row frame is used for every subject
    expect_equal(
      .paramsColsCentral(rxSolve(m, .ev, params = data.frame(k = 0.5, sex = "male"))),
      rep(10 * exp(-1), 2),
      tolerance = 1e-5
    )
    # a model literal no row has never matches, with the data column's warning
    expect_warning(
      .p <- rxSolve(m, .ev, params = data.frame(k = 0.5, sex = c("female", "female"))),
      "which is not in the data"
    )
    expect_equal(.paramsColsCentral(.p), rep(10 * exp(-0.5), 2), tolerance = 1e-5)
  })

  test_that("a character or factor params column is compared to strings when simulating", {
    m <- function() {
      ini({
        k <- 0.5
        eta.k ~ 0.1
      })
      model({
        kk <- k * exp(eta.k)
        if (sex == "male") kk <- k * 2 * exp(eta.k)
        d / dt(central) <- -kk * central
      })
    }
    .ev <- et(amt = 10) |> et(1)
    set.seed(42)
    # the level codes of the model's literals: "male" is 1
    .want <- .paramsColsCentral(suppressWarnings(rxSolve(
      m,
      .ev,
      params = data.frame(k = 0.5, sex = c(1, 2)),
      nSub = 4
    )))
    for (.sex in list(c("male", "female"), factor(c("male", "female"), levels = c("female", "male")))) {
      set.seed(42)
      .got <- .paramsColsCentral(rxSolve(m, .ev, params = data.frame(k = 0.5, sex = .sex), nSub = 4))
      expect_identical(.got, .want)
    }
  })

  test_that("a character params column a parameter reads without comparing it is a factor, as in the data", {
    m <- .paramsColsModel()
    d <- .paramsColsData()
    .d <- d
    .d$k <- ifelse(.d$id == 11, "b", "a")
    .d$v <- 2
    .want <- as.data.frame(rxSolve(m, .d))$cp
    expect_identical(as.data.frame(rxSolve(m, d, params = data.frame(k = c("b", "a"), v = 2)))$cp, .want)
  })
})
