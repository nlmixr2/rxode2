rxTest({
  test_that("linCmtMicro() returns micro constants", {
    one <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        linCmt() ~ add(add.sd)
      })
    }
    m <- linCmtMicro(one)
    expect_length(m, 1L)
    expect_equal(m[[1]]$ncmt, 1L)
    expect_equal(m[[1]]$oral0, 1L)
    expect_equal(m[[1]]$ka, quote(ka))
    expect_equal(m[[1]]$v, quote(v))
    expect_equal(m[[1]]$k, quote(cl / v))

    two <- function() {
      ini({
        tcl <- 1
        tv <- 3.45
        tq <- 1
        tvp <- 4
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        q <- exp(tq)
        vp <- exp(tvp)
        cp <- linCmt()
        cp ~ add(add.sd)
      })
    }
    m <- linCmtMicro(two)
    expect_equal(m[[1]]$ncmt, 2L)
    expect_equal(m[[1]]$oral0, 0L)
    expect_equal(m[[1]]$k12, quote(q / v))
    expect_equal(m[[1]]$k21, quote(q / vp))

    ode <- function() {
      ini({
        tk <- 0
        add.sd <- 0.7
      })
      model({
        k <- exp(tk)
        d/dt(central) <- -k * central
        central ~ add(add.sd)
      })
    }
    expect_equal(linCmtMicro(ode), list())
  })

  test_that("linToOde() of linCmt() ~ gives a model that is not linCmt()", {
    one <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        linCmt() ~ add(add.sd)
      })
    }
    ode <- suppressMessages(linToOde(one))
    expect_equal(ode$predDf$var, "rxLinCmtOde")
    expect_false(ode$predDf$linCmt)
    # the translated model must parse without a linCmt()/ODE collision
    expect_error(rxode2(ode$simulationModel), NA)
  })

  test_that("linToOde() keeps the compartment numbers of a mixed model", {
    mixed <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        tq <- 1
        tvp <- 3
        ke0 <- 0.5
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        q <- exp(tq)
        vp <- exp(tvp)
        d/dt(ce) <- -ke0 * ce
        cp <- linCmt()
        cp ~ add(add.sd)
      })
    }
    ui <- suppressMessages(mixed())
    ode <- suppressMessages(linToOde(ui))
    expect_equal(ui$stateDf[["Compartment Name"]], c("depot", "central", "ce"))
    expect_equal(ode$stateDf[["Compartment Name"]],
                 c("depot", "central", "ce", "peripheral1"))
    # both give the same prediction when dosing by compartment number
    et <- et(amt = 100, cmt = 1) |>
      et(amt = 10, cmt = 3) |>
      et(seq(0.5, 24, by = 0.5))
    s1 <- rxSolve(ui, et, returnType = "data.frame")
    s2 <- rxSolve(ode, et, returnType = "data.frame")
    expect_equal(s1$cp, s2$cp, tolerance = 1e-4)
    expect_equal(s1$ce, s2$ce, tolerance = 1e-4)
  })
})
