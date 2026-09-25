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
})
