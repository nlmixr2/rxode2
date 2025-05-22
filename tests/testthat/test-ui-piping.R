rxTest({

  testPipeQuote <- function(..., envir=parent.frame(), iniDf = NULL) {
    rxUnloadAll()
    gc()
    .quoteCallInfoLines(match.call(expand.dots = TRUE)[-1], envir=envir, iniDf=iniDf)
  }


  rxTest({

    test_that("nse evaluation", {

      tmp <- "d/dt(depot)"
      expect_equal(testPipeQuote(tmp),
                   list(quote(d/dt(depot))))


      t <- c("-d/dt(peripheral1)", "-d/dt(peripheral2)")
      expect_equal(testPipeQuote(t),
                   list(quote(-d/dt(peripheral1)),
                        quote(-d/dt(peripheral2))))

      t <- c(a="x", b="y")

      expect_equal(testPipeQuote(t),
                   list(quote(a <- x), quote(b <- y)))



      tmp <- list(tmp="d/dt(depot)")

      expect_equal(testPipeQuote(tmp$tmp),
                   list(quote(d/dt(depot))))

      tmp <- list(tmp=list(tmp="d/dt(depot)"))

      expect_equal(testPipeQuote(tmp$tmp$tmp),
                   list(quote(d/dt(depot))))
    })

    test_that("equivalent drop statements", {

      expect_equal(.changeDropNullLine(quote(a <- NULL)),
                   quote(-a))
      expect_equal(.changeDropNullLine(quote(a ~ NULL)),
                   quote(-a))
      expect_equal(.changeDropNullLine(str2lang("a = NULL")),
                   quote(-a))

      expect_equal(.changeDropNullLine(quote(d/dt(a) <- NULL)),
                   quote(-d/dt(a)))
      expect_equal(.changeDropNullLine(quote(d/dt(a) ~ NULL)),
                   quote(-d/dt(a)))
      expect_equal(.changeDropNullLine(str2lang("d/dt(a) = NULL")),
                   quote(-d/dt(a)))

      expect_equal(.changeDropNullLine(quote(lag(a) <- NULL)),
                   quote(-lag(a)))
      expect_equal(.changeDropNullLine(quote(lag(a) ~ NULL)),
                   quote(-lag(a)))
      expect_equal(.changeDropNullLine(str2lang("lag(a) = NULL")),
                   quote(-lag(a)))

      expect_equal(.changeDropNullLine(quote(alag(a) <- NULL)),
                   quote(-alag(a)))
      expect_equal(.changeDropNullLine(quote(alag(a) ~ NULL)),
                   quote(-alag(a)))
      expect_equal(.changeDropNullLine(str2lang("alag(a) = NULL")),
                   quote(-alag(a)))

      expect_equal(.changeDropNullLine(quote(F(a) <- NULL)),
                   quote(-F(a)))
      expect_equal(.changeDropNullLine(quote(F(a) ~ NULL)),
                   quote(-F(a)))
      expect_equal(.changeDropNullLine(str2lang("F(a) = NULL")),
                   quote(-F(a)))

      expect_equal(.changeDropNullLine(quote(f(a) <- NULL)),
                   quote(-f(a)))
      expect_equal(.changeDropNullLine(quote(f(a) ~ NULL)),
                   quote(-f(a)))
      expect_equal(.changeDropNullLine(str2lang("f(a) = NULL")),
                   quote(-f(a)))

      expect_equal(.changeDropNullLine(quote(rate(a) <- NULL)),
                   quote(-rate(a)))
      expect_equal(.changeDropNullLine(quote(rate(a) ~ NULL)),
                   quote(-rate(a)))
      expect_equal(.changeDropNullLine(str2lang("rate(a) = NULL")),
                   quote(-rate(a)))

      expect_equal(.changeDropNullLine(quote(dur(a) <- NULL)),
                   quote(-dur(a)))
      expect_equal(.changeDropNullLine(quote(dur(a) ~ NULL)),
                   quote(-dur(a)))
      expect_equal(.changeDropNullLine(str2lang("dur(a) = NULL")),
                   quote(-dur(a)))

      expect_equal(.changeDropNullLine(quote(a(0) <- NULL)),
                   quote(-a(0)))
      expect_equal(.changeDropNullLine(quote(a(0) ~ NULL)),
                   quote(-a(0)))
      expect_equal(.changeDropNullLine(str2lang("a(0) = NULL")),
                   quote(-a(0)))

    })

    test_that("test fix/unfix for eta", {
      expect_equal(testPipeQuote(a~fix),
                   list(quote(a<-fix)))
      expect_equal(testPipeQuote(a~unfix),
                   list(quote(a<-unfix)))
    })

    test_that("test as formula", {
      expect_equal(testPipeQuote(as.formula(a~b)),
                   list(quote(a~b)))
    })

    test_that("test of standard quoting of piping arguments", {
      expect_equal(testPipeQuote(-ka, tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
        cl = exp(tcl + eta.cl)
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, "tv10=3"), list(quote(-ka),
                         quote(tka <- 0.5),
                         quote(tv <- 3),
                         quote(tcl <- 10),
                         quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
                         quote(cl <- exp(tcl + eta.cl)),
                         quote(eta.ka ~ 3),
                         quote(eta.ka ~ 3),
                         quote(tv <- 3),
                         quote(tcl <- 10),
                         quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
                         quote(tv10 <- 3)))

      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2)))

      expect_equal(testPipeQuote({
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2),
      list(quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2)))

      # Test c()
      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, c(tka=1, tv=3, tcl=4)),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(tka <- 1),
           quote(tv <- 3),
           quote(tcl <- 4))
      )

      # test list()
      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, list(tka=1, tv=3, tcl=4)),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(tka <- 1),
           quote(tv <- 3),
           quote(tcl <- 4))
      )

      .tmp <- list(tcl = 3, tv = 4)
      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, .tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(tcl <- 3),
           quote(tv <- 4))
      )

      .tmp <- list(tcl = 3, tv = 4)
      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, ~.tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(tcl ~ 3),
           quote(tv ~ 4))
      )


      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, ~.tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(tcl ~ 3),
           quote(tv ~ 4))
      )

      .tmp <- c(tcl = 3, tv = 4)

      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, .tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(tcl <- 3),
           quote(tv <- 4))
      )

      .tmp <- quote({
        ka = exp(tka)
      })

      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, .tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(ka <- exp(tka)))
      )

      .tmp <- quote(ka <- 8)

      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, .tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(ka <- 8))
      )

      .tmp <- quote(ka4 ~ 8)

      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, .tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(ka4 ~ 8))
      )

      .tmp <- quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1))))

      expect_equal(testPipeQuote(tka=0.5, {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.ka ~ 3, eta.ka ~ 3,
      {
        tv = 3
        tcl = 10
        eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
      }, eta.v ~ 0.2, .tmp),
      list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(eta.v ~ 0.2),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))))
      )
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, assign", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)

    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        f(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, lower case f()", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), 6L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), 6L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)

    })

    one.compartment <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        F(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, upper case F()", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), 6L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), 6L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        lag(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, lag()", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), 6L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), 6L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        alag(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, alag()", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), 6L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), 6L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        rate(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, rate()", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), 6L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        dur(depot) <- 3
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, dur()", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), 6L)

      expect_equal(.getModelLineFromExpression(quote(not), f), NA_integer_)
    })

    # look at duplicate lines
    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d/dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, duplicate d/dt(depot)", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)

      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), NULL)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -5L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -5L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -5L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -5L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -5L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -5L)

      expect_equal(.getModelLineFromExpression(quote(not), f), NA_integer_)
    })

    # look at duplicate lines
    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        f(depot) <- 3
        F(depot) <- 1
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, duplicate f(depot)", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), NULL)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), NULL)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(not), f), NA_integer_)
    })

    # look at duplicate lag()
    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        lag(depot) <- 3
        alag(depot) <- 1
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("Model Line from Expression, duplicate f(depot)", {
      expect_equal(.getModelLineFromExpression(quote(ka), f), 1L)
      expect_equal(.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)

      expect_equal(.getModelLineFromExpression(quote(f(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(F(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(lag(depot)), f), NULL)
      expect_equal(.getModelLineFromExpression(quote(alag(depot)), f), NULL)

      expect_equal(.getModelLineFromExpression(quote(rate(depot)), f), -4L)
      expect_equal(.getModelLineFromExpression(quote(dur(depot)), f), -4L)

      expect_equal(.getModelLineFromExpression(quote(not), f), NA_integer_)
      expect_equal(.getModelLineFromExpression(quote(cp), f), 8L)

      expect_equal(.getModelLineFromExpression(quote(cp), f, TRUE), 9L)
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    testEst <- function(ui, par, lower, value, upper, fix=FALSE) {
      uiForce <- suppressMessages(force(ui))
      .ini <- ui$iniDf
      .w <- which(.ini$name == par)
      expect_equal(length(.w), 1)
      expect_equal(.ini$lower[.w], lower)
      expect_equal(.ini$est[.w], value)
      expect_equal(.ini$upper[.w], upper)
      expect_equal(.ini$fix[.w], fix)
    }

    test_that("simple ini piping, uncorrelated model", {
      testEst(f, "tka", -Inf, 0.45, Inf, FALSE)
      testEst(f %>% ini(tka=0.5), "tka", -Inf, 0.5, Inf, FALSE)
      testEst(f %>% ini(tka=fix), "tka", -Inf, 0.45, Inf, TRUE)

      testEst(f %>% ini(tka=c(0, 0.5)), "tka", 0, 0.5, Inf, FALSE)
      testEst(f %>% ini(tka=c(0, 0.5, 1)), "tka", 0, 0.5, 1, FALSE)

      expect_error(f %>% ini(tka=c(0, 0.5, 1, 4)), "tka")

      expect_error(f %>% ini(tka=c(3,2,1)), "tka")

      suppressMessages(
        fFix <- f %>% ini(tka=fix)
      )
      testEst(fFix, "tka", -Inf, 0.45, Inf, TRUE)
      testEst(fFix %>% ini(tka=unfix), "tka", -Inf, 0.45, Inf, FALSE)
      testEst(fFix %>% ini(tka=unfix(0.5)), "tka", -Inf, 0.5, Inf, FALSE)

      testEst(f %>% ini(eta.v ~ 0.2), "eta.v", -Inf, 0.2, Inf, FALSE)

      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "eta.cl", -Inf, 0.3, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "eta.v", -Inf, 0.1, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "(eta.cl,eta.v)", -Inf, 0.02, Inf, FALSE)

      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "eta.cl", -Inf, 0.3, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "eta.v", -Inf, 0.1, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "(eta.cl,eta.v)", -Inf, 0.02*(sqrt(0.3)*sqrt(0.1)), Inf, FALSE)

      testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, TRUE)

      # Test adding matrix directly

      .omega <- lotri::lotri(eta.cl+eta.v~c(0.3, 0.02, 0.1))

      testEst(f %>% ini(.omega), "eta.cl", -Inf, 0.3, Inf, FALSE)
      testEst(f %>% ini(.omega), "eta.v", -Inf, 0.1, Inf, FALSE)
      testEst(f %>% ini(.omega), "(eta.cl,eta.v)", -Inf, 0.02, Inf, FALSE)

      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, FALSE),
        regexp="unfix.*eta.cl"), regexp="unfix.*eta.v"
        )
      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, FALSE),
        regexp="unfix.*eta.cl"), regexp="unfix.*eta.v"
        )
      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, FALSE),
        regexp="unfix.*eta.cl"), regexp="unfix.*eta.v"
        )
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl + eta.v ~ sd(cor(0.3,
                                -0.7, 0.1))
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("simple ini piping, correlated model", {
      testEst(f, "tka", -Inf, 0.45, Inf, FALSE)
      testEst(f %>% ini(tka=0.5), "tka", -Inf, 0.5, Inf, FALSE)
      testEst(f %>% ini(tka=fix), "tka", -Inf, 0.45, Inf, TRUE)

      testEst(f %>% ini(tka=c(0, 0.5)), "tka", 0, 0.5, Inf, FALSE)
      testEst(f %>% ini(tka=c(0, 0.5, 1)), "tka", 0, 0.5, 1, FALSE)

      expect_error(f %>% ini(tka=c(0, 0.5, 1, 4)), "tka")

      expect_error(f %>% ini(tka=c(3,2,1)), "tka")

      suppressMessages(
        fFix <- f %>% ini(tka=fix)
      )
      testEst(fFix, "tka", -Inf, 0.45, Inf, TRUE)
      testEst(fFix %>% ini(tka=unfix), "tka", -Inf, 0.45, Inf, FALSE)
      testEst(fFix %>% ini(tka=unfix(0.5)), "tka", -Inf, 0.5, Inf, FALSE)

      testEst(f %>% ini(eta.v ~ 0.2), "eta.v", -Inf, 0.2, Inf, FALSE)

      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "eta.cl", -Inf, 0.3, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "eta.v", -Inf, 0.1, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "(eta.cl,eta.v)", -Inf, 0.02, Inf, FALSE)

      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "eta.cl", -Inf, 0.3, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "eta.v", -Inf, 0.1, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "(eta.cl,eta.v)", -Inf, 0.02*(sqrt(0.3)*sqrt(0.1)), Inf, FALSE)

      testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, TRUE)

      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, FALSE),
        regexp="unfix.*eta.cl"), regexp="unfix.*eta.v"
        )
      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, FALSE),
        regexp="unfix.*eta.cl"), regexp="unfix.*eta.v"
        )
      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, FALSE),
        regexp="unfix.*eta.cl"), regexp="unfix.*eta.v"
        )
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl + eta.v ~ fix(sd(cor(0.3,
                                    -0.7, 0.1)))
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("simple ini piping, fixed correlated model", {
      testEst(f, "tka", -Inf, 0.45, Inf, FALSE)
      testEst(f %>% ini(tka=0.5), "tka", -Inf, 0.5, Inf, FALSE)
      testEst(f %>% ini(tka=fix), "tka", -Inf, 0.45, Inf, TRUE)

      testEst(f %>% ini(tka=c(0, 0.5)), "tka", 0, 0.5, Inf, FALSE)
      testEst(f %>% ini(tka=c(0, 0.5, 1)), "tka", 0, 0.5, 1, FALSE)

      expect_error(f %>% ini(tka=c(0, 0.5, 1, 4)), "tka")

      expect_error(f %>% ini(tka=c(3,2,1)), "tka")

      suppressMessages(
        fFix <- f %>% ini(tka=fix)
      )
      testEst(fFix, "tka", -Inf, 0.45, Inf, TRUE)
      testEst(fFix %>% ini(tka=unfix), "tka", -Inf, 0.45, Inf, FALSE)
      testEst(fFix %>% ini(tka=unfix(0.5)), "tka", -Inf, 0.5, Inf, FALSE)

      # should warn? Modify fixed value
      testEst(f %>% ini(eta.v ~ 0.2), "eta.v", -Inf, 0.2, Inf, TRUE)

      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "eta.cl", -Inf, 0.3, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "eta.v", -Inf, 0.1, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1)), "(eta.cl,eta.v)", -Inf, 0.02, Inf, TRUE)

      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "eta.cl", -Inf, 0.3, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "eta.v", -Inf, 0.1, Inf, TRUE)
      testEst(f %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1)), "(eta.cl,eta.v)", -Inf, 0.02*(sqrt(0.3)*sqrt(0.1)), Inf, TRUE)

      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, TRUE),
        regexp="fix.*eta.cl"), regexp="fix.*eta.v"
        )
      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, TRUE),
        regexp="fix.*eta.cl"), regexp="fix.*eta.v"
        )
      expect_warning(expect_warning(
        testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, TRUE),
        regexp="fix.*eta.cl"), regexp="fix.*eta.v"
        )

      testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, FALSE)
      testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, FALSE)
    })

    # %>% ini(tka=0.5)
    # %>% ini(tka=fix)
    # %>% ini(tka=unfix)
    # %>% ini(eta.v~0.2)

    # Try with |>
    # %>% ini(eta.cl+eta.v~c(0.3, 0.02, 0.1))
    # %>% ini(eta.cl+eta.v~cor(0.3, 0.02, 0.1))
    # %>% ini(eta.v+eta.cl~fix(cor(sd(0.3,0.02,0.1))))
    # %>% ini(eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1))))

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    testUi <- function(ui, has = NULL, exclude = NULL, values = NULL) {
      uiForce <- suppressMessages(force(ui))
      if (!is.null(has)) {
        expect_true(all(has %in% paste(uiForce$ini$name)))
      }
      if (!is.null(values) && !is.null(names(values))) {
        .vals <- setNames(uiForce$ini$est, paste(uiForce$ini$name))
        .vals <- .vals[names(values)]
        expect_equal(values, .vals)
      }
      if (!is.null(exclude)) {
        expect_false(any(exclude %in% paste(uiForce$ini$name)))
      }
      ## General UI properties
      expect_true(all(!is.na(uiForce$ini$fix)))
      expect_true(all(!is.na(uiForce$ini$lower)))
      expect_true(all(!is.na(uiForce$ini$upper)))
    }

    test_that("update: Test Base model", {
      testUi(f, c("tka", "tcl", "tv", "eta.ka", "eta.cl", "eta.v", "add.err"),
             "matt", c(tka = 0.45, tcl = 1, tv = 3.45, eta.ka = 0.6, eta.cl = 0.3, eta.v = 0.1, add.err = 0.7))
    })

    test_that("UI updates work correctly", {
      # context("update: Multiple component change with c()")
      testUi(
        f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), c(tcl = 3, tv = 4)),
        c("tka", "tcl", "tv", "eta.v", "add.err"),
        c("eta.ka", "eta.cl"),
        c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
      )

      # context("update: Multiple component change with list()")
      testUi(
        f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), list(tcl = 3, tv = 4)),
        c("tka", "tcl", "tv", "eta.v", "add.err"),
        c("eta.ka", "eta.cl"),
        c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
      )

      # context("update: Multiple component change with assigned .tmp=list()")
      .tmp <- list(tcl = 3, tv = 4)
      testUi(
        f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), .tmp),
        c("tka", "tcl", "tv", "eta.v", "add.err"),
        c("eta.ka", "eta.cl"),
        c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
      )

      # context("update: Multiple component change with assigned .tmp=c()")
      .tmp <- c(tcl = 3, tv = 4)
      testUi(
        f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), .tmp),
        c("tka", "tcl", "tv", "eta.v", "add.err"),
        c("eta.ka", "eta.cl"),
        c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
      )

      # context("update: Multiple component change with assigned .tmp={}")
      .tmp <- quote({
        ka <- exp(tka)
      })
      testUi(
        f %>% update(tka = 4, cl = exp(tcl), .tmp, c(tcl = 3, tv = 4)),
        c("tka", "tcl", "tv", "eta.v", "add.err"),
        c("eta.ka", "eta.cl"),
        c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
      )

      testUi(
        f %>% update(
          tka = 4,
          cl = exp(tcl),
          {
            ka <- exp(tka)
          },
          c(tcl = 3, tv = 4)
        ),
        c("tka", "tcl", "tv", "eta.v", "add.err"),
        c("eta.ka", "eta.cl"),
        c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
      )

      testUi(
        f %>% update(ka = exp(tka)),
        c("tka", "tcl", "tv", "eta.cl", "eta.v", "add.err"),
        "eta.ka", c(tka = 0.45, tcl = 1, tv = 3.45, eta.cl = 0.3, eta.v = 0.1, add.err = 0.7)
      )

      ## Now test linCmt() issue #166
      one.cmt <- function() {
        ini({
          tka <- 0.45 ; label("Log Ka")
          tcl <- 1 ; label("Log Cl")
          tv <- 3.45 ; label("Log V")
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.err <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.err)
        })
      }

      suppressMessages(
        .ui <- one.cmt %>% update({
          linCmt() ~ add(add.err) + prop(prop.err)
        })
      )
      expect_s3_class(.ui, "rxUi")
    })

    # piping looks through parent environments
    test_that("Looks through prior frames for the correct object", {
      fit <- rxode2(one.compartment)
      fits <- lapply(seq(-1, -0.1, 0.1), function(kainit) {
        suppressMessages(
          rxode2(update(fit, tka = kainit))
        )
      })

      expect_type(fits, "list")
      expect_error(lapply(seq(-1, -0.1, 0.1), function(kainit) {
        suppressMessages(
          rxode2(update(fit, tka = matt))
        )
      }))
    })

    one.compartment <- function() {
      ini({
        tka <- 0.45 ; label("Log Ka")
        tcl <- 1 ; label("Log Cl")
        tv <- 3.45 ; label("Log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    test_that("piping works for correlations #1", {
      testUi(f %>% ini(eta.ka + eta.cl ~ c(
        0.2,
        0.01, 0.2
      )),
      has = c("tka", "tcl", "tv", "eta.ka", "eta.cl", "eta.v", "add.err", "(eta.ka,eta.cl)"),
      exclude = "matt",
      values = c(
        tka = 0.45, tcl = 1, tv = 3.45, eta.ka = 0.2, eta.cl = 0.2, eta.v = 0.1, add.err = 0.7,
        `(eta.ka,eta.cl)` = 0.01
      )
      )
    })

    test_that("piping works for correlations #2", {
      suppressMessages(
        expect_error(
          f %>%
            ini(eta.ka + eta.matt ~ c(0.2,
                                      0.01, 0.2)
                )
        )
      )
    })

    test_that("piping works for correlations #3", {
      testUi(
        f %>% update(eta.ka + eta.cl ~ c(
          0.2,
          0.01, 0.2
        )),
        c("tka", "tcl", "tv", "eta.ka", "eta.cl", "eta.v", "add.err", "(eta.ka,eta.cl)"),
        "matt", c(
          tka = 0.45, tcl = 1, tv = 3.45, eta.ka = 0.2, eta.cl = 0.2, eta.v = 0.1, add.err = 0.7,
          `(eta.ka,eta.cl)` = 0.01
        )
      )
    })

    test_that("piping works for correlations #4", {
      suppressMessages(
        expect_error(
          f %>%
            update(eta.ka + eta.matt ~ c(0.2,
                                         0.01, 0.2)
                   )
        )
      )
    })

    test_that("expected piping errors", {
      f <- function() {
        ini({
          ke <- 0.5
          eta.ke ~ 0.04
          prop.sd <- sqrt(0.1)
        })
        model({
          ke <- ke * exp(eta.ke)
          ipre <- 10 * exp(-ke * t)
          ipre ~ prop(prop.sd)
        })
      }

      f <- rxode2::rxode2(f)

      suppressMessages(
        expect_error(f %>% model(ipre ~ add(add.sd)) %>% ini(add.sd=sqrt(0.1)), NA)
      )
    })

    test_that("new ipre", {
      f <- function() {
        ini({
          tke <- 0.5
          eta.ke ~ 0.04
          prop.sd <- sqrt(0.1)
        })
        model({
          ke <- tke * exp(eta.ke)
          ipre <- 10 * exp(-ke * t)
          f2 <- ipre / (ipre + 5)
          ipre ~ prop(prop.sd)
        })
      }

      f <- rxode2(f)

      trans <- function(f) {
        suppressMessages(
          f %>% model(ipre ~ propF(prop.sd, f2)) %>% ini(prop.sd=sqrt(0.1))
        )
      }

      f2 <- trans(f)

      expect_true(!any(f2$iniDf$name %in% c("f2")))
    })

    test_that("piping looks in the right environment for variables with fix()", {
      f <- function() {
        ini({
          tke <- 0.5
          eta.ke ~ 0.04
          prop.sd <- sqrt(0.1)
        })
        model({
          ke <- tke * exp(eta.ke)
          ipre <- 10 * exp(-ke * t)
          f2 <- ipre / (ipre + 5)
          ipre ~ prop(prop.sd)
        })
      }

      intke <- 5
      suppressMessages(
        tmp <- f %>% ini(tke=fix(intke))
      )

      expect_true(tmp$iniDf[tmp$iniDf$name == "tke","fix"])
      expect_equal(tmp$iniDf[tmp$iniDf$name == "tke","est"], 5)
      rm(list="intke")

      f2 <- function() {
        f <- function() {
          ini({
            tke <- 0.5
            eta.ke ~ 0.04
            prop.sd <- sqrt(0.1)
          })
          model({
            ke <- tke * exp(eta.ke)
            ipre <- 10 * exp(-ke * t)
            f2 <- ipre / (ipre + 5)
            ipre ~ prop(prop.sd)
          })
        }

        intke <- 5
        f %>% ini(tke=fix(intke))
      }

      suppressMessages(
        tmp <- f2()
      )

      expect_true(tmp$iniDf[tmp$iniDf$name == "tke","fix"])
      expect_equal(tmp$iniDf[tmp$iniDf$name == "tke","est"], 5)

      expect_false(any(ls() == "intke"))
    })

    test_that("invalid model pipe (more arguments than expected) throws an error", {
      f <- function() {
        ini({
          tke <- 0.5
          eta.ke ~ 0.04
          prop.sd <- sqrt(0.1)
        })
        model({
          ke <- tke * exp(eta.ke)
          ipre <- 10 * exp(-ke * t)
          f2 <- ipre / (ipre + 5)
          f3 <- f2 * 3
          lipre <- log(ipre)
          ipre ~ prop(prop.sd)
        })
      }

      expect_error(f %>% model(ipre~prop(f2,f3,c)))
    })

    test_that("Add an eta to a model that does not have an eta will work", {
      ocmt <- function() {
        ini({
          tka <- exp(0.45)
          tcl <- exp(1)
          tv <- exp(3.45)
          add.sd <- 0.7
        })
        model({
          ka <- tka
          cl <- tcl
          v <- tv
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          cp ~ add(add.sd)
        })
      }

      suppressMessages(
        expect_error(
          ocmt %>%
            model(ka <- exp(tka + eta.ka)),
          NA
        )
      )
    })

    test_that("Add covariate to model works", {
      ocmt <- function() {
        ini({
          tka <- exp(0.45)
          tcl <- exp(1)
          tv <- exp(3.45)
          add.sd <- 0.7
        })
        model({
          ka <- tka
          cl <- tcl
          v <- tv
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          cp ~ add(add.sd)
        })
      }


      suppressMessages(
        expect_error(
          ocmt %>%
            model(ka <- exp(tka + covKa * wt + eta.ka)),
          NA
        )
      )

      suppressMessages(
        tmp <-
          ocmt %>%
          model(ka <- exp(tka + covKaWt * wt + eta.ka))
      )
      expect_equal(tmp$allCovs, "wt")

      expect_true("covKaWt" %in% tmp$iniDf$name)
      expect_true("tka" %in% tmp$iniDf$name)
      expect_true("eta.ka" %in% tmp$iniDf$name)

      suppressMessages(
        tmp <-
          ocmt %>%
          model(ka <- exp(covKaWt * wt + eta.ka))
      )

      expect_equal(tmp$allCovs, "wt")
      expect_true("covKaWt" %in% tmp$iniDf$name)
      expect_false("tka" %in% tmp$iniDf$name)
      expect_true("eta.ka" %in% tmp$iniDf$name)

      suppressMessages(
        tmp <-
          tmp %>%
          model(ka <- exp(tka + covKaWt * wt + eta.ka))
      )
      expect_equal(tmp$allCovs, "wt")
      expect_true("covKaWt" %in% tmp$iniDf$name)
      expect_true("tka" %in% tmp$iniDf$name)
      expect_true("eta.ka" %in% tmp$iniDf$name)
    })

    test_that("Appending or pre-pending items to a model works", {
      ocmt <- function() {
        ini({
          tka <- exp(0.45)
          tcl <- exp(1)
          tv <- exp(3.45)
          add.sd <- 0.7
        })
        model({
          ka <- tka
          cl <- tcl
          v <- tv
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          cp ~ add(add.sd)
        })
      }

      f <- rxode2(ocmt)
      f2 <- f %>% model(cp1 <- cp, append=TRUE)

      expect_true("cp1" %in% f2$mv0$lhs)
      expect_equal(f2$lstExpr[[length(f2$lstExpr)]], quote(cp1 <- cp))

      f <- rxode2(ocmt)
      f2 <- f %>% model(cp1 <- cp, append=Inf)

      expect_true("cp1" %in% f2$mv0$lhs)
      expect_equal(f2$lstExpr[[length(f2$lstExpr)]], quote(cp1 <- cp))

      f <- rxode2(ocmt)
      f2 <- f %>% model(cp1 <- cp, append=100)

      expect_true("cp1" %in% f2$mv0$lhs)
      expect_equal(f2$lstExpr[[length(f2$lstExpr)]], quote(cp1 <- cp))

      f2 <- f %>% model(f2 <- 3 * 2, append=NA)
      expect_true("f2" %in% f2$mv0$lhs)
      expect_equal(f2$lstExpr[[1]], quote(f2 <- 3 * 2))
    })

    test_that("ini promotion works", {
      ocmt <- function() {
        ini({
          tka <- 0.45
          tcl <- 1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          cp ~ add(add.sd)
        })
      }

      f <- rxode2(ocmt)

      expect_equal(f$allCovs, c("eta.ka", "eta.cl", "tv", "eta.v"))
      expect_equal(f$theta, c(tka=0.45, tcl=1, add.sd=0.7))
      expect_equal(f$omega, NULL)

      # now promote tv
      suppressMessages(
        f2 <- f %>% ini(tv=0.5)
      )
      expect_equal(f2$allCovs, c("eta.ka", "eta.cl", "eta.v"))
      expect_equal(f2$theta, c(tka=0.45, tcl=1, add.sd=0.7, tv=0.5))
      expect_equal(f2$omega, NULL)

      # now promote eta.ka
      suppressMessages(
        f3 <- f2 %>% ini(eta.ka ~ 0.01)
      )

      expect_equal(f3$allCovs, c("eta.cl", "eta.v"))
      expect_equal(f3$theta, c(tka=0.45, tcl=1, add.sd=0.7, tv=0.5))
      expect_equal(f3$omega, matrix(0.01, dimnames=list("eta.ka", "eta.ka")))

      # now promote a correlation between eta.cl and eta.v
      suppressMessages(
        f4 <- f2 %>% ini(eta.cl + eta.v ~ c(1,
                                            0.01, 1))
      )
      expect_equal(f4$allCovs, "eta.ka")
      expect_equal(f4$theta, c(tka=0.45, tcl=1, add.sd=0.7, tv=0.5))

      expect_equal(f4$omega, lotri(eta.cl + eta.v ~ c(1,
                                                      0.01, 1)))

      # Now promote independent eta block
      suppressMessages(
        f5 <- f3 %>% ini(eta.cl + eta.v ~ c(1,
                                            0.01, 1))
      )
      expect_length(f5$allCovs, 0)
      expect_equal(f5$theta, c(tka=0.45, tcl=1, add.sd=0.7, tv=0.5))
      expect_equal(f5$omega, lotri(eta.ka ~ 0.01,
                                   eta.cl + eta.v ~ c(1,
                                                      0.01, 1)))

      # Now promote eta block that includes prior eta information
      suppressMessages(
        f6 <- f3 %>% ini(eta.ka + eta.cl + eta.v ~ c(1,
                                                     0.01, 1,
                                                     -0.01, 0.01, 1))
      )
      expect_length(f6$allCovs, 0)
      expect_equal(f6$theta, c(tka=0.45, tcl=1, add.sd=0.7, tv=0.5))
      expect_equal(f6$omega, lotri(eta.ka + eta.cl + eta.v ~ c(1,
                                                               0.01, 1,
                                                               -0.01, 0.01, 1)))
    })

    test_that("Ignoring auto-selected parameter types work", {
      ocmt <- function() {
        ini({
          tka <- exp(0.45)
          tcl <- exp(1)
          eta.v ~ 0.01
          add.sd <- 0.7
        })
        model({
          ka <- tka
          cl <- tcl
          v <- eta.v
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          cp ~ add(add.sd)
        })
      }

      suppressWarnings(
        f <- rxode2(ocmt)
      )

      expect_equal(f$allCovs, character(0))
      expect_equal(f$theta, c(tka=exp(0.45), tcl=exp(1), add.sd=0.7))
      expect_equal(f$omega, matrix(0.01, dimnames=list("eta.v", "eta.v")))

      suppressMessages(suppressWarnings(
        f2 <- f %>% model(ka <- tka * exp(eta.ka), auto=FALSE)
      ))

      expect_equal(f2$allCovs, "eta.ka")
      expect_equal(f2$theta, c(tka=exp(0.45), tcl=exp(1), add.sd=0.7))
      expect_equal(f2$omega, matrix(0.01, dimnames=list("eta.v", "eta.v")))

      suppressMessages(suppressWarnings(
        f2 <-
          f %>%
          model(ka <- tka * exp(eta.ka), auto=FALSE) %>%
          ini(eta.ka ~ 0.02)
      ))
      expect_equal(f2$allCovs, character(0))
      expect_equal(f2$theta, c(tka=exp(0.45), tcl=exp(1), add.sd=0.7))
      expect_equal(f2$omega, lotri(eta.v ~ 0.01,
                                   eta.ka ~ 0.02))

      suppressMessages(suppressWarnings(
        f2 <- f %>% model(v <- tv + eta.v, auto=FALSE)
      ))
      expect_equal(f2$allCovs, "tv")
      expect_equal(f2$theta, c(tka=exp(0.45), tcl=exp(1), add.sd=0.7))
      expect_equal(f2$omega, lotri(eta.v ~ 0.01))

      suppressMessages(suppressWarnings(
        f2 <- f %>% model(v <- tv + eta.v, auto=FALSE) %>%
          ini(tv=0.2)
      ))
      expect_equal(f2$allCovs, character(0))
      expect_equal(f2$theta, c(tka=exp(0.45), tcl=exp(1), add.sd=0.7, tv=0.2))
      expect_equal(f2$omega, lotri(eta.v ~ 0.01))
    })

    test_that("Ignoring auto-selected parameter types work", {
      ocmt <- function() {
        ini({
          tka <- exp(0.45)
          tcl <- exp(1)
          eta.v ~ 0.01
          add.sd <- 0.7
          tprop <- 0.5
          prop.eta ~ 0.01
        })
        model({
          ka <- tka
          cl <- tcl
          v <- eta.v
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          prop.sd <- exp(tprop + prop.eta)
          cp ~ add(add.sd)
        })
      }

      suppressWarnings(
        f1 <- ocmt()
      )
      suppressWarnings(
        f2 <- ocmt %>% model(cp ~ add(add.sd) + prop(prop.sd))
      )
      expect_equal(f2$theta, f1$theta)
      expect_equal(f2$omega, f1$omega)
    })

    test_that("Pre-declaring list of covariates works", {
      rxSetCovariateNamesForPiping(c("WT","HT", "TC"))

      # Note this is case sensitive
      one.compartment <- function() {
        ini({
          tka <- 0.45
          tcl <- 1
          tv <- 3.45
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.err <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          d / dt(depot) <- -ka * depot
          d/dt(depot) <- -ka * depot
          d / dt(center) <- ka * depot - cl / v * center
          cp <- center / v
          cp ~ add(add.err)
        })
      }

      # now TC is detected as a covariate instead of a population parameter
      suppressMessages(
        mod <-
          one.compartment %>%
          model({ka <- exp(tka + eta.ka + TC * cov_C)})
      )
      expect_true("cov_C" %in% mod$iniDf$name)
      expect_false("TC" %in% mod$iniDf$name)

      rxSetCovariateNamesForPiping()

      suppressMessages(
        mod <-
          one.compartment %>%
          model({ka <- exp(tka + eta.ka + TC * cov_C)})
      )
      expect_true("cov_C" %in% mod$iniDf$name)
      expect_true("TC" %in% mod$iniDf$name)
    })
  })

  test_that("eff(0) piping should work", {
    mod1 <- rxode2({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <- Q*C2 - Q*C3
      d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    })

    suppressMessages(
      expect_error(
        mod1 %>%
          model(KA<-exp(tka+eta.ka), append=NA) %>% # Prepend a line by append=NA
          ini(tka=log(2.94E-01),
              eta.ka=0.2,
              CL=1.86E+01, V2=4.02E+01, # central
              Q=1.05E+01,  V3=2.97E+02, # peripheral
              Kin=1, Kout=1, EC50=200) %>%
          model(eff(0) <- 1),
        NA
      )
    )
  })


  test_that("auto with studid==", {

    one.compartment <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv+eta.v)
        cp <- linCmt()
        cp ~ add(add.sd)
      })
    }


    i <- rxode2(one.compartment)

    j <- i %>%
      model({
        f(central)  <- 1 + f_study1*(STUDYID==1)
      },
      append=NA,
      auto=FALSE)

    expect_false(any(j$iniDf$name == "f_study1"))
    expect_false(any(j$iniDf$name == "STUDYID"))

  })

  test_that("piping with append=lhs", {

    ocmt_rx0 <- rxode2( {
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
    })

    m1 <- ocmt_rx0 %>% model( cl <- tvcl*2, append = NA)

    expect_true(identical(m1$lstExpr[[1]], quote(cl <- tvcl * 2)))

    m2 <- ocmt_rx0 %>% model( cl <- tvcl*2, append = d/dt(depot))

    expect_true(identical(m2$lstExpr[[2]], quote(cl <- tvcl * 2)))

    expect_error(ocmt_rx0 %>% model( cl <- tvcl*2, append = notFound))

    m3 <- ocmt_rx0 %>% model( cl <- tvcl*2, append = cp)

    expect_true(identical(m3$lstExpr[[4]], quote(cl <- tvcl * 2)))

    test_that("piping ui functions", {

      m1 <- function() {
        ini({
          tka <- 0.463613555325211
          label("Ka")
          tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)
          label("Log Cl")
          tv <- 3.46039743010498
          label("log V")
          add.sd <- c(0, 0.694761430696633)
          eta.ka ~ 0.400673718508127
          eta.cl ~ 0.069154564934726
          eta.v ~ 0.0191298379535425
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      m1 <- m1()

      m2 <- function() {
        ini({
          tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)
          label("Log Cl")
          tv <- 3.46039743010498
          label("log V")
          add.sd <- c(0, 0.694761430696633)
          eta.cl ~ 0.069154564934726
          eta.v ~ 0.0191298379535425
        })
        model({
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      m2 <- m2()

      expect_equal(testPipeQuote(m1, iniDf=m2$iniDf),
                   list(quote(tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)),
                        quote(tv <- 3.46039743010498),
                        quote(add.sd <- c(0, 0.694761430696633)),
                        quote(eta.cl ~ 0.069154564934726),
                        quote(eta.v ~ 0.0191298379535425)))

      expect_equal(testPipeQuote(m2, iniDf=m1$iniDf),
                   list(quote(tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)),
                        quote(tv <- 3.46039743010498),
                        quote(add.sd <- c(0, 0.694761430696633)),
                        quote(eta.cl ~ 0.069154564934726),
                        quote(eta.v ~ 0.0191298379535425)))

      m4 <- function() {
        ini({
          tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)
          label("Log Cl")
          tv <- 3.46039743010498
          label("log V")
          add.sd <- c(0, 0.694761430696633)
          eta..cl ~ 0.069154564934726
          eta..v ~ 0.0191298379535425
        })
        model({
          cl <- exp(tcl + eta..cl)
          v <- exp(tv + eta..v)
          linCmt() ~ add(add.sd)
        })
      }

      # no etas

      m4 <- m4()

      expect_equal(testPipeQuote(m4, iniDf=m1$iniDf),
                   list(quote(tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)),
                        quote(tv <- 3.46039743010498),
                        quote(add.sd <- c(0, 0.694761430696633))))

      expect_equal(testPipeQuote(m1, iniDf=m4$iniDf),
                   list(quote(tcl <- c(-Inf, 1.01211464338867, 4.60517018598809)),
                        quote(tv <- 3.46039743010498),
                        quote(add.sd <- c(0, 0.694761430696633))))

      # no thetas

      m5 <- function() {
        ini({
          t.cl <- c(-Inf, 1.01211464338867, 4.60517018598809)
          label("Log Cl")
          t.v <- 3.46039743010498
          label("log V")
          add..sd <- c(0, 0.694761430696633)
          eta.cl ~ 0.069154564934726
          eta.v ~ 0.0191298379535425
        })
        model({
          cl <- exp(t.cl + eta.cl)
          v <- exp(t.v + eta.v)
          linCmt() ~ add(add..sd)
        })
      }

      m5 <- m5()

      expect_equal(testPipeQuote(m5, iniDf=m1$iniDf),
                   list(quote(eta.cl ~ 0.069154564934726),
                        quote(eta.v ~ 0.0191298379535425)))

      expect_equal(testPipeQuote(m1, iniDf=m5$iniDf),
                   list(quote(eta.cl ~ 0.069154564934726),
                        quote(eta.v ~ 0.0191298379535425)))


      m6 <- function() {
        ini({
          t.cl <- c(-Inf, 1.01211464338867, 4.60517018598809)
          label("Log Cl")
          t.v <- 3.46039743010498
          label("log V")
          add..sd <- c(0, 0.694761430696633)
          eta..cl ~ 0.069154564934726
          eta..v ~ 0.0191298379535425
        })
        model({
          cl <- exp(t.cl + eta..cl)
          v <- exp(t.v + eta..v)
          linCmt() ~ add(add..sd)
        })
      }

      m6 <- m6()

      expect_equal(testPipeQuote(m6, iniDf=m1$iniDf),
                   list())

      expect_equal(testPipeQuote(m1, iniDf=m6$iniDf),
                   list())
    })
    test_that("model piping that shares err parameter#427", {
      u <- function() {
        ini({
          b <- 3
          err.sd <- 2
        })
        model({
          a <- x + err.sd
          c <- 1+b
          c ~ add(err.sd)
        })
      }

      expect_error(u %>% model(-a), NA)
    })

    test_that("adding a constant does not add to the ini block", {
      u <- function() {
        ini({
          b <- 3
          err.sd <- 2
        })
        model({
          a <- x + err.sd
          c <- 1+b
          c ~ add(err.sd)
        })
      }

      n <- u %>% model(aa <- pi+4, append=c)

      expect_false(any(n$iniDf$name == "pi"))
    })

    test_that("adding a line with a defined constant doesn't add to ini()", {
      u <- function() {
        ini({
          b <- 3
          err.sd <- 2
        })
        model({
          a <- x + err.sd
          aa <- 3
          c <- 1+b
          c ~ add(err.sd)
        })
      }

      n <- u %>% model(aaa <- aa+4, append=c)

      expect_false(any(n$iniDf$name == "aa"))
    })
  })


  test_that("test ui appending of derived variables like `sim` can work", {

    one.compartment <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.err <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    f <- rxode2(one.compartment)

    expect_error(model(f$simulationModel, sim2=sim+1, append=sim), NA)

  })


  test_that("off-diagonal piping issue #518", {

    mod <- function() {
      ini({
        a <- 1
        b <- 2
        etaa + etab ~ c(3, 0.1, 4)
        c <- 5
        etac ~ 6
        d <- 7
        f <- 9
        etad + etaf ~ c(8, 0.2, 10)
      })
      model({
        g <- (a + etaa)/(b + etab)
        h <- (c + etac)
        i <- (d + etad)
        j <- f + etaf
      })
    }

    modNew <-
      ini(
        rxode2(mod),
        etab + etac + etad ~
          c(7,
            0.2, 8,
            0.3, 0.4, 9),
        etaa ~ 0
      )

    expect_error(modNew$omega, NA)

  })

  test_that("piping append", {

    mod <- function() {
      ini({
        tka <- 0.45
        label("Ka")
        tcl <- 1
        label("Cl")
        tv <- 3.45
        label("V")
        add.sd <- c(0, 0.7)
        eta.cl ~ 0.3
        eta.v ~ 0.1
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl/v * center
        cp = center/v
        cp ~ add(add.sd)
      })
    }

    t <- c("-cp","-d/dt(depot)")
    expect_error(mod |> model(t), NA)

    t <- c("cp <- NULL","d/dt(depot) = NULL")
    expect_error(mod |> model(t), NA)

    t <- c("cp <- NULL","d/dt(depot) ~ NULL")
    expect_error(mod |> model(t), NA)

    mod5 <- mod |>
      model({
        PD <- 1-emax*cp/(ec50+cp)
        ##
        effect(0) <- e0
        kin <- e0*kout
        d/dt(effect) <- kin*PD -kout*effect
      }, append=d/dt(center))

    expect_equal(mod5$theta, c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7))

    mod5 <- mod |>
      model({
        PD <- 1-emax*cp/(ec50+cp)
        ##
        effect(0) <- e0
        kin <- e0*kout
        d/dt(effect) <- kin*PD -kout*effect
      }, append="d/dt(center)")

    expect_equal(mod5$theta, c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7))

    mod6 <- mod5 |>
      model({
        emax <- exp(temax)
        e0 <- exp(te0 + eta.e0)
        ec50 <- exp(tec50)
        kin <- exp(tkin)
        kout <- exp(tkout)
      }, append=NA)

    expect_equal(mod6$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, temax = 1, te0 = 1, tec50 = 1, tkin = 1, tkout = 1))

    expect_equal(
      mod6$omega,
      lotri({
        eta.cl ~ 0.3
        eta.v ~ 0.1
        eta.e0 ~ 1
      }))

    mod6 <- mod5 |>
      model({
        emax <- exp(temax)
        e0 <- exp(te0 + eta.e0)
        ec50 <- exp(tec50)
        kin <- exp(tkin)
        kout <- exp(tkout)
      }, append=FALSE)

    expect_equal(
      mod6$omega,
      lotri({
        eta.cl ~ 0.3
        eta.v ~ 0.1
        eta.e0 ~ 1
      }))

    expect_equal(mod6$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, temax = 1, te0 = 1, tec50 = 1, tkin = 1, tkout = 1))

    mod6 <- mod5 |>
      model({
        emax <- exp(temax)
        e0 <- exp(te0 + eta.e0)
        ec50 <- exp(tec50)
        kin <- exp(tkin)
        kout <- exp(tkout)
      }, append=0)

    expect_equal(
      mod6$omega,
      lotri({
        eta.cl ~ 0.3
        eta.v ~ 0.1
        eta.e0 ~ 1
      }))

    expect_equal(mod6$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, temax = 1, te0 = 1, tec50 = 1, tkin = 1, tkout = 1))

    # make sure auto model piping turns off

    withr::with_options(list(rxode2.autoVarPiping=FALSE),
                        mod7 <- mod5 |>
                          model({
                            emax <- exp(temax)
                            e0 <- exp(te0 + eta.e0)
                            ec50 <- exp(tec50)
                            kin <- exp(tkin)
                            kout <- exp(tkout)
                          }, append=NA))

    expect_equal(mod7$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7))

    expect_equal(
      mod7$omega,
      lotri({
        eta.cl ~ 0.3
        eta.v ~ 0.1
      }))


  })
})
