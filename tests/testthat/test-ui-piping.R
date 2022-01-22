.rx <- loadNamespace("rxode2")

testPipeQuote <- function(..., envir=parent.frame()) {
  .rx$.quoteCallInfoLines(match.call(expand.dots = TRUE)[-1], envir=envir)
}

test_that("test of standard quoting of piping arguments", {
  expect_equal(testPipeQuote(tka=0.5, {
    tv = 3
    tcl = 10
    eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
    cl = exp(tcl + eta.cl)
  }, eta.ka ~ 3, eta.ka ~ 3,
  {
    tv = 3
    tcl = 10
    eta.v+eta.cl~unfix(cor(sd(0.3,0.02,0.1)))
  }), list(quote(tka <- 0.5),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1)))),
           quote(cl <- exp(tcl + eta.cl)),
           quote(eta.ka ~ 3),
           quote(eta.ka ~ 3),
           quote(tv <- 3),
           quote(tcl <- 10),
           quote(eta.v + eta.cl ~ unfix(cor(sd(0.3, 0.02, 0.1))))))
  
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
  
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), 6L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), 6L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
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
    F(depot) <- 3
    cp <- center / v
    cp ~ add(add.err)
  })
}

f <- rxode2(one.compartment)

test_that("Model Line from Expression, upper case F()", {
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), 6L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), 6L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), 6L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), 6L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), 6L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), 6L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), 6L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), 6L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(not), f), NA_integer_)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), NULL)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -5L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -5L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -5L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -5L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -5L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -5L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(not), f), NA_integer_)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), NULL)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), NULL)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(not), f), NA_integer_)
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
  expect_equal(.rx$.getModelLineFromExpression(quote(ka), f), 1L)
  expect_equal(.rx$.getModelLineFromExpression(quote(d/dt(depot)), f), 4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(f(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(F(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(lag(depot)), f), NULL)
  expect_equal(.rx$.getModelLineFromExpression(quote(alag(depot)), f), NULL)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(rate(depot)), f), -4L)
  expect_equal(.rx$.getModelLineFromExpression(quote(dur(depot)), f), -4L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(not), f), NA_integer_)
  expect_equal(.rx$.getModelLineFromExpression(quote(cp), f), 8L)
  
  expect_equal(.rx$.getModelLineFromExpression(quote(cp), f, TRUE), 9L)
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
  
  expect_error(f %>% ini(tka=NULL), "tka")
  expect_error(f %>% ini(tka=c(3,2,1)), "tka")
  
  fFix <- f %>% ini(tka=fix)
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
  
  expect_error(f %>% ini(tka=NULL), "tka")
  expect_error(f %>% ini(tka=c(3,2,1)), "tka")
  
  fFix <- f %>% ini(tka=fix)
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
  
  expect_warning(expect_warning(expect_warning(
    testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, FALSE),
    regexp="unfix.*eta.cl"), regexp="unfix.*eta.cl,eta.v"), regexp="unfix.*eta.v"
  )
  expect_warning(expect_warning(expect_warning(
    testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, FALSE),
    regexp="unfix.*eta.cl"), regexp="unfix.*eta.cl,eta.v"), regexp="unfix.*eta.v"
  )
  expect_warning(expect_warning(expect_warning(
    testEst(f %>% ini(eta.cl+eta.v~unfix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, FALSE),
    regexp="unfix.*eta.cl"), regexp="unfix.*eta.cl,eta.v"), regexp="unfix.*eta.v"
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
  
  expect_error(f %>% ini(tka=NULL), "tka")
  expect_error(f %>% ini(tka=c(3,2,1)), "tka")
  
  fFix <- f %>% ini(tka=fix)
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
  
  expect_warning(expect_warning(expect_warning(
    testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.cl", -Inf, 0.3 * 0.3, Inf, TRUE),
    regexp="fix.*eta.cl"), regexp="fix.*eta.cl,eta.v"), regexp="fix.*eta.v"
  )
  expect_warning(expect_warning(expect_warning(
    testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "eta.v", -Inf, 0.1 * 0.1, Inf, TRUE),
    regexp="fix.*eta.cl"), regexp="fix.*eta.cl,eta.v"), regexp="fix.*eta.v"
  )
  expect_warning(expect_warning(expect_warning(
    testEst(f %>% ini(eta.cl+eta.v~fix(cor(sd(0.3,0.02,0.1)))), "(eta.cl,eta.v)", -Inf, 0.1 * 0.3 * 0.02, Inf, TRUE),
    regexp="fix.*eta.cl"), regexp="fix.*eta.cl,eta.v"), regexp="fix.*eta.v"
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
  if (!is.null(has)) {
    expect_true(all(has %in% paste(ui$ini$name)))
  }
  if (!is.null(values) && !is.null(names(values))) {
    .vals <- setNames(ui$ini$est, paste(ui$ini$name))
    .vals <- .vals[names(values)]
    expect_equal(values, .vals)
  }
  if (!is.null(exclude)) {
    expect_false(any(exclude %in% paste(ui$ini$name)))
  }
  ## General UI properties
  expect_true(all(!is.na(ui$ini$fix)))
  expect_true(all(!is.na(ui$ini$lower)))
  expect_true(all(!is.na(ui$ini$upper)))
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
    c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7))
  
  # context("update: Multiple component change with list()")
  testUi(
    f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), list(tcl = 3, tv = 4)),
    c("tka", "tcl", "tv", "eta.v", "add.err"),
    c("eta.ka", "eta.cl"),
    c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
  )
  
  # context("update: Multiple component change with assigned .tmp=list()")
  .tmp <- list(tcl = 3, tv = 4)
  .ui <- f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), .tmp)
  testUi(
    .ui,
    c("tka", "tcl", "tv", "eta.v", "add.err"),
    c("eta.ka", "eta.cl"),
    c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
  )
  
  # context("update: Multiple component change with assigned .tmp=c()")
  .tmp <- c(tcl = 3, tv = 4)
  .ui <- f %>% update(tka = 4, cl = exp(tcl), ka = exp(tka), .tmp)
  testUi(
    .ui,
    c("tka", "tcl", "tv", "eta.v", "add.err"),
    c("eta.ka", "eta.cl"),
    c(tka = 4, tcl = 3, tv = 4, eta.v = 0.1, add.err = 0.7)
  )
  
  # context("update: Multiple component change with assigned .tmp={}")
  .tmp <- quote({
    ka <- exp(tka)
  })
  .ui <- f %>% update(tka = 4, cl = exp(tcl), .tmp, c(tcl = 3, tv = 4))
  testUi(
    .ui,
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
  
  .ui <- one.cmt %>% update({
    linCmt() ~ add(add.err) + prop(prop.err)
  })
  
  expect_true(inherits(.ui, "rxUi"))
})

# piping looks through parent environments
test_that("Looks through prior frames for the correct object", {
  fit <- rxode2(one.compartment)
  fits <- lapply(seq(-1, -0.1, 0.1), function(kainit) {
    rxode2(update(fit, tka = kainit))
  })
  
  expect_true(inherits(fits, "list"))
  expect_error(lapply(seq(-1, -0.1, 0.1), function(kainit) {
    rxode2(update(fit, tka = matt))
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
  expect_error(f %>% ini(eta.ka + eta.matt ~ c(
    0.2,
    0.01, 0.2
  )))
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
  expect_error(f %>% update(eta.ka + eta.matt ~ c(
    0.2,
    0.01, 0.2
  )))
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
  
  expect_error(f %>% model(ipre ~ add(add.sd)) %>% ini(add.sd=sqrt(0.1)), NA)
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
    f %>% model(ipre ~ propF(prop.sd, f2)) %>% ini(prop.sd=sqrt(0.1))
  }
  
  f2 <- trans(f)
  
  expect_true(!any(f2$iniDf$name %in% c("f2")))
})