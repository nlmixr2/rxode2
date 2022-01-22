# (Regression) test 3 multiple instances of rxode2 objects to ensure
# C symbols and operations don't conflict.

# Make sure C operations and symbols don't conflict

test.dir <- tempfile("Rxmult-")

## rxode2 instance 1
m1 <-
  rxode2(
    model = "
         C2 = centr/V2;
         C3 = peri/V3;
         d/dt(depot) =-KA*depot;
         d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
         d/dt(peri)  =                    Q*C2 - Q*C3;
         d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;",
    modName = "inst1",
    wd = test.dir
  )

test_that("rxode2 instance 1 is created", {
  expect_s3_class(m1, "rxode2")
})

et1 <- eventTable(amount.units = "ug", time.units = "hours")
et1$add.dosing(dose = 10000, nbr.doses = 5, dosing.interval = 24)
et1$add.sampling(0:24)
et1$add.sampling(seq(from = 24 + 8, to = 5 * 24, by = 8))

test_that("rxode2 event table 1 was created", {
  expect_s3_class(et1, "rxEt")
  expect_equal(et1$get.nobs(), 37)
  expect_equal(length(et1$get.dosing()[, 1]), 1)
})

o1.first <- NULL
test_that("warning", {
  expect_warning({
    o1.first <<- m1$solve(
      params = c(
        KA = .291, CL = 18.6, V2 = 40.2, Q = 10.5, V3 = 297.0,
        Kin = 1.0, Kout = 1.0, EC50 = 200.0
      ),
      events = et1,
      inits = c(0, 0, 0, 1)
    )
  })
})

test_that("rxode2 event 1 solved.", {
  expect_equal(length(o1.first[, 1]), et1$get.nobs())
})

## rxode2 instance 2 (complete example)
m2 <-
  rxode2(
    model = "d/dt(y) = r * y * (1.0 - y/K);",
    modName = "inst2",
    wd = test.dir
  )

test_that("rxode2 instance 2 was created", {
  expect_s3_class(m1, "rxode2")
})

et2 <- eventTable(time.units = NA)
et2$add.sampling(seq(from = 0, to = 20, by = 0.2))

test_that("rxode2 event table 2 was created", {
  expect_s3_class(et1, "rxEt")
  expect_equal(et2$get.nobs(), 101)
  expect_equal(length(et2$get.dosing()[, 1]), 0)
})

o2.s <-
  m2$solve(params = c(r = 1, K = 10), events = et2, inits = c(y = 2), method = "lsoda")

test_that("rxode2 instance 2 was solved", {
  expect_equal(length(o2.s[, 1]), et2$get.nobs())
})

## rxode2 instance 3 (complete example)
m3 <-
  rxode2(
    model = "
         d/dt(X) = a*X + Y*Z;
         d/dt(Y) = b*(Y - Z);
         d/dt(Z) = -X*Y + c*Y - Z;",
    modName = "inst3",
    wd = tempdir() # don't pollute "./tests"
  )

test_that("rxode2 instance 3 is created", {
  expect_s3_class(m3, "rxode2")
})

et3 <- eventTable() # default time units
et3$add.sampling(seq(from = 0, to = 100, by = 0.01))

test_that("rxode2 instance 3 event table is created", {
  expect_s3_class(et3, "rxEt")
  expect_equal(et3$get.nobs(), 10001)
})

o3 <-
  m3$solve(
    params = c(a = -8 / 3, b = -10, c = 28),
    events = et3,
    inits = c(X = 1, Y = 1, Z = 1)
  )

test_that("rxode2 instance 3 was solved", {
  expect_equal(et3$get.nobs(), length(o3[, 1]))
})

## Now go back to model 1 for (same) integration
o1.second <- NULL
test_that("Warning when inits is unnamed", {
  expect_warning({
    o1.second <<- m1$solve(
      params = c(
        KA = .291, CL = 18.6, V2 = 40.2, Q = 10.5, V3 = 297.0,
        Kin = 1.0, Kout = 1.0, EC50 = 200.0
      ),
      events = et1,
      inits = c(0, 0, 0, 1)
    )
  })
})

test_that("Second solve of model 1 gives the same results", {
  expect_equal(o1.first, o1.second)
})

## and go back to model 2 for different ode solver
o2.ns <-
  m2$solve(params = c(r = 1, K = 10), events = et2, inits = c(y = 12), method = "dop853")

test_that("Differnt solve of Model 1 still works", {
  expect_equal(length(o2.ns[, 1]), length(o2.s[, 1]))
})

## Inspect the internal compilation manager in each of the rxode2 objects
prt <-
  function(obj, expect) {
    ## cat(
    ##     sprintf('Model name: %s\nDyn lib: %s\node_solver symbol: %s\n',
    ##             obj$modName, obj$cmpMgr$dllfile, obj$cmpMgr$ode_solver
    ##             ))
    expect_equal(obj$modName, expect)
    expect_match(obj$cmpMgr$dllfile, expect)
  }

test_that("Compilation manager elements make sense.", {
  prt(m1, "inst1")
  prt(m2, "inst2")
  prt(m3, "inst3")
})

## Unload object code (this is platform-dependent, as documented in the
## "Note" section of help("dyn.load"). Remove test.dir.
test_that("Unloading will allow removal of directory", {
  m1$dynUnload()
  m2$dynUnload()
  m3$dynUnload()
  .ret <- unlink(test.dir, recursive = TRUE)
  skip_on_os("windows")
  expect_equal(.ret, 0) # 0==success, 1==failed
})
