rxode2Test(
  {
    library(dplyr)
    library(testthat)
    ms <- c("liblsoda", "lsoda", "dop853")

    for (m in ms) {
      context(sprintf("Modeled rate (%s)", m))

      mod.rate <- rxode2({
        a <- 6
        b <- 0.6
        f <- 1
        ri <- 10
        li <- 0
        d / dt(intestine) <- -a * intestine
        f(intestine) <- f
        rate(intestine) <- ri
        alag(intestine) <- li
        d / dt(blood) <- a * intestine - b * blood
      })

      et <- et() %>%
        et(amt = 2 / 24, rate = -1, 0, addl = 9, ii = 1) %>%
        et(seq(0, 10, by = 1 / 24))


      et2 <- et() %>%
        et(amt = 2 / 24, rate = 2, 0, addl = 9, ii = 1) %>%
        et(seq(0, 10, by = 1 / 24))

      f1.a <- rxSolve(mod.rate, et, c(ri = 2, f = 1, li = 0), method = m)
      f1.b <- rxSolve(mod.rate, et2, c(ri = 0.1, f = 1, li = 0), method = m)

      test_that("rate=2, f=1, lag=0 makes sense for modeled rate", {
        expect_equal(seq(0, 10, by = 1 / 24), f1.a$time)
        expect_equal(seq(0, 10, by = 1 / 24), f1.b$time)
        expect_equal(as.data.frame(f1.a), as.data.frame(f1.b))
      })

      f1.a <- rxSolve(mod.rate, et, c(ri = 2, f = 1, li = 0.5), method = m)
      f1.b <- rxSolve(mod.rate, et2, c(ri = 0.1, f = 1, li = 0.5), method = m)

      test_that("rate=2, f=1, lag=0.5 makes sense for modeled rate", {
        expect_equal(seq(0, 10, by = 1 / 24), f1.a$time)
        expect_equal(seq(0, 10, by = 1 / 24), f1.b$time)
        expect_equal(as.data.frame(f1.a), as.data.frame(f1.b))
      })

      f1.a <- rxSolve(mod.rate, et, c(ri = 2, f = 2, li = 0.5), method = m)
      f1.b <- rxSolve(mod.rate, et2, c(ri = 0.1, f = 2, li = 0.5), method = m)

      test_that("rate=2, f=2, lag=0.5 makes sense for modeled rate & fixed rate", {
        expect_equal(seq(0, 10, by = 1 / 24), f1.a$time)
        expect_equal(seq(0, 10, by = 1 / 24), f1.b$time)
        expect_equal(as.data.frame(f1.a), as.data.frame(f1.b))
      })

      et <- et() %>%
        et(amt = 2 / 24, rate = -1) %>%
        et(amt = 0.2, time = 1) %>%
        et(seq(0, 10, by = 1 / 24))

      et2 <- et() %>%
        et(amt = 2 / 24, rate = 2) %>%
        et(amt = 0.2, time = 1) %>%
        et(seq(0, 10, by = 1 / 24))

      f.a <- rxSolve(mod.rate, et, c(ri = 2, f = 2, li = 0.3), method = m)
      f.b <- rxSolve(mod.rate, et2, c(ri = 1e-6, f = 2, li = 0.3), method = m)

      test_that("rate=2 makes sense for modeled rate", {
        expect_equal(seq(0, 10, by = 1 / 24), f.a$time)
        expect_equal(seq(0, 10, by = 1 / 24), f.b$time)
        expect_equal(as.data.frame(f.a), as.data.frame(f.b))
      })

      f.a <- rxSolve(mod.rate, et, c(ri = 0.125, f = 2, li = 0.3), method = m)

      et2 <- et() %>%
        et(amt = 2 / 24, rate = 0.125) %>%
        et(amt = 0.2, time = 1) %>%
        et(seq(0, 10, by = 1 / 24))

      f.b <- rxSolve(mod.rate, et2, c(ri = 1e-6, f = 2, li = 0.3), method = m)

      test_that("rate=0.125, f=2, lag=0.3 with 2 doses makes sense for modeled rate", {
        expect_equal(seq(0, 10, by = 1 / 24), f.a$time)
        expect_equal(seq(0, 10, by = 1 / 24), f.b$time)
        expect_equal(round(as.data.frame(f.a), 4), round(as.data.frame(f.b), 4))
      })

      f.a <- rxSolve(mod.rate, et, c(ri = 0.05, f = 2, li = 0.3), method = m)

      et2 <- et() %>%
        et(amt = 2 / 24, rate = 0.05) %>%
        et(amt = 0.2, time = 1) %>%
        et(seq(0, 10, by = 1 / 24))

      f.b <- rxSolve(mod.rate, et2, c(ri = 1, f = 2, li = 0.3), method = m)

      et3 <- et() %>%
        et(amt = 2 / 24 * 2, rate = 0.05) %>%
        et(amt = 0.2 * 2, time = 1) %>%
        et(seq(0, 10, by = 1 / 24))

      f.c <- rxSolve(mod.rate, et3, c(ri = 1, f = 1, li = 0.3), method = m)

      test_that("rate=0.05, f=2, lag=0.3 makes sense for modeled rate", {
        expect_equal(seq(0, 10, by = 1 / 24), f.a$time)
        expect_equal(seq(0, 10, by = 1 / 24), f.b$time)
        expect_equal(as.data.frame(f.a), as.data.frame(f.b))
      })

      test_that("bad rates (zero/negative) throw errors", {
        expect_error(rxSolve(mod.rate, et, c(ri = -1, f = 1, li = 0.3), method = m))
        expect_error(rxSolve(mod.rate, et, c(ri = 0, f = 1, li = 0.3), method = m))
      })

      mod.dur <- rxode2({
        a <- 6
        b <- 0.6
        f <- 1
        di <- 3
        li <- 0
        d / dt(intestine) <- -a * intestine
        f(intestine) <- f
        dur(intestine) <- di
        alag(intestine) <- li
        d / dt(blood) <- a * intestine - b * blood
      })

      test_that("Error when rate is requested but not in table", {
        expect_error(rxSolve(mod.dur, et, c(ri = 1, f = 1, li = 0.3), method = m))
      })

      context(sprintf("Modeled duration (%s)", m))

      ## Now model duration
      et <- et(amt = 2 / 24 * 2, rate = -2) %>%
        et(amt = 0.2 * 2, time = 2) %>%
        et(seq(0, 10, by = 1 / 24))

      test_that("Error is thrown without modeled duration", {
        expect_error({
          rxSolve(mod.rate, et, c(ri = 1, f = 1, li = 0.3), method = m)
        })
      })

      f.a <- rxSolve(mod.dur, et, c(di = 0.5, f = 1, li = 0.3), method = m)

      et2 <- et() %>%
        et(amt = 2 / 24 * 2, dur = 0.5) %>%
        et(amt = 0.2 * 2, time = 2) %>%
        et(seq(0, 10, by = 1 / 24))

      f.b <- rxSolve(mod.dur, et2, c(di = 10, f = 1, li = 0.3), method = m)

      test_that("Duration 0.5, F=1, li=0.3", {
        expect_equal(as.data.frame(f.a), as.data.frame(f.b))
      })

      et2 <- et() %>%
        et(amt = 2 / 24 * 2, dur = 0.5) %>%
        et(amt = 0.2 * 2, time = 2) %>%
        et(seq(0, 10, by = 1 / 24))

      f.a <- rxSolve(mod.dur, et, c(di = 0.5, f = 1, li = 0.3), method = m)

      f.b <- rxSolve(mod.dur, et2, c(di = 10, f = 1, li = 0.3), method = m)

      test_that("Duration 0.5, F=1, li=0.3", {
        expect_equal(as.data.frame(f.a), as.data.frame(f.b))
      })

      f.a <- rxSolve(mod.dur, et, c(di = 0.5, f = 2, li = 0.3), method = m)

      f.b <- rxSolve(mod.dur, et2, c(di = 10, f = 2, li = 0.3), method = m)

      test_that("Duration 0.5, F=1, li=0.3", {
        expect_equal(as.data.frame(f.a), as.data.frame(f.b))
      })
    }

    context("error when bioavaibility(state)+rate/dur, but maintain f(state) #216 #222")

    mod.rate <- rxode2({
      a <- 6
      b <- 0.6
      f <- 1
      ri <- 10
      li <- 0
      d / dt(intestine) <- -a * intestine
      f(intestine) <- f
      rate(intestine) <- ri * intestine
      alag(intestine) <- li
      d / dt(blood) <- a * intestine - b * blood
    })

    et <- et() %>%
      et(amt = 2 / 24, rate = -1, 0, addl = 9, ii = 1) %>%
      et(seq(0, 10, by = 1 / 24))

    expect_error(rxSolve(mod.rate, et))

    mod.rate <- rxode2({
      a <- 6
      b <- 0.6
      f <- 1
      ri <- 10
      li <- 0
      d / dt(intestine) <- -a * intestine
      f(intestine) <- f
      dur(intestine) <- ri * intestine
      alag(intestine) <- li
      d / dt(blood) <- a * intestine - b * blood
    })

    et <- et() %>%
      et(amt = 2 / 24, rate = -1, 0, addl = 9, ii = 1) %>%
      et(seq(0, 10, by = 1 / 24))

    expect_error(rxSolve(mod.rate, et))

    mod.rate <- rxode2({
      a <- 6
      b <- 0.6
      f <- 1
      ri <- 10
      li <- 0
      d / dt(intestine) <- -a * intestine
      f(intestine) <- f * intestine
      rate(intestine) <- ri
      alag(intestine) <- li
      d / dt(blood) <- a * intestine - b * blood
    })

    et <- et() %>%
      et(amt = 2 / 24, rate = 1, 0, addl = 9, ii = 1) %>%
      et(seq(0, 10, by = 1 / 24))

    expect_error(rxSolve(mod.rate, et))

    et <- et() %>%
      et(amt = 2 / 24, rate = -1, 0, addl = 9, ii = 1) %>%
      et(seq(0, 10, by = 1 / 24))

    expect_error(rxSolve(mod.rate, et))

    mod.rate <- rxode2({
      a <- 6
      b <- 0.6
      f <- 1
      ri <- 10
      li <- 0
      d / dt(intestine) <- -a * intestine
      f(intestine) <- f * intestine
      dur(intestine) <- ri
      alag(intestine) <- li
      d / dt(blood) <- a * intestine - b * blood
    })

    et <- et() %>%
      et(amt = 2 / 24, rate = -2, 0, addl = 9, ii = 1) %>%
      et(seq(0, 10, by = 1 / 24))

    expect_error(rxSolve(mod.rate, et))

    ## Test that state-based bio-availability's does work
    CLp <- 25
    CLh <- 1
    fuinc <- 1
    Nh <- 0.4
    Vh <- 3.9
    V1 <- 1800
    ke <- 0.02
    sample.int <- 24
    Sample.V <- 20
    Days <- 5
    ini <- c(A1 = 1000, A2 = 0, V.ms = 0)
    mod <- "
  d/dt(V.ms) = 0

  Vme = ke * time
  V1 = V1 - Vme - V.ms
  C1 = A1 / V1
  C2 = A2 / V2
  d/dt(A1) =  CLp * fuinc * (C2 - C1)
  f(A1)    = (V1 - Sample.V) / V1
  d/dt(A2) =  CLp * fuinc * (C1 - C2) - CLh * C2 * Nh
  d/dt(M) = CLh * C2 * Nh
"
    mod <- rxode2(model = mod)

    par <- c(
      CLp = CLp, CLh = CLh, fuinc = fuinc, Nh = Nh, V2 = Vh,
      V1 = V1 - Sample.V, Sample.V = Sample.V, ke = ke, Cp1 = 0
    )

    sim.ev <- et(seq(0, Days * 60 * 24, by = 1)) %>%
      et(cmt = "V.ms", evid = 4, amt = Sample.V, nbr = 5, ii = sample.int * 60) %>%
      et(cmt = "A1", time = 1440, evid = 6, amt = 1)

    expect_error(rxSolve(mod, par, sim.ev, inits = ini, addDosing = TRUE), NA)
  },
  test = "lvl2"
)


## et <- eventTable(time.units="days")
## et$add.sampling(seq(0,10,by=1/24))
## et$add.dosing(dose=2/24,rate=2,start.time=0,
##               nbr.doses=1,dosing.interval=1)

## et1 <- et$get.EventTable() %>% filter(is.na(amt) | amt > 0) %>%
##     mutate(evid=ifelse(evid==0, 0, 90101)) %>%
##     mutate(amt=ifelse(is.na(amt),NA_real_,0.4))

## et2 <- et1 %>% filter(evid !=0) %>% mutate(evid=70101)

## et3 <- rbind(et1, et2, data.frame(time=1,evid=101,amt=0.2)) %>% arrange(time,-evid)

## f <- rxSolve(mod.rate, et3, c(ri=2,f=2,li=0.3)); ggplot(f,aes(time,intestine)) + geom_line()

## f <- rxSolve(mod.rate, et3, c(ri=0.5,f=2,li=0.3)); ggplot(f,aes(time,intestine)) + geom_line()
