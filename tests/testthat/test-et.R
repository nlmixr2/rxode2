test_that("et import rate=-2", {

  d <- data.frame(id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
       time = c(0, 2, 12.5, 24.5, 37, 48, 60.5, 72.5, 85.3, 96.5, 108.5, 112.5),
       amt = c(25, 0, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 0),
       rate = c(-2, 0, -2, -2, -2, -2, -2, -2, -2, -2, -2, 0))
  d2 <- as.data.frame(et(d))
  expect_equal(d2$rate, c(-2, NA_real_, -2, -2, -2, -2, -2, -2, -2, -2, -2, NA_real_))


})


for (radi in c(1, 2)) {
  forderForceBase(switch(radi,
                         TRUE,
                         FALSE
  ))
  radix <- switch(radi,
                  "base::order",
                  "data.table::forder"
  )

  # context(sprintf("Test event Table et(...) sort:%s", radix))
  et <- et()

  test_that("Empty event table check", {
    ## expect_s3_class(et$env,"rxHidden")
    expect_equal(et$nobs, 0L)
    expect_equal(et$ndose, 0L)
    expect_equal(et$get.EventTable(), NULL)
    expect_equal(et$get.dosing(), NULL)
    expect_equal(et$get.sampling(), NULL)
  })

  et <- et(1:10)

  test_that("10 sample items", {
    expect_equal(et$nobs, 10L)
    expect_equal(et$ndose, 0L)
    expect_s3_class(et$get.EventTable(), "data.frame")
    expect_true(is(et, "rxEt"))
    expect_false(et$show["id"])
    expect_false(et$show["cmt"])
    expect_equal(et$get.dosing(), NULL)
    expect_equal(length(et$get.sampling()$time), 10)
  })

  et <- et(1:10, "matt")

  test_that("compartment check", {
    expect_equal(et$nobs, 10L)
    expect_equal(et$ndose, 0L)
    expect_s3_class(et$get.EventTable(), "data.frame")
    expect_true(is(et, "rxEt"))
    expect_false(et$show["id"])
    expect_true(et$show["cmt"])
    expect_equal(et$get.dosing(), NULL)
    expect_equal(length(et$get.sampling()$time), 10)
  })

  et <- et(1:10, cmt = 1)
  test_that("compartment check", {
    expect_equal(et$nobs, 10L)
    expect_equal(et$ndose, 0L)
    expect_s3_class(et$get.EventTable(), "data.frame")
    expect_true(is(et, "rxEt"))
    expect_false(et$show["id"])
    expect_true(et$show["cmt"])
    expect_equal(et$get.dosing(), NULL)
    expect_equal(length(et$get.sampling()$time), 10)
    expect_true(all(et$cmt == 1L))
  })



  et1 <- et(1:10, id = 1:10)

  test_that("Observation only table check", {
    expect_equal(et1$nobs, 100L)
    expect_equal(et1$ndose, 0L)
    expect_s3_class(et1$get.EventTable(), "data.frame")
    expect_true(is(et1, "rxEt"))
    expect_true(et1$show["id"])
    expect_false(et1$show["cmt"])
    expect_equal(et1$get.dosing(), NULL)
    expect_equal(length(et1$get.sampling()$time), 100)
  })

  ## now resize down
  et2 <- et1 %>% et(id = -(2:10))

  test_that("compartment check", {
    expect_equal(et2$nobs, 10L)
    expect_equal(et2$ndose, 0L)
    expect_s3_class(et2$get.EventTable(), "data.frame")
    expect_true(is(et, "rxEt"))
    expect_true(et2$show["id"])
    expect_false(et2$show["cmt"])
    expect_equal(et2$get.dosing(), NULL)
    expect_equal(length(et2$get.sampling()$time), 10)
  })


  ## now resize back up
  et3 <- et2 %>% et(id = 1:10)

  test_that("Using simulate with et without windows will warn", {
    f1 <- as.data.frame(et3)
    f2 <- suppressWarnings({
      as.data.frame(simulate(et3))
    })
    expect_equal(f1$time, f2$time)
    expect_warning(simulate(et3))
  })

  eti <- et(amt = 10) %>% et(id = 2:3)
  eti1 <- et(amt = 10, id = 3:4)
  eti2 <- et(amt = 10, id = 3)

  eti3 <- et(10, id = 3:4)
  eti4 <- et(10, id = 3)

  eti5 <- et(10) %>% et(id = 2:3)

  eti6 <- et(amt = 10) %>% et(id = 2)

  eti7 <- et(10) %>% et(id = 2)


  eti8 <- et(10, id = 1)
  eti9 <- et(10) %>% et(id = 1)
  eti10 <- et(amt = 10, id = 1)
  eti11 <- et(amt = 10) %>% et(id = 1)

  ## Now look at windows

  eti12 <- et(list(c(10, 11)), id = 1)
  eti13 <- et(list(c(10, 11))) %>% et(id = 1)

  eti14 <- et(list(c(10, 11)), amt = 10, id = 1)
  eti15 <- et(list(c(10, 11)), amt = 10) %>% et(id = 1)


  eti16 <- et(list(c(10, 11)), id = 2)
  eti17 <- et(list(c(10, 11))) %>% et(id = 2)

  eti18 <- et(list(c(10, 11)), amt = 10, id = 2)
  eti19 <- et(list(c(10, 11)), amt = 10) %>% et(id = 2)

  eti20 <- et(list(c(10, 11)), id = 2:3)
  eti21 <- et(list(c(10, 11))) %>% et(id = 2:3)

  eti22 <- et(list(c(10, 11)), amt = 10, id = 2:3)
  eti23 <- et(list(c(10, 11)), amt = 10) %>% et(id = 2:3)

  ## Now do it with dosing/sampling windows

  test_that("Make sure that et only has IDs 2 and 3.", {
    expect_equal(eti$id, 2:3)
    expect_equal(eti1$id, 3:4)
    expect_equal(eti2$id, 3)
    expect_equal(eti3$id, 3:4)
    expect_equal(eti4$id, 3)
    expect_equal(eti5$id, 2:3)
    expect_equal(eti6$id, 2)
    expect_equal(eti7$id, 2)
    expect_equal(eti8$id, 1)
    expect_true(eti8$env$show["id"])
    expect_equal(eti9$id, 1)
    expect_true(eti9$env$show["id"])
    expect_equal(eti10$id, 1)
    expect_true(eti10$env$show["id"])
    expect_equal(eti11$id, 1)
    expect_true(eti11$env$show["id"])
    expect_equal(eti12$id, 1)
    expect_true(eti12$env$show["id"])
    expect_equal(eti13$id, 1)
    expect_true(eti13$env$show["id"])
    expect_equal(eti14$id, 1)
    expect_true(eti14$env$show["id"])
    expect_equal(eti15$id, 1)
    expect_true(eti15$env$show["id"])
    expect_equal(eti16$id, 2)
    expect_true(eti16$env$show["id"])
    expect_equal(eti17$id, 2)
    expect_true(eti17$env$show["id"])
    expect_equal(eti18$id, 2)
    expect_true(eti18$env$show["id"])
    expect_equal(eti19$id, 2)
    expect_true(eti19$env$show["id"])
    expect_equal(eti20$id, 2:3)
    expect_true(eti20$env$show["id"])
    expect_equal(eti21$id, 2:3)
    expect_true(eti21$env$show["id"])
    expect_equal(eti22$id, 2:3)
    expect_true(eti22$env$show["id"])
    expect_equal(eti23$id, 2:3)
    expect_true(eti23$env$show["id"])
  })


  test_that("Observation only table check", {
    expect_equal(et1$nobs, 100L)
    expect_equal(et1$ndose, 0L)
    expect_s3_class(et1$get.EventTable(), "data.frame")
    expect_true(is(et1, "rxEt"))
    expect_true(et1$show["id"])
    expect_false(et1$show["cmt"])
    expect_equal(et1$get.dosing(), NULL)
    expect_equal(length(et1$get.sampling()$time), 100)
  })

  ## Check adding different units of time, rate, amt work
  test_that("units tests", {
    skip_if_not_installed("units")
    ## Make sure units are right.
    et3 <- et3 %>% units::set_units(mg)
    e <- et(amount.units = "mg", time_units = "hr") %>%
      add.sampling(seq(0, 24, by = 3)) %>%
      add.dosing(1, 3) %>%
      et(rate = 3, amt = 2, time = 120)
    e2 <- e %>% et(amt = units::set_units(0.0003, "lb"), time = 0.5)
    expect_equal(e2$amt[e2$time == units::set_units(0.5, hr)], units::set_units(units::set_units(0.0003, lb), mg))
    e2 <- e %>% et(units::set_units(30, min))
    expect_true(any(e2$time == units::set_units(0.5, hr)))
    e2 <- e %>% et(time = 0.25, rate = units::set_units(30, ug / min), amt = units::set_units(4, ug))
    tmp <- e2[e2$time == units::set_units(0.25, hr), ]
    expect_equal(units::set_units(1.8, mg / h), tmp$rate)
    expect_equal(units::set_units(0.004, mg), tmp$amt)
    e2 <- e %>% et(time = 0.25, ii = units::set_units(30, min), amt = 4, addl = 4)
    expect_equal(e2$ii[e2$time == units::set_units(0.25, hr)], units::set_units(0.5, hr))

    ## Check importing wrong different ii and time units as well as different rate units work.
    e <- et(amount.units = "mg", time_units = "hr") %>%
      add.sampling(seq(0, 24, by = 3)) %>%
      add.dosing(1, 3) %>%
      et(rate = 3, amt = 2, time = 120)

    etDf <- as.data.frame(e)
    etDf$rate <- units::set_units(etDf$rate, ug / s)
    etDf$ii <- units::set_units(etDf$ii, min)

    et <- et()
    et$import.EventTable(etDf)

    expect_equal(et$ii, e$ii)
    expect_equal(et$rate, e$rate)
  })

  test_that("seq works with wait", {

    e1 <- et(amt = 100, ii = 24, addl = 6)

    e2 <- et(amt = 200, ii = 24, addl = 6)

    e3 <- seq(e1, e2, e1)

    e4 <- seq(e1, wait = 72, e2, wait = 72, e1) %>% as.data.frame()

    expect_equal(structure(list(
      time = c(0, 216, 432),
      amt = c(100, 200, 100),
      ii = c(24, 24, 24),
      addl = c(6L, 6L, 6L),
      evid = c(1L, 1L, 1L)
    ),
    class = "data.frame",
    row.names = c(NA, -3L)
    ), e4)

    e5 <- etSeq(e1, wait = 72, e2, wait = 72, e1, waitII = "+ii") %>%
      as.data.frame()

    expect_equal(structure(list(
      time = c(0, 240, 480),
      amt = c(100, 200, 100),
      ii = c(24, 24, 24),
      addl = c(6L, 6L, 6L),
      evid = c(1L, 1L, 1L)
    ),
    class = "data.frame",
    row.names = c(NA, -3L)
    ), e5)

    e1 <- et(amt = 500)
    e2 <- et(amt = 250, ii = 24, addl = 4)

    expect_equal(
      structure(list(
        time = c(0, 24),
        amt = c(500, 250),
        ii = c(0, 24),
        addl = c(0L, 4L),
        evid = c(1L, 1L)
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
      ),
      c(e1, e2) %>% as.data.frame()
    )

    expect_equal(
      structure(list(
        time = c(0, 120, 144),
        amt = c(250, 500, 250),
        ii = c(24, 0, 24),
        addl = c(4L, 0L, 4L),
        evid = c(1L, 1L, 1L)
      ),
      class = "data.frame",
      row.names = c(NA, -3L)
      ),
      c(e2, e1, e2) %>% as.data.frame()
    )

    e3 <- et(amt = 200)

    expect_warning(c(e1, e3))

    e4 <- suppressWarnings(c(e1, e3) %>% as.data.frame())

    expect_equal(
      structure(list(
        time = c(0, 24),
        amt = c(500, 200),
        ii = c(0, 0),
        addl = c(0L, 0L),
        evid = c(1L, 1L)
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
      ),
      e4
    )

    e4 <- suppressWarnings(c(e1, e3, ii = 12) %>% as.data.frame())

    expect_equal(
      structure(list(
        time = c(0, 12),
        amt = c(500, 200),
        ii = c(0, 0),
        addl = c(0L, 0L),
        evid = c(1L, 1L)
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
      ),
      e4
    )


    e1 <- et(amt = 100, ii = 24, addl = 6) %>%
      et(seq(0, 2 * 168, by = 0.1))
    e2 <- c(e1, e1, samples = "use")
    expect_equal(range(e2$time), c(0, 672))

    ## combine without changing, use rbind
    e1 <- et(amt = 100, ii = 24, addl = 6, ID = 1:5)
    e2 <- et(amt = 50, ii = 12, addl = 13, ID = 1:3)
    e3 <- et(amt = 200, ii = 24, addl = 2, ID = 1:2)

    e4 <- rbind(e1, e2, e3)
    expect_equal(
      e4 %>% dplyr::select(id, time, amt, ii, addl) %>% as.data.frame(),
      structure(list(
        id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 5L),
        time = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        amt = c(100, 50, 200, 100, 50, 200, 100, 50, 100, 100),
        ii = c(24, 12, 24, 24, 12, 24, 24, 12, 24, 24),
        addl = c(6L, 13L, 2L, 6L, 13L, 2L, 6L, 13L, 6L, 6L)
      ),
      class = "data.frame",
      row.names = c(NA, -10L)
      )
    )

    e4 <- rbind(e1, e2, e3, id = "unique")
    expect_equal(
      e4 %>% dplyr::select(id, time, amt, ii, addl) %>% as.data.frame(),
      structure(list(
        id = 1:10,
        time = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        amt = c(100, 100, 100, 100, 100, 50, 50, 50, 200, 200),
        ii = c(24, 24, 24, 24, 24, 12, 12, 12, 24, 24),
        addl = c(6L, 6L, 6L, 6L, 6L, 13L, 13L, 13L, 2L, 2L)
      ),
      class = "data.frame",
      row.names = c(NA, -10L)
      )
    )
  })

  ## FIXME test windows and dur
  test_that("no ii throws an error with addl", {
    expect_error(et(c(11, 13), amt = 10, addl = 3))
  })
  ## Test Windows
  et <- et(list(
    c(0, 1),
    c(1, 4),
    c(4, 8),
    c(8, 12),
    c(12, 24)
  )) %>%
    et(amt = 10) %>%
    et(c(11, 13), amt = 10, addl = 3, ii = 12)

  test_that("Using simulate with et works and gives a different data frame", {
    et2 <- as.data.frame(simulate(et))
    et1 <- as.data.frame(et)
    expect_false(all(et1$time == et2$time))
    et3 <- as.data.frame(et)
    expect_true(all(et1$time == et3$time))
    et$simulate()
    et3 <- as.data.frame(et)
    expect_false(all(et1$time == et3$time))
  })

  test_that("Low/High middle tests; i.e windows", {
    et2 <- et[which(!is.na(et$low)), ]
    expect_true(et$show["low"])
    expect_true(et$show["high"])
    expect_true(all(et2$time < et2$high))
    expect_true(all(et2$time > et2$low))
  })

  et <- et(list(
    c(0, 1),
    c(1, 4),
    c(4, 8),
    c(8, 12),
    c(12, 24)
  ), cmt = 1) %>%
    et(amt = 10) %>%
    et(c(11, 13), amt = 10, addl = 3, ii = 12)

  test_that("Low/High middle tests; i.e windows", {
    et2 <- et[which(!is.na(et$low)), ]
    expect_true(et$show["low"])
    expect_true(et$show["high"])
    expect_true(all(et2$time < et2$high))
    expect_true(all(et2$time > et2$low))
  })

  test_that("Window Errors", {
    expect_error(et(list(c(1, 0))))
    expect_error(et(list(c(0, 1, 2))))
  })
  test_that("until is inclusive", {
    expect_equal(et(amt = 1, time = 50, until = 57.5, ii = 1.5)$addl, 5)
    expect_equal(et(amt = 1, time = 50, until = 57.49999, ii = 1.5)$addl, 4)
    expect_equal(et(amt = 1, time = 50, until = 57.50001, ii = 1.5)$addl, 5)
  })

  test_that("et expected errors", {
    expect_error(et(list(c(2, 1), c(3, 4)), amt = 3))
    expect_error(et(list(c(1, 2), 3, c(1, 2, 3)), amt = 3))
    expect_error(et(list(c(1, 2), 3, TRUE), amt = 3))
  })

  test_that("et steady state constant infusion", {
    expect_error(et(amt = 0, rate = 10, ii = 0, ss = 2))
    expect_error(et(amt = 0, rate = 10, ii = 2, ss = 1))
    expect_error(et(amt = 0, rate = -2, ii = 0, ss = 1))
    expect_error(et(rate = 10, ii = 0, ss = 2))
    expect_error(et(rate = 10, ii = 2, ss = 1))
    expect_error(et(rate = -2, ii = 0, ss = 1))

    t1 <- et(amt = 0, rate = 10, ii = 0, ss = 1) %>% as.data.frame()
    t2 <- et(rate = 10, ii = 0, ss = 1) %>% as.data.frame()
    expect_equal(t1, t2)

    t1 <- et(amt = 0, rate = -1, ii = 0, ss = 1) %>% as.data.frame()
    t2 <- et(rate = -1, ii = 0, ss = 1) %>% as.data.frame()
    expect_equal(t1, t2)
  })

  test_that("et addl expand", {
    ev <- et(amt = 3, ii = 24, until = 120)
    tmp <- etExpand(ev)
    expect_equal(ev$amt, 3)
    expect_equal(tmp$time, c(0, 24, 48, 72, 96, 120))
    ev$expand()
    expect_equal(ev$time, c(0, 24, 48, 72, 96, 120))
  })

  ev2 <- et(amt = 3, ii = 24, until = 120) %>% et(amt = 3, rate = 7)

  test_that("data.frame conversion", {
    tmp <- data.frame(ev2)
    expect_equal(names(tmp), c("time", "amt", "rate", "ii", "addl", "evid"))
    expect_false(inherits(tmp$rate, "rxRateDur"))
    expect_false(inherits(tmp$evid, "rxEvid"))
  })

  test_that("tibble conversion", {
    tmp <- tibble::as_tibble(ev2)
    expect_equal(names(tmp), c("time", "amt", "rate", "ii", "addl", "evid"))
    expect_false(inherits(tmp$rate, "rxRateDur"))
    expect_false(inherits(tmp$evid, "rxEvid"))
  })
}

test_that("seq() args work; see #97", {
  et1 <- et() %>% add.sampling(seq(0, 24, by = 3))

  et2 <- et(from = 0, to = 24, by = 3)

  expect_equal(et1$time, et2$time)
})

test_that("errors", {
  expect_error(et(ii = 12, dosing.interval = 14))
  expect_error(et(dose = 4, amt = 3))
  expect_error(et(cmt = 1, dosing.to = 3))
  expect_error(et(cmt = 1, dose.to = 3))
  expect_error(et(cmt = 1, state = 3))
  expect_error(et(amt.units = "mg", dose.units = "mg"))
  expect_error(et(time.units = "mg", timeUnits = "mg"))
  expect_error(et(time = 4, start.time = 5))
  expect_error(et(nbr.doses = 4, nbrDoses = 5))
  expect_error(et(dur = 4, duration = 5))
})

test_that("dose=0 is OK; see #192", {
  ev1 <- et(amt = 0, time = 10)
  ev2 <- eventTable()
  ev2$add.dosing(dose = 0, start.time = 10)

  expect_equal(as.data.frame(ev1), as.data.frame(ev2))
})

test_that("Issue #236 math in to/from", {
  expect_error(et(from = 0, to = 168 * 2 * 6, length.out = 168 * 2 * 6 + 1), NA)
})

test_that("Issue #257 numeric cmt vectorized", {
  ds4 <- c(1, 2, 3, 4)
  rate <- c(1.5, 2.5, 3.5, 4.5)
  expect_error(et() %>% et(amt = ds4, rate = rate, cmt = 4), NA)
})

test_that("etRep #313", {
  skip_if_not_installed("units")
  sch1 <- et(timeUnits = "hr") %>%
    et(amt = 100, ii = 24, until = units::set_units(2, "days"))

  toto <- rep(sch1, times = 10, wait = units::set_units(19, "days"))

  expect_equal(toto$time, seq(0, by = 504, length.out = 10))

  sch1 <- et(timeUnits = "hr") %>%
    et(amt = 100, ii = 24, until = units::set_units(2, "days")) %>%
    etExpand()

  toto1 <- etExpand(toto)

  expect_warning(toto <- rep(sch1, times = 10, wait = units::set_units(19, "days")))
})

test_that("'by' and 'length.out'", {
  expect_error(et(0, 3, by = 0.5, length.out = 3))
})

test_that("'amt' and 'time' have different values", {
  expect_error(et(amt = c(1, 2), time = c(1, 3, 4)))
})

test_that("'time' with invalid list", {
  expect_error(et(time = list(1, 2, 3)))
})

test_that("'is' for rxode2 event tables are 'rxEt'", {
  ev <- et()
  expect_true(is(ev, "rxEt"))
})

test_that("can use 'evid=0' with time entries", {
  expect_error(et(amt = 10, cmt = 1, time = 0, evid = 1, id = 1) %>%
                 et(time = c(0, 10, 20), evid = 0), NA)

})

test_that("extra doses are not added (nlmixr2/rxode2et#2)", {
  foo <- et(amt=10, id=1:2) %>% et(time=1, id=2:3)
  expect_equal(
    foo$id[!is.na(foo$amt)],
    1:2
  )
})

test_that("event table id, (rxode2et#4)", {
  expect_error(et(amt = 10, time = 0, evid = 1, id = 1:5) %>%
    et(amt = 100, time = 0, evid = 1, id = 6:10) %>%
    et(amt = 1000, time = 0, evid = 1, id = 11), NA)

})

test_that("event table non-zero time", {
  expect_warning(et(amt=1.153846, ii=24*7*6, until=24*7*6*2) %>%
    et(amt=1.153846, time=24*7*6*(2+8), ii=24*7*8, until=24*7), "until")
})


test_that("event table cmt needs to be evaluated #16", {

  dosing_df <- data.frame(
    DOSE = c(0.1, 0.5),
    CMT = c("A", "B"),
    TIME = c(0, 0)
  )

  samp_t <- c(0, 0.1, 0.5, 1, 2)

  # The below should work... but does not
  sub_df <- dosing_df[1, , drop = T]

  expect_error(et(
    amt = sub_df$DOSE,
    cmt = sub_df$CMT,
    time = sub_df$TIME,
    evid = 1,
    id = 1:5
  ) %>%
    add.sampling(time = samp_t), NA)

})

test_that("toTrialDuration works", {
  trialEnd = 2
  ev <- et(data.frame(id = rep(1:2, 3),  time = c(13, 14, 13.5, 14.5, 15.3, 16.5)))
  res <- toTrialDuration(ev, trialEnd = trialEnd, interval = 0.5)
  expect_setequal(res$time, c(13, 13.5, 14, 14.5, 15, 14, 14.5, 15, 15.5, 16))
})

test_that("Ad issue #23", {

  cmti <- "Ad"
  dose_nmol <- 3
  dosing <- et(time = 0, amt = dose_nmol, cmt = cmti)

  expect_equal(dosing$cmt, "Ad")

  dosing <- et(time = 0, amt = dose_nmol, cmt = cmtj)

  expect_equal(dosing$cmt, "cmtj")

})

test_that("test import with NA time", {

  e <- et()

  expect_warning(e$importEventTable(data.frame(
    ID = "A",
    TIME = c(738.9333333, NA),
    CMT = c("depot", "central"),
    AMT = c(300, NA),
    EVID = c(1, 0),
    DOSE = 300,
    COVAR = 1
  )))

})


test_that("another import", {

  e <- et()

  expect_warning(e$importEventTable(data.frame(ID = c("A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A131", "A131", "A131", "A131",
                                       "A131", "A131", "A131", "A131", "A132", "A132", "A132", "A132",
                                       "A132", "A132", "A132", "A132", "A132", "A132", "A132", "A132",
                                       "A132", "A132", "A132", "A132", "A132", "A132", "A132", "A132",
                                       "A132", "A132", "A132", "A132", "A132", "A132", "A132", "A132",
                                       "A132", "A132", "A132", "A132", "A132", "A132", "A132", "A132",
                                       "A132", "A132", "A132", "A132", "A133", "A133", "A133", "A133",
                                       "A133", "A133", "A133", "A133", "A133", "A133", "A133", "A133",
                                       "A133", "A133", "A133", "A133", "A133", "A133", "A133", "A133",
                                       "A133", "A133", "A133", "A133", "A133", "A133", "A133", "A133",
                                       "A133", "A133", "A133", "A133", "A133", "A133", "A133", "A133",
                                       "A133", "A133", "A133", "A134", "A134", "A134", "A134", "A134",
                                       "A134", "A134", "A134", "A134", "A134", "A134", "A135", "A135",
                                       "A135", "A135", "A135", "A135", "A135", "A135", "A135", "A135",
                                       "A135", "A135", "A135", "A135", "A135", "A135", "A135", "A135",
                                       "A135", "A135", "A135", "A135", "A135", "A135", "A135", "A135",
                                       "A135", "A135", "A135", "A135", "A135"),
                                TIME = c(0, 0, 1.3,
                                         1.883333333, 2.766666667, 4.85, 6.883333333, 7.816666667, 23.81666667,
                                         47.81666667, 71.81666667, 95.81666667, 166.2, 166.2, 190.2, 214.2,
                                         238.2, 262.2, 332.3333333, 332.3333333, 332.95, 333.5333333,
                                         334.5333333, 336.5333333, 338.5333333, 339.45, 355.45, 379.45,
                                         403.45, 427.45, 504.3166667, 504.3166667, 528.3166667, 552.3166667,
                                         576.3166667, 672.3166667, 696.3166667, 720.3166667, 744.3166667,
                                         840.3166667, 864.3166667, 888.3166667, 909.5333333, 909.5333333,
                                         910.3666667, 910.8166667, 911.7, 913.45, 915.4, 917, 1009.783333,
                                         1009.783333, 1012.3, 1034.3, 1058.3, 1082.3, 1178.3, 1202.3,
                                         1226.3, 1250.3, 2016.1, 2016.1, 2040.1, 2064.1, 2088.1, 2184.1,
                                         2208.1, 2232.1, 2256.1, 2352.1, 2376.1, 2400.1, 2424.1, 2522.1,
                                         2522.1, 2522.45, 2544.45, 2568.45, 2592.45, 2688.45, 2712.45,
                                         2736.45, 2760.45, 2856.45, 2880.45, 2904.45, 2928.45, 3025.8,
                                         3025.8, 3028.266667, 3050.266667, 3074.266667, 3098.266667, 3194.266667,
                                         3218.266667, 3242.266667, 3266.266667, 0, 0, 0.85, 24.35, 48.35,
                                         72.35, 96.35, 336.35, 360.35, 384.35, 408.35, 432.35, 500.7,
                                         500.7, 503.0166667, 525.0166667, 549.0166667, 573.0166667, 597.0166667,
                                         666.1333333, 666.1333333, 666.9833333, 667.4833333, 668.4833333,
                                         669.9833333, 672.0333333, 673.7, 689.7, 713.7, 737.7, 761.7,
                                         833.7, 881.7, 905.7, 1001.7, 1025.7, 1049.7, 1082.816667, 1082.816667,
                                         1085.7, 0, 0, 1.616666667, 2.033333333, 2.866666667, 4.866666667,
                                         6.283333333, 24.28333333, 170.2833333, 170.2833333, 194.2833333,
                                         218.2833333, 242.2833333, 261.6833333, 333.1666667, 357.1666667,
                                         381.1666667, 405.1666667, 429.1666667, 505.35, 505.35, 508.3333333,
                                         530.3333333, 554.3333333, 578.3333333, 595.4, 595.4, 596.9333333,
                                         666.9333333, 690.9333333, 714.9333333, 738.9333333, NA, NA, NA,
                                         NA, NA, NA, NA, 0, 0, 1.133333333, 1.716666667, 2.633333333,
                                         4.416666667, 6.216666667, 24.21666667, 48.21666667, 72.21666667,
                                         96.21666667, 0, 0, 0.9666666667, 1.466666667, 2.266666667, 4.35,
                                         6.016666667, 24.01666667, 48.01666667, 168.0166667, 192.0166667,
                                         216.0166667, 240.0166667, 264.0166667, 336.0166667, 360.0166667,
                                         384.0166667, 408.0166667, 432.0166667, 503.3, 503.3, 505.9, 527.9,
                                         551.9, 575.9, 599.9, 1175.566667, 1175.566667, 1178, 1200, 1224
                                         ),
                                CMT = c("central", "depot", "central", "central", "central",
                                        "central", "central", "central", "depot", "depot", "depot", "depot",
                                        "central", "depot", "depot", "depot", "depot", "depot", "central",
                                        "depot", "central", "central", "central", "central", "central",
                                        "central", "depot", "depot", "depot", "depot", "central", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "depot", "central", "depot", "central", "central",
                                        "central", "central", "central", "central", "central", "depot",
                                        "central", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "depot", "central", "depot", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "central", "depot", "central", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "central", "depot", "central", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "depot", "central", "depot", "central", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "depot", "central", "depot", "central", "depot", "depot", "depot",
                                        "depot", "central", "depot", "central", "central", "central",
                                        "central", "central", "central", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "central",
                                        "depot", "central", "central", "depot", "central", "central",
                                        "central", "central", "central", "depot", "central", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "central", "depot", "central", "depot", "depot",
                                        "depot", "central", "depot", "central", "depot", "depot", "depot",
                                        "depot", "central", "central", "central", "central", "central",
                                        "central", "central", "central", "depot", "central", "central",
                                        "central", "central", "central", "depot", "depot", "depot", "depot",
                                        "central", "depot", "central", "central", "central", "central",
                                        "central", "depot", "depot", "depot", "depot", "depot", "depot",
                                        "depot", "depot", "depot", "depot", "depot", "depot", "central",
                                        "depot", "central", "depot", "depot", "depot", "depot", "central",
                                        "depot", "central", "depot", "depot"),
                                AMT = c(NA, 400, NA, NA,
                                        NA, NA, NA, NA, 400, 400, 400, 400, NA, 400, 400, 400, 400, 400,
                                        NA, 400, NA, NA, NA, NA, NA, NA, 400, 400, 400, 400, NA, 400,
                                        400, 400, 400, 400, 400, 400, 400, 400, 400, 400, NA, 400, NA,
                                        NA, NA, NA, NA, NA, NA, 400, NA, 400, 400, 400, 400, 400, 400,
                                        400, NA, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                        400, NA, 400, NA, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                        400, 400, NA, 400, NA, 400, 400, 400, 400, 400, 400, 400, NA,
                                        500, NA, 500, 500, 500, 500, 450, 450, 450, 450, 450, NA, 450,
                                        NA, 450, 450, 450, 450, NA, 450, NA, NA, NA, NA, NA, NA, 450,
                                        450, 450, 450, 450, 450, 450, 450, 450, 450, NA, 450, NA, NA,
                                        350, NA, NA, NA, NA, NA, 350, NA, 300, 300, 300, 300, 300, 300,
                                        300, 300, 300, 300, NA, 300, NA, 300, 300, 300, NA, 300, NA,
                                        300, 300, 300, 300, NA, NA, NA, NA, NA, NA, NA, NA, 350, NA,
                                        NA, NA, NA, NA, 350, 350, 350, 350, NA, 400, NA, NA, NA, NA,
                                        NA, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                        NA, 400, NA, 400, 400, 400, 400, NA, 400, NA, 400, 400),
                                DOSE = c(0,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 0, 500, 500, 500, 500, 500, 500, 450,
                                         450, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450,
                                         450, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450,
                                         450, 450, 450, 450, 450, 450, 0, 350, 350, 350, 350, 350, 350,
                                         350, 350, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300,
                                         300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300,
                                         300, 300, 300, 300, 300, 300, 0, 350, 350, 350, 350, 350, 350,
                                         350, 350, 350, 350, 0, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400,
                                         400, 400, 400, 400, 400, 400, 400, 400, 400),
                                EVID = c(0, 1,
                                         0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0,
                                         0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
                                         0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
                                         1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                         1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,
                                         1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                                         1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1,
                                         1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0,
                                         0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0,
                                         0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1,
                                         1, 0, 1, 0, 1, 1),
                                COVAR = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
                                          1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
                                )))

})
