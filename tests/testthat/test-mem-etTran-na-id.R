test_that("NA id gives clean error, not OOB segfault", {
  # etTran.cpp: the main event-processing loop used idLvl[cid-1] in error
  # messages before checking cid == NA_INTEGER.  With cid = NA_INTEGER,
  # cid - 1 overflows signed int and the CharacterVector[] access reads
  # out-of-bounds memory, causing heap corruption or a segfault.
  #
  # Fix: guard cid == NA_INTEGER at the TOP of the loop, before any
  # idLvl[cid-1] access.  The redundant later check was removed.

  m <- rxode2::rxode2(
    "d/dt(depot)  = -ka*depot
     d/dt(center) = ka*depot - cl/v*center"
  )

  # Infinite time on a row with NA id triggers the idLvl OOB path
  # (the NA id check was previously AFTER the isinf(ctime) check).
  ev_inf_time <- data.frame(
    id   = c(NA_real_, 1),
    time = c(Inf, 0),
    amt  = c(0, 100),
    evid = c(0L, 1L),
    cmt  = c(1L, 1L)
  )
  expect_error(
    rxode2::rxSolve(m, ev_inf_time, c(ka = 0.5, cl = 2, v = 10)),
    "id.*cannot.*NA|NA.*id",
    perl = TRUE
  )

  # Bad censoring value on a row with NA id triggers the OTHER idLvl OOB path.
  ev_bad_cens <- data.frame(
    id   = c(NA_real_, 1),
    time = c(0, 1),
    amt  = c(0, 0),
    evid = c(0L, 0L),
    dv   = c(1, 1),
    cens = c(5L, 0L)   # cens=5 is invalid
  )
  expect_error(
    rxode2::rxSolve(m, ev_bad_cens, c(ka = 0.5, cl = 2, v = 10)),
    "id.*cannot.*NA|NA.*id",
    perl = TRUE
  )
})
