rxTest({
  # `sortIds(rx, 0)` reorders the solve most-expensive-first so that a costly
  # subject is not the last thing a thread picks up.  `?setRxThreads` documents
  # the throttle as SUPPRESSING that sort when (nsubject solved)*throttle <=
  # nthreads, so the sort is taken when nall*throttle > cores.
  #
  # A refactor once flattened the suppress-branch into the sort-branch without
  # negating the comparison, which left the sort firing only when threads
  # outnumbered subjects -- the one regime the throttle exists to exclude -- so
  # on any ordinary machine the ordering was dead code.  The polarity is
  # asserted here rather than described in a comment because that is the form
  # it regressed in.
  #
  # rxode2 itself only ever calls `sortIds(rx, 1)` (identity), so the gate
  # cannot be reached through `rxSolve()`; it is tested directly.

  test_that("the throttle suppresses the sort only below its documented cutoff", {
    # cutoff: sort <=> nall*throttle > cores
    expect_false(.rxSortIdsWanted(cores = 8L, nall = 4L, throttle = 2L)) # 8 == 8
    expect_true(.rxSortIdsWanted(cores = 8L, nall = 5L, throttle = 2L))  # 10 > 8
    expect_false(.rxSortIdsWanted(cores = 8L, nall = 3L, throttle = 2L)) # 6 < 8

    # throttle = 1 moves the cutoff to one subject per thread
    expect_false(.rxSortIdsWanted(cores = 8L, nall = 8L, throttle = 1L))
    expect_true(.rxSortIdsWanted(cores = 8L, nall = 9L, throttle = 1L))
  })

  test_that("realistic population fits take the sort", {
    # the regression: 131 subjects on 4 threads at the default throttle wants
    # the sort; under the inverted gate it needed >= 262 cores to fire
    expect_true(.rxSortIdsWanted(cores = 4L, nall = 131L, throttle = 2L))
    expect_true(.rxSortIdsWanted(cores = 16L, nall = 131L, throttle = 2L))
    expect_true(.rxSortIdsWanted(cores = 2L, nall = 131L, throttle = 2L))
    # and it is NOT the "more threads than subjects" regime
    expect_false(.rxSortIdsWanted(cores = 262L, nall = 131L, throttle = 2L))
  })

  test_that("a single thread never reorders", {
    expect_false(.rxSortIdsWanted(cores = 1L, nall = 1000L, throttle = 2L))
    expect_false(.rxSortIdsWanted(cores = 0L, nall = 1000L, throttle = 2L))
    expect_false(.rxSortIdsWanted(cores = -1L, nall = 1000L, throttle = 2L))
  })

  test_that("degenerate problem sizes suppress the sort", {
    expect_false(.rxSortIdsWanted(cores = 4L, nall = 0L, throttle = 2L))
    expect_false(.rxSortIdsWanted(cores = 4L, nall = 1L, throttle = 2L))
    expect_true(.rxSortIdsWanted(cores = 4L, nall = 3L, throttle = 2L)) # 6 > 4
  })

  test_that("a large throttle does not wrap the nall*throttle product", {
    # nall*throttle is computed in 64 bits: in 32 bits these wrap to 0 and to a
    # value below `cores`, which would silently suppress a wanted sort
    expect_true(.rxSortIdsWanted(cores = 4L, nall = 2^16, throttle = 2^16))
    expect_true(.rxSortIdsWanted(cores = 4L, nall = 2^31 - 1, throttle = 2L))
    expect_true(.rxSortIdsWanted(cores = 4L, nall = 1000L, throttle = .Machine$integer.max))
  })

  test_that("the gate rejects impossible inputs rather than coercing them", {
    expect_error(.rxSortIdsWanted(cores = NA_integer_, nall = 10, throttle = 2L))
    expect_error(.rxSortIdsWanted(cores = 4L, nall = NA_real_, throttle = 2L))
    expect_error(.rxSortIdsWanted(cores = 4L, nall = 10, throttle = NA_integer_))
    expect_error(.rxSortIdsWanted(cores = 4L, nall = -1, throttle = 2L))
    expect_error(.rxSortIdsWanted(cores = 4L, nall = 2^32, throttle = 2L))
  })
})
