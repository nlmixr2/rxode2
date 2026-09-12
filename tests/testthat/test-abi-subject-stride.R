test_that("the ABI subject accessor walks the array at the allocated stride", {
  # getSolvingOptionsInd() (src/rx2api.c) is what downstream packages resolve
  # through the function-pointer table, and it is a different translation unit
  # from the one that allocates rx->subjects.  A build where the two disagree
  # on sizeof(rx_solving_options_ind) reads every subject but the first from
  # the wrong address (nlmixr2/nlmixr2est#1039) -- so read each subject's
  # record count both ways and require them to agree, and to agree with the
  # data.  More than one subject is essential: subject 0 sits at the start of
  # the array and reads correctly under any stride.
  m <- rxode2({
    ka <- 1
    cl <- 3
    v <- 30
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center
    cp <- center / v
  })
  # unequal per-subject record counts, so a wrong stride cannot pass by landing
  # on a neighbouring subject that happens to hold the same count
  nObsI <- c(3, 5, 2, 7, 4, 6)
  d <- do.call(rbind, lapply(seq_along(nObsI), function(id) {
    rbind(data.frame(id = id, time = 0, amt = 320, evid = 1, cmt = 1),
          data.frame(id = id, time = seq(0.5, 12, length.out = nObsI[id]),
                     amt = 0, evid = 0, cmt = 1))
  }))
  suppressMessages(rxSolve(m, d))

  cnt <- rxTestAbiSubjectCounts_()
  # the stride the ABI walks with is the one the allocator published, and this
  # is the translation unit that owns the struct
  expect_equal(cnt$abiStride, cnt$localStride)
  expect_equal(length(cnt$nAllTimes), length(nObsI))
  expect_equal(as.integer(cnt$nAllTimes), as.integer(nObsI) + 1L)
})

test_that("the ABI accessor follows the published stride, not its own sizeof", {
  # the test above only compares two views a clean build makes equal.  This one
  # builds the disagreement on purpose: a private subject array whose entries
  # are `pad` bytes further apart than this build's sizeof, with rxIndSize set
  # to match.  An accessor walking with its own sizeof reads the wrong entries
  # for any pad > 0.
  for (pad in c(0L, 8L, 688L)) {
    expect_equal(rxTestAbiStrideProbe_(pad, 6L), 1000L + 0:5,
                 info = paste("pad =", pad))
  }
  # nothing to walk
  expect_equal(length(rxTestAbiStrideProbe_(0L, 0L)), 0L)
})
