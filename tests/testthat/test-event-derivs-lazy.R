test_that('empty event derivative tables do not expand the symbolic model', {
  local_mocked_bindings(.rxLoadPrune = function(...) stop('unneeded symbolic expansion'), .package = 'rxode2')
  map <- list(
    states = 'central',
    sensParams = 'k',
    stateCmt = 1L,
    lagCmt = integer(),
    fCmt = integer(),
    rateCmt = integer(),
    durCmt = integer()
  )
  out <- .rxEventSensDerivs(NULL, map = map)
  expect_type(out, 'list')
  expect_true(out$lagRhsQOk)
})
