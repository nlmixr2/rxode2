test_that('empty event derivative tables do not expand the symbolic model', {
  local_mocked_bindings(.rxLoadPrune = function(...) stop('unneeded symbolic expansion'), .package = 'rxode2')
  map <- list(
    states = character(),
    sensParams = character(),
    stateCmt = integer(),
    lagCmt = integer(),
    fCmt = integer(),
    rateCmt = integer(),
    durCmt = integer()
  )
  out <- .rxEventSensDerivs(NULL, map = map)
  expect_type(out, 'list')
})
