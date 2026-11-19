rxTest({
  # rxode2 <-> rxode2lincmt wiring (src/rxode2lincmtLink.cpp).  Nothing is
  # checked at load, so the invariants live here.

  test_that("every lincmt table slot is bound after load", {
    .linked <- .Call(`_rxode2_rxode2lincmtLinked`)
    expect_length(.linked, 9L)
    expect_true(all(.linked))
  })

  test_that("rxode2lincmt holds exactly the offsets rxode2 builds", {
    .host <- .Call(`_rxode2_rxode2lincmtHost`)
    .info <- rxode2lincmt::.rxode2lincmtHostInfo()
    expect_identical(.info$offsets, .host[[1]])
    expect_true(all(.info$fns))
    expect_length(.host, 1L + length(.info$fns))
  })

  test_that("offsets are consistent with the struct size", {
    .off <- .Call(`_rxode2_rxode2lincmtHost`)[[1]]
    .names <- rxode2lincmt::.rxode2lincmtHostFieldNames()
    expect_length(.off, 3L + length(.names))
    .indSize <- .off[1]
    .ind <- .off[-(1:3)][startsWith(.names, "ind_")]
    expect_true(all(.ind >= 0L & .ind < .indSize))
    expect_identical(.off[2:3], c(8L, 4L))
  })

  test_that("the rxode2lincmt re-link hook is installed once and relinks", {
    .event <- packageEvent("rxode2lincmt", "onLoad")
    .rxode2lincmtRelinkHook()
    .rxode2lincmtRelinkHook()
    .tagged <- Filter(function(h) isTRUE(attr(h, "rxode2Relink")), getHook(.event))
    expect_length(.tagged, 1L)
    .tagged[[1]]()
    expect_true(all(.Call(`_rxode2_rxode2lincmtLinked`)))
  })

  test_that("re-linking is idempotent", {
    .linkAll()
    expect_true(all(.Call(`_rxode2_rxode2lincmtLinked`)))
    expect_identical(rxode2lincmt::.rxode2lincmtHostInfo()$offsets, .Call(`_rxode2_rxode2lincmtHost`)[[1]])
  })
})
