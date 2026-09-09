# `dist()` arguments are substituted POSITIONALLY by every consumer -- the
# inverse-CDF line rxUdfUiLhs.dist emits, nlmixr2est's .etaDistMstepCore(), its
# C++ RPN parser, the warm start, babelmixr2's native path.  The ini({}) form has
# always stored lotri's canonical order; the model({}) form used to store the
# call as the user wrote it.
#
# So `dgamma(rate = r, shape = s)` in model({}) fitted a DIFFERENT DISTRIBUTION
# than written, with no error anywhere:
#
#   as written  cl <- gammapInv((1/exp(lclrv)), ...)/((1/(exp(lclrv)*exp(lclm))))
#   swapped     cl <- gammapInv((1/(exp(lclrv)*exp(lclm))), ...)/((1/exp(lclrv)))
#
# Normalizing at the storage point fixes every consumer at once, so these tests
# pin the storage and the emitted line together.

.adModel <- function(decl) {
  eval(parse(text = paste0("function() {
    ini({ lclm <- 1.5; lclrv <- 0.7; tv <- 3.45; add.sd <- 0.7; cl ~ 1 })
    model({ ", decl, "
            v <- exp(tv)
            linCmt() ~ add(add.sd) })
  }")))
}

.adIni <- function(decl) {
  eval(parse(text = paste0("function() {
    ini({ lclm <- 1.5; lclrv <- 0.7; tv <- 3.45; add.sd <- 0.7; cl ~ 1
          ", decl, " })
    model({ v <- exp(tv)
            linCmt() ~ add(add.sd) })
  }")))
}

.adShape <- function(ui) {
  list(stored = rxUiEtaDists(ui)$etaDist,
       emitted = grep("^cl <-",
                      vapply(rxEtaDistExpand(ui)$lstExpr,
                             function(z) paste(deparse(z), collapse = " "),
                             character(1)),
                      value = TRUE))
}

test_that("dist() argument ORDER does not change the model", {
  .canon <- "dist(cl) ~ dgamma(shape = 1/exp(lclrv), rate = 1/(exp(lclrv)*exp(lclm)))"
  .swap  <- "dist(cl) ~ dgamma(rate = 1/(exp(lclrv)*exp(lclm)), shape = 1/exp(lclrv))"

  .a <- .adShape(rxode2(.adModel(.canon)))
  .b <- .adShape(rxode2(.adModel(.swap)))
  expect_identical(.a, .b)

  # and the two BLOCKS agree, which is what makes ini({}) sugar
  .c <- .adShape(rxode2(.adIni(.swap)))
  expect_identical(.b, .c)

  # the stored text is lotri's canonical positional form, not the user's
  expect_identical(.a$stored,
                   "dgamma(1/exp(lclrv), 1/(exp(lclrv) * exp(lclm)))")
  # shape really is the first gammapInv argument
  expect_match(.a$emitted, "gammapInv\\(\\(1/exp\\(lclrv\\)\\)", fixed = FALSE)
})

test_that("positional arguments are unchanged by normalization", {
  .pos <- "dist(cl) ~ dgamma(1/exp(lclrv), 1/(exp(lclrv)*exp(lclm)))"
  .named <- "dist(cl) ~ dgamma(shape = 1/exp(lclrv), rate = 1/(exp(lclrv)*exp(lclm)))"
  expect_identical(.adShape(rxode2(.adModel(.pos))),
                   .adShape(rxode2(.adModel(.named))))
})
