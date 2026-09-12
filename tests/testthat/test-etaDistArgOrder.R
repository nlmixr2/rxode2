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
  # shape really is the first gammapInv argument -- now by way of its role
  # anchor, since the expansion hoists each family argument onto its own
  # `rxEdA.<eta>.<role>` line and the decoder refers to that name
  expect_match(.a$emitted, "gammapInv\\(rxEdA[.]cl[.]shape", fixed = FALSE)
})

test_that("positional arguments are unchanged by normalization", {
  .pos <- "dist(cl) ~ dgamma(1/exp(lclrv), 1/(exp(lclrv)*exp(lclm)))"
  .named <- "dist(cl) ~ dgamma(shape = 1/exp(lclrv), rate = 1/(exp(lclrv)*exp(lclm)))"
  expect_identical(.adShape(rxode2(.adModel(.pos))),
                   .adShape(rxode2(.adModel(.named))))
})


# Role anchors (plan phase 3.3).  Each family argument is hoisted onto its own
# named line, `rxEdA.<eta>.<role>`, keyed by lotri's ROLE rather than the
# family's argument name -- so `scale` means the same thing across families.
# The point is that a covariate on a declaration's rate becomes a term added to
# one named line, which every downstream consumer already handles, instead of a
# substitution buried inside a quantile call.
test_that("the expansion emits a role anchor per family argument", {
  .m <- function() {
    ini({
      lclm <- 1.63; lclrv <- -2.4; prop.sd <- 0.1
      eta.cl ~ 1
      dist(eta.cl) ~ dgamma(shape = 1 / exp(lclrv),
                            rate = 1 / (exp(lclrv) * exp(lclm)))
    })
    model({
      cl <- eta.cl; v <- 5
      linCmt() ~ prop(prop.sd)
    })
  }
  .txt <- vapply(rxUiDecompress(rxEtaDistExpand(rxUiDecompress(.m())))$lstExpr,
                 function(z) paste(deparse(z), collapse = " "), character(1))
  # named by ROLE, and carrying the argument's expression.  fixed = TRUE: the
  # expressions are full of parentheses, which as a regexp would be groups.
  expect_true(any(grepl("rxEdA.eta.cl.shape <- 1/exp(lclrv)", .txt,
                        fixed = TRUE)))
  expect_true(any(grepl("rxEdA.eta.cl.rate <- 1/(exp(lclrv) * exp(lclm))",
                        .txt, fixed = TRUE)))
  # and the decoder refers to the anchors rather than repeating the expressions
  .dec <- grep("^eta[.]cl <- gammapInv", .txt, value = TRUE)
  expect_length(.dec, 1L)
  expect_match(.dec, "rxEdA[.]eta[.]cl[.]shape")
  expect_match(.dec, "rxEdA[.]eta[.]cl[.]rate")
})

test_that("both dist() spellings emit the same anchors", {
  # the invariant this file exists for: the ini({}) and model({}) spellings must
  # produce the same model, and anchors must not open a gap between them
  .ini <- function() {
    ini({
      lclm <- 1.63; lclrv <- -2.4; prop.sd <- 0.1
      eta.cl ~ 1
      dist(eta.cl) ~ dgamma(shape = 1 / exp(lclrv),
                            rate = 1 / (exp(lclrv) * exp(lclm)))
    })
    model({ cl <- eta.cl; v <- 5; linCmt() ~ prop(prop.sd) })
  }
  .mod <- function() {
    ini({ lclm <- 1.63; lclrv <- -2.4; prop.sd <- 0.1 })
    # arguments deliberately in the OTHER order
    model({
      dist(cl) ~ dgamma(rate = 1 / (exp(lclrv) * exp(lclm)),
                        shape = 1 / exp(lclrv))
      v <- 5
      linCmt() ~ prop(prop.sd)
    })
  }
  .anch <- function(.f, .eta) {
    .t <- vapply(rxUiDecompress(rxEtaDistExpand(rxUiDecompress(.f())))$lstExpr,
                 function(z) paste(deparse(z), collapse = " "), character(1))
    # strip the eta name explicitly: it can itself contain a dot ("eta.cl"),
    # so a "[^.]+" pattern would eat only part of it
    sub(paste0("^rxEdA[.]", .eta, "[.]"), "",
        grep("^rxEdA[.]", .t, value = TRUE))
  }
  # same roles, same expressions, same order -- only the eta name differs
  expect_identical(.anch(.ini, "eta[.]cl"), .anch(.mod, "cl"))
})
