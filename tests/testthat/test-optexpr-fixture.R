## Differential fixture for rxOptExpr().
##
## The C++ common-subexpression pass must reproduce the R reference BYTE FOR
## BYTE: downstream codegen consumes this text, and nlmixr2est's saem rewrites
## the rx_expr_ prefix by name.  The reference is the single whole-model pass
## (chunkLines = 0L); see inst/tools/genOptExprFixture.R for why.
##
## Regenerate with `Rscript inst/tools/genOptExprFixture.R` ONLY when new
## syntax is added -- never to make a diff go away.
rxTest({
  .f <- test_path("opt-expr-fixture.rds")
  skip_if_not(file.exists(.f), "opt-expr fixture not generated")
  .pairs <- readRDS(.f)

  test_that("rxOptExpr reproduces the fixture byte for byte", {
    for (.nm in names(.pairs)) {
      .p <- .pairs[[.nm]]
      if (is.na(.p$error)) {
        .got <- suppressMessages(rxOptExpr(.p$input, "model", chunkLines = 0L))
        expect_identical(.got, .p$output, info = .nm)
      } else {
        ## a model the reference cannot optimize must still fail, and with the
        ## same message -- an approximation here would be worse than an error
        expect_error(suppressMessages(rxOptExpr(.p$input, "model", chunkLines = 0L)),
                     fixed = TRUE, regexp = .p$error)
      }
    }
  })
})
