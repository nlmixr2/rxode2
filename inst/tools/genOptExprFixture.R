## Generate the differential fixture for rxOptExpr().
##
## rxOptExpr() is being reimplemented in C++ (a dparser grammar plus a hashed
## common-subexpression index).  The C++ version must reproduce the current R
## output BYTE FOR BYTE, because downstream codegen consumes the text and
## nlmixr2est's saem rewrites the rx_expr_ prefix by name.
##
## The reference is rxOptExpr(x, chunkLines = 0L) -- the single whole-model
## pass.  Chunking is a workaround for the parser superlinearity this branch
## removes, and its output differs (per-chunk rx_expr_c<i>_ temporaries, less
## sharing), so it is not the contract.
##
## Run from the package root:
##   Rscript inst/tools/genOptExprFixture.R
##
## Regenerate ONLY when new syntax is added -- never to paper over a diff.

suppressMessages(library(rxode2))
options(rxprogress.disable = TRUE)

source("inst/tools/optExprFixtureCorpus.R", local = TRUE)

## These are internal, and a standalone script has to say so explicitly rather
## than reach in with `:::`.
.rxJacobian <- getFromNamespace(".rxJacobian", "rxode2")
.rxSens <- getFromNamespace(".rxSens", "rxode2")

## -------------------------------------------------------------- harvest ----
## Each pair is (model text in, optimized text out).  A model the reference
## itself cannot optimize is recorded with its error message instead, so the
## C++ path is held to raising the same thing rather than to silence.
.pairs <- list()

.add <- function(nm, txt) {
  .out <- tryCatch(
    suppressMessages(rxode2::rxOptExpr(txt, "model", chunkLines = 0L)),
    error = function(e) structure(conditionMessage(e), class = "optErr"))
  .pairs[[nm]] <<- list(input = txt,
                        output = if (inherits(.out, "optErr")) NA_character_ else .out,
                        error = if (inherits(.out, "optErr")) as.character(.out) else NA_character_)
}

for (nm in names(.optModels)) .add(nm, .optModels[[nm]])

## rxNorm()ed real models: what rxode2() itself feeds rxOptExpr.
for (nm in names(.optUiModels)) {
  .m <- try(.optUiModels[[nm]](), silent = TRUE)
  if (inherits(.m, "try-error")) { message("skip ui model ", nm); next }
  .add(paste0("ui_", nm), rxode2::rxNorm(.m))
}

## The real reason this matters: symengine-generated jacobian and sensitivity
## text is both the largest and the most redundant input rxOptExpr ever sees.
## Built the way .rxJacobian()/.rxSens() build it -- an rxS() environment --
## rather than through rxGetModel(calcSens=), which needs declared parameters.
for (nm in names(.optUiModels)) {
  .m <- try(.optUiModels[[nm]](), silent = TRUE)
  if (inherits(.m, "try-error")) next
  .mv <- try(rxode2::rxModelVars(.m), silent = TRUE)
  if (inherits(.mv, "try-error")) next
  .pars <- .mv$params
  if (length(.pars) == 0L) { message("skip sens ", nm, " (no parameters)"); next }
  .env <- try(suppressMessages(rxode2::rxS(.m)), silent = TRUE)
  if (inherits(.env, "try-error")) { message("skip sens ", nm); next }
  ## the derivative lines are not a model on their own -- they reference the
  ## states, so they are appended to the model text the way rxode2 assembles it
  .base <- rxode2::rxNorm(.m)
  .jac <- try(suppressMessages(.rxJacobian(.env)), silent = TRUE)
  if (!inherits(.jac, "try-error") && is.character(.jac) && length(.jac) > 0L) {
    .add(paste0("jac_", nm), paste(c(.base, .jac), collapse = "\n"))
  }
  .sens <- try(suppressMessages(.rxSens(.env, .pars)), silent = TRUE)
  if (!inherits(.sens, "try-error") && is.character(.sens) && length(.sens) > 0L) {
    .add(paste0("sens_", nm), paste(c(.base, .jac, .sens), collapse = "\n"))
  } else {
    message("skip sens ", nm)
  }
}

.n <- length(.pairs)
.nErr <- sum(vapply(.pairs, function(p) !is.na(p$error), logical(1)))
cat("captured ", .n, " rxOptExpr pairs (", .nErr, " of them errors)\n", sep = "")

saveRDS(.pairs, "tests/testthat/opt-expr-fixture.rds", version = 2)
cat("wrote tests/testthat/opt-expr-fixture.rds\n")
