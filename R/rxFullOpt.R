## Function-specific code generation (`options(rxode2.fullOpt=)`).
##
## "none" emits every model statement into every generated C function (the
## historical behavior); "slice" emits into each function only the statements
## its outputs depend on (src/codegenSlice.h).

.rxFullOptLevels <- c("none", "slice")

## The requested level as an integer index into .rxFullOptLevels, minus 1
.rxFullOptLevel <- function() {
  .opt <- getOption("rxode2.fullOpt", "slice")
  if (is.logical(.opt) && length(.opt) == 1L && !is.na(.opt)) {
    return(if (.opt) 1L else 0L)
  }
  if (!is.character(.opt) || length(.opt) != 1L) {
    return(0L)
  }
  .w <- match(.opt, .rxFullOptLevels)
  if (is.na(.w)) {
    return(0L)
  }
  .w - 1L
}

## Key folded into the parsed md5 so models compiled at different levels do not
## share a DLL; "" at "none" leaves the md5 unchanged
.rxFullOptKey <- function() {
  .lvl <- .rxFullOptLevel()
  if (.lvl == 0L) {
    return("")
  }
  .rxFullOptLevels[.lvl + 1L]
}

## Calls (as spelled in the generated C) with side effects, or that read state
## a skipped call would leave different.  A statement calling one is kept in
## every function that emitted it before.
.rxFullOptImpureBuiltin <- c(
  "_udf",
  "_rxDelay",
  "linCmtA",
  "linCmtB",
  "simeps",
  "simeta",
  "_getIndSim",
  "_rxord",
  "_mix",
  "_obs",
  "_rxTte",
  "_rxPushDose",
  "Rprintf",
  "REprintf",
  "Rf_error",
  "Rf_warning",
  "rxnorm",
  "rxbinom",
  "rxnbinom",
  "rxnbinomMu",
  "rxcauchy",
  "rxchisq",
  "rxexp",
  "rxf",
  "rxgeom",
  "rxgamma",
  "rxbeta",
  "rxpois",
  "rxt_",
  "rxunif",
  "rxweibull",
  "rxlnorm",
  "rinorm",
  "ribinom",
  "rinbinom",
  "rinbinomMu",
  "ricauchy",
  "richisq",
  "riexp",
  "rif",
  "rigeom",
  "rigamma",
  "ribeta",
  "ripois",
  "rit_",
  "riunif",
  "riweibull",
  "rbeta",
  "rbinom",
  "rcauchy",
  "rchisq",
  "rexp",
  "rf",
  "rgamma",
  "rgamma1",
  "rgeom",
  "rlnorm",
  "rnbinom",
  "rnorm",
  "rnorm1",
  "rnormV",
  "rnormV1",
  "rpois",
  "rt",
  "runif",
  "runif1",
  "rweibull",
  "rweibull1",
  "rinorm1",
  "rinormV1",
  "riunif1",
  "riweibull1",
  "ricauchy1",
  "rigamma1",
  "rxcauchy1"
)

## Level and impure call names, read by codegen (getRxode2FullOpt() in
## src/qs.cpp)
.rxFullOptInfo <- function() {
  .df <- rxode2parseGetTranslation()
  .ext <- character(0)
  if (is.data.frame(.df) && nrow(.df) > 0L) {
    .ext <- .df$fun[!(.df$package %in% c("rxode2", "rxode2ll"))]
    ## random-number generators registered by rxode2 itself
    .ext <- c(.ext, .df$fun[grepl("^r[xi]", .df$fun) & .df$package == "rxode2"])
  }
  .usr <- names(.udfEnv$rxSEeqUsr)
  list(
    level = .rxFullOptLevel(),
    impure = unique(c(.rxFullOptImpureBuiltin, .ext, .usr))
  )
}
