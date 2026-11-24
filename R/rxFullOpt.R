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

## Key folded into the parsed md5 so models compiled at different levels (or
## with different rxImpureFun() registrations) do not share a DLL; "" at "none"
## leaves the md5 unchanged
.rxFullOptKey <- function(level = .rxFullOptLevel(), impure = .rxImpureEnv$names) {
  if (level == 0L) {
    return("")
  }
  .key <- .rxFullOptLevels[level + 1L]
  ## functions registered with rxImpureFun() change what is sliced
  if (length(impure) > 0L) {
    .key <- paste0(.key, ";impure=", paste(sort(impure), collapse = ","))
  }
  .key
}

## base: parsed md5 without the level folded in, keyed by parsed md5 (with and
## without the level); key: the key folded into each parsed md5; level and
## impure: the level and rxImpureFun() names codegen uses instead of the
## current ones, when not NULL
.rxFullOptState <- new.env(parent = emptyenv())
.rxFullOptState$base <- new.env(parent = emptyenv())
.rxFullOptState$key <- new.env(parent = emptyenv())

.rxFullOptFold <- function(md5, key) {
  if (!nzchar(key)) {
    return(md5)
  }
  digest::digest(c(md5, paste0("fullOpt=", key)), serialize = TRUE, algo = "md5")
}

## Fold the level key into parsed md5 `md5`, remembering the unfolded md5
.rxFullOptMd5 <- function(md5, key) {
  .md5 <- as.character(md5)
  .ret <- .rxFullOptFold(.md5, key)
  assign(.md5, .md5, envir = .rxFullOptState$base)
  assign(as.character(.ret), .md5, envir = .rxFullOptState$base)
  assign(as.character(.ret), key, envir = .rxFullOptState$key)
  .ret
}

## Level and rxImpureFun() names whose key gave parsed md5 `md5`, as
## list(level=, impure=); NULL when unknown
.rxFullOptOf <- function(md5) {
  .key <- .rxFullOptState$key[[md5]]
  if (is.null(.key)) {
    return(NULL)
  }
  if (!nzchar(.key)) {
    return(list(level = 0L, impure = character(0)))
  }
  .parts <- strsplit(.key, ";impure=", fixed = TRUE)[[1]]
  .lvl <- match(.parts[1], .rxFullOptLevels) - 1L
  if (is.na(.lvl)) {
    return(NULL)
  }
  .imp <- if (length(.parts) > 1L) strsplit(.parts[2], ",", fixed = TRUE)[[1]] else character(0)
  list(level = .lvl, impure = .imp)
}

## Calls (as spelled in the generated C) with side effects, or that read state
## a skipped call would leave different.  A statement calling one is kept in
## every function that emitted it before.  User R functions (`_udf("name",
## ...)`) and other C functions are pure unless registered with rxImpureFun().
.rxFullOptImpureBuiltin <- c(
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
  "rxnorm1",
  "rxnormV1",
  "rxgamma1",
  "rxunif1",
  "rxlnorm1",
  "rxweibull1",
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
  .rng <- character(0)
  if (is.data.frame(.df) && nrow(.df) > 0L) {
    ## random-number generators registered by rxode2 itself
    .rng <- .df$fun[grepl("^r[xi]", .df$fun) & .df$package == "rxode2"]
  }
  list(
    level = if (is.null(.rxFullOptState$level)) {
      .rxFullOptLevel()
    } else {
      .rxFullOptState$level
    },
    impure = unique(c(
      .rxFullOptImpureBuiltin,
      .rng,
      .rxImpureFunNames(.rxFullOptState$impure)
    ))
  )
}
