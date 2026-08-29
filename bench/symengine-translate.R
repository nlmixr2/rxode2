## Benchmark the symbolic-derivative pipeline: rxS() -> .rxJacobian() -> .rxSens().
##
## rxFromSE()/rxToSE() (the text translators) dominate this, not symengine
## itself -- symengine::D() is ~0.5% of the total.  Run against a reference
## build and a candidate build and compare.
##
##   Rscript bench/symengine-translate.R           # loads installed rxode2
##   Rscript bench/symengine-translate.R load_all  # loads the working tree
##
## Prints one row per model size; `total` is what a cold nlmixr2est model
## build pays per symengine environment.

.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) && .args[1] == "load_all") {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  .label <- "load_all (working tree)"
} else {
  suppressMessages(library(rxode2))
  .label <- paste0("installed ", utils::packageVersion("rxode2"))
}
options(rxprogress.disable = TRUE)

## These are internal, and a standalone script has to say so explicitly rather
## than reach in with `:::`.
.rxJacobian <- getFromNamespace(".rxJacobian", "rxode2")
.rxSens <- getFromNamespace(".rxSens", "rxode2")

## Linear chain of n compartments with n parameters: the cheapest way to scale
## states and parameters together and watch the O(S^2*P) sensitivity blow-up.
.chain <- function(n) {
  .l <- c(sprintf("k%d <- exp(lk%d + eta%d)", seq_len(n), seq_len(n), seq_len(n)),
          "d/dt(a1) <- -k1*a1",
          if (n > 1) sprintf("d/dt(a%d) <- k%d*a%d - k%d*a%d",
                             2:n, 1:(n - 1), 1:(n - 1), 2:n, 2:n),
          sprintf("cp <- a%d", n))
  paste(.l, collapse = "\n")
}

.time1 <- function(txt) {
  .mv <- rxode2::rxModelVars(rxode2::rxode2(txt))
  .nm <- rxode2::rxNorm(.mv)
  .pars <- grep("^lk", .mv$params, value = TRUE)
  .etas <- grep("^eta", .mv$params, value = TRUE)
  .vars <- c(.pars, .etas)
  gc(FALSE)
  .tS <- system.time(.s <- rxode2::rxS(.nm))[["elapsed"]]
  .tJ <- system.time(.rxJacobian(.s, c(.mv$state, .vars)))[["elapsed"]]
  .tSn <- system.time(.rxSens(.s, .vars))[["elapsed"]]
  c(rxS = .tS, jac = .tJ, sens = .tSn, total = .tS + .tJ + .tSn)
}

cat("== symengine translate benchmark ==\n")
cat("build: ", .label, "\n")
cat(sprintf("%4s %8s %8s %8s %9s\n", "n", "rxS", "jac", "sens", "total"))
for (n in c(3L, 6L, 10L, 15L)) {
  .r <- tryCatch(.time1(.chain(n)), error = function(e) {
    message("  n=", n, " failed: ", conditionMessage(e)); NULL
  })
  if (!is.null(.r)) {
    cat(sprintf("%4d %8.3f %8.3f %8.3f %9.3f\n",
                n, .r[["rxS"]], .r[["jac"]], .r[["sens"]], .r[["total"]]))
  }
}
