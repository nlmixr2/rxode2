## Benchmark model piping: model() / ini() on small and large models.
##
## A model() pipe rebuilds the whole ui (error processing + mu-referencing), so
## its cost tracks the full parse; ini() value edits do not rebuild.  Run
## against a reference build and a candidate build (each installed into its own
## library -- never time a load_all tree) and compare.
##
##   Rscript bench/model-piping.R           # loads installed rxode2
##   Rscript bench/model-piping.R load_all  # loads the working tree
##
## The large models come from nlmixr2lib and are skipped when it is absent.
## Prints milliseconds per operation.

.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) && .args[1] == "load_all") {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  .label <- "load_all (working tree)"
} else {
  suppressMessages(library(rxode2))
  .label <- paste0("installed ", utils::packageVersion("rxode2"))
}
options(cli.default_handler = function(...) NULL, warn = -1)

.small <- function() {
  ini({
    tka <- 0.45
    tcl <- log(2.7)
    tv <- 3.45
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d / dt(depot) <- -ka * depot
    d / dt(center) <- ka * depot - cl / v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
}

.ms <- function(expr, n) {
  .e <- substitute(expr)
  .env <- parent.frame()
  .t0 <- proc.time()[["elapsed"]]
  for (.i in seq_len(n)) {
    suppressMessages(eval(.e, .env))
  }
  (proc.time()[["elapsed"]] - .t0) / n * 1000
}

.benchOne <- function(label, fun, n) {
  .u <- suppressMessages(rxode2(fun))
  .th <- .u$iniDf$name[!is.na(.u$iniDf$ntheta)][1]
  .iniCall <- as.call(list(quote(ini), quote(.u), call("<-", as.name(.th), 0.1)))
  data.frame(
    model = label,
    lines = length(.u$lstExpr),
    iniRows = nrow(.u$iniDf),
    parse = .ms(rxode2(fun), n),
    modelAppend = .ms(model(.u, rxBenchNewVar <- 3, append = TRUE), n),
    modelCov = .ms(model(.u, rxBenchNewVar <- rxBenchTheta * rxBenchCov, append = TRUE), n),
    iniValue = .ms(eval(.iniCall), n)
  )
}

.models <- list(small = list(.small, 20L))
if (requireNamespace("nlmixr2lib", quietly = TRUE)) {
  for (.m in c(
    "Franzese_2026_pdl1_nsclc_mbma",
    "Dasti_2025_mrna1273_qsp",
    "Ippolito_2024_pacmilimab_qsp"
  )) {
    .f <- try(nlmixr2lib::readModelDb(.m), silent = TRUE)
    if (!inherits(.f, "try-error")) .models[[.m]] <- list(.f, 3L)
  }
}

cat("rxode2:", .label, "\n")
.res <- do.call(
  rbind,
  lapply(names(.models), function(.n) {
    .benchOne(.n, .models[[.n]][[1]], .models[[.n]][[2]])
  })
)
.num <- vapply(.res, is.double, logical(1))
.res[.num] <- lapply(.res[.num], round, 1)
print(.res, row.names = FALSE)
