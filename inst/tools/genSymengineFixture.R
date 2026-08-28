## Generate the differential fixture for the symengine text translators.
##
## rxFromSE()/rxToSE() are being reimplemented in C (dparser grammars).  The C
## versions must reproduce the current R output BYTE FOR BYTE.  This script
## captures (input, output) pairs for both directions into
## tests/testthat/symengine-translate-fixture.rds, which
## test-symengine-translate-fixture.R replays.
##
## Run from the package root with the CURRENT (reference) implementation loaded:
##   Rscript inst/tools/genSymengineFixture.R
##
## Regenerate ONLY when new syntax is added -- never to paper over a diff.

suppressMessages(library(rxode2))
suppressMessages(library(symengine))
options(rxprogress.disable = TRUE)

## ---------------------------------------------------------------- corpus ----
## Models chosen to cover every emitter branch: ODEs, algebraic lhs, Emax,
## transit, linCmt, the llik family, the error-model transforms, lag()/diff(),
## delay() and the special-function table.
.models <- list(
  oral1 = function() {
    ini({tka <- 0.45; tcl <- 1; tv <- 3.45; eta.ka ~ 0.6; add.sd <- 0.7})
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  },
  pkpd2 = function() {
    ini({lka <- 0.45; lcl <- 1; lvc <- 3.45; lq <- 0.5; lvp <- 3
         lemax <- 1; lec50 <- 0.5; lkin <- 0.1; lkout <- 0.1
         eta.ka ~ 0.6; eta.cl ~ 0.3; prop.sd <- 0.1; pd.sd <- 0.3})
    model({
      ka <- exp(lka + eta.ka); cl <- exp(lcl + eta.cl); vc <- exp(lvc)
      q <- exp(lq); vp <- exp(lvp)
      emax <- exp(lemax); ec50 <- exp(lec50); kin <- exp(lkin); kout <- exp(lkout)
      d/dt(depot) <- -ka * depot
      d/dt(cent) <- ka * depot - cl / vc * cent - q / vc * cent + q / vp * per1
      d/dt(per1) <- q / vc * cent - q / vp * per1
      cp <- cent / vc
      d/dt(eff) <- kin * (1 + emax * cp / (ec50 + cp)) - kout * eff
      cp ~ prop(prop.sd)
      eff ~ add(pd.sd)
    })
  },
  lognormal = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ lnorm(add.sd)
    })
  },
  logitnorm = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ logitNorm(add.sd, 0, 10)
    })
  },
  boxcox = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7; lmbd <- 0.5})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ add(add.sd) + boxCox(lmbd)
    })
  },
  transit1 = function() {
    ini({tktr <- 1; tka <- 0.45; tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7})
    model({
      ktr <- exp(tktr); ka <- exp(tka); cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(depot) <- transit(3, ktr) * depot - ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  },
  studentT = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7; nu <- 5})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ add(add.sd) + t(nu)
    })
  },
  addProp = function() {
    ini({tka <- 0.45; tcl <- 1; tv <- 3.45; eta.ka ~ 0.6; eta.cl ~ 0.3
         add.sd <- 0.7; prop.sd <- 0.1})
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd) + prop(prop.sd)
    })
  }
)

## Extra rxode2-syntax expressions covering the special-function table.  These
## go through the rxToSE direction and, once in symengine, back out again.
.exprs <- c(
  "a*b+c/d-e", "a^2", "a^3", "a^0.5", "a^(-1)", "a^b", "a^(-2)", "2^a",
  "exp(a)", "log(a)", "log(1+a)", "log1p(a)", "sqrt(a)", "abs(a)",
  "sin(a)", "cos(a)", "tan(a)", "asin(a)", "acos(a)", "atan(a)", "atan2(a,b)",
  "sinh(a)", "cosh(a)", "tanh(a)", "expit(a)", "logit(a)", "probitInv(a)",
  "gamma(a)", "lgamma(a)", "digamma(a)", "trigamma(a)", "psigamma(a,b)",
  "beta(a,b)", "lbeta(a,b)", "choose(a,b)", "lchoose(a,b)", "factorial(a)",
  "erf(a)", "erfc(a)", "pnorm(a)", "pnorm(a,b)", "pnorm(a,b,c)",
  "max(a,b)", "min(a,b)", "floor(a)", "ceiling(a)", "round(a)", "trunc(a)",
  "sign(a)", "a %% b", "log(a)/log(b)", "exp(a+b)*exp(a-b)",
  "1/a", "(-1)*a", "-(a+b)", "a*(b+c)*(d+e)", "((a))",
  "pi", "pi*2", "pi/2", "pi/4", "1/pi", "2/pi", "sqrt(pi)", "sqrt(2)",
  "log(2)", "log(10)", "1/log(2)", "1/log(10)", "sqrt(2*pi)",
  "a*pi", "exp(1)", "cos(pi*a)", "sin(pi*a)",
  "llikNorm(a,b,c)", "llikPois(a,b)", "llikBinom(a,b,c)",
  "llikBeta(a,b,c)", "llikT(a,b,c,d)",
  "rxTBS(a,b,c,d,e)", "rxTBSi(a,b,c,d,e)"
)

## Raw symengine-syntax strings pinned directly at the rxFromSE emitter: the
## branches that a model round-trip does not reliably produce.
.seRaw <- c(
  "Derivative(f(x), x)", "Derivative(f(x, y), x)", "Derivative(f(x, y), y)",
  "Subs(Derivative(f(x), x), x, y)",
  "THETA_1_ + ETA_1_", "THETA_10_*ETA_2_",
  "rx__d_dt_depot__", "rx__df_center_dy_cl__", "rx__sens_center_BY_cl__",
  "rx_f_depot_", "rx_lag_depot_", "rx_rate_depot_", "rx_dur_depot_",
  "depot_ini0", "rx_SymPy_Res_gamma",
  "E", "EulerGamma", "Catalan", "GoldenRatio",
  "2.718281828459045", "3.141592653589793", "1.4142135623730951",
  "x**2", "x**(-1)", "x**0.5", "x**(1/2)", "x**(-1/2)", "x**3",
  "1/2", "3/4", "-1/2", "0", "1", "-1",
  "abs0(x)", "rxNot(x)", "rxEq(x, y)", "rxNeq(x, y)",
  "rxAnd(x, y)", "rxOr(x, y)", "rxLt(x, y)", "rxGt(x, y)",
  "polygamma(0, x)", "polygamma(1, x)",
  "lag(x, 1)", "lag0(x, 1)", "diff(x, 1)", "delay(x, tau)",
  "linCmtA(a, b, c)", "max(x, y)", "min(x, y)",
  "erf(x)", "erfc(x)", "gamma(x)", "loggamma(x)", "zeta(x)",
  ## the constant peepholes in .rxFromSE()'s binary branch; test-dsl.R checks
  ## some of these and the C emitter silently dropped two whole lists at first
  "pi**(1/2)", "pi^(1/2)", "(pi)^(1/2)", "pi^0.5", "(pi)^0.5",
  "log(pi**(1/2))", "log(pi^(1/2))", "log(pi^0.5)",
  "2/sqrt(pi)", "1/sqrt(2*pi)", "log(2)/log(10)", "1/log(2)", "1/log(10)",
  "sqrt(3)", "sqrt(32)", "exp(1)", "log(sqrt(2*pi))", "log(sqrt(pi/2))",
  "x + y*z - w/v", "(x + y)*(z - w)", "exp(x)*log(y)/sqrt(z)"
)

## ---------------------------------------------------------------- harvest ---
.se <- character(0)   # symengine-syntax strings -> rxFromSE fixture
.rx <- character(0)   # rxode2-syntax strings    -> rxToSE  fixture

.tryAdd <- function(what, f) {
  r <- try(f(), silent = TRUE)
  if (inherits(r, "try-error") || !is.character(r) || length(r) != 1L ||
        is.na(r) || !nzchar(r)) {
    return(character(0))
  }
  r
}

## (1) rxode2 expressions -> symengine text, then differentiate w.r.t. each
## free symbol.  Both directions get exercised and the derivative output is
## exactly the shape .rxJacobian/.rxSens feed to rxFromSE.
for (e in .exprs) {
  .rx <- c(.rx, e)
  s <- .tryAdd(e, function() rxode2::rxToSE(e))
  if (!length(s)) next
  .se <- c(.se, s)
  b <- try(symengine::S(s), silent = TRUE)
  if (inherits(b, "try-error")) next
  fs <- try(as.character(symengine::free_symbols(b)), silent = TRUE)
  if (inherits(fs, "try-error")) next
  for (v in fs) {
    d <- try(as.character(symengine::D(b, symengine::S(v))), silent = TRUE)
    if (!inherits(d, "try-error")) .se <- c(.se, d)
  }
}

## (2) Real models: every normalized model line, its symengine translation, and
## the first/second derivatives w.r.t. every parameter and state.
for (nm in names(.models)) {
  ui <- try(.models[[nm]](), silent = TRUE)
  if (inherits(ui, "try-error")) { message("skip model ", nm); next }
  mv <- try(rxode2::rxModelVars(ui), silent = TRUE)
  if (inherits(mv, "try-error")) next
  vars <- c(mv$params, mv$state, mv$lhs)
  lines <- strsplit(rxode2::rxNorm(mv), "\n")[[1]]
  lines <- lines[nzchar(lines)]
  for (ln in lines) {
    rhs <- sub("^[^=]*=", "", ln)
    rhs <- sub(";$", "", rhs)
    if (!nzchar(rhs) || grepl("^[[:space:]]*$", rhs)) next
    .rx <- c(.rx, rhs)
    s <- .tryAdd(rhs, function() rxode2::rxToSE(rhs))
    if (!length(s)) next
    .se <- c(.se, s)
    b <- try(symengine::S(s), silent = TRUE)
    if (inherits(b, "try-error")) next
    fs <- try(intersect(as.character(symengine::free_symbols(b)),
                        c(vars, rxode2::rxToSE(vars))), silent = TRUE)
    if (inherits(fs, "try-error")) next
    for (v in fs) {
      d <- try(symengine::D(b, symengine::S(v)), silent = TRUE)
      if (inherits(d, "try-error")) next
      .se <- c(.se, as.character(d))
      ## second order: this is what .rxSens(order 2) emits
      for (v2 in head(fs, 3L)) {
        d2 <- try(as.character(symengine::D(d, symengine::S(v2))), silent = TRUE)
        if (!inherits(d2, "try-error")) .se <- c(.se, d2)
      }
    }
  }
}

## (3) The real hot path: run rxS() + .rxJacobian() + .rxSens() and harvest the
## generated Basics directly.  These are the exact expressions the C emitter
## will see in production (mangled rx__df_*/rx__sens_*/rx__d_dt_* symbols and
## the long sensitivity sums), which stages (1) and (2) do not reproduce.
for (nm in names(.models)) {
  ui <- try(.models[[nm]](), silent = TRUE)
  if (inherits(ui, "try-error")) next
  env <- try({
    e <- rxode2::rxS(rxode2::rxNorm(rxode2::rxModelVars(ui)))
    rxode2:::.rxJacobian(e)
    rxode2:::.rxSens(e)
    e
  }, silent = TRUE)
  if (inherits(env, "try-error")) { message("skip jac/sens ", nm); next }
  gen <- grep("^rx__(df|sens|d_dt)", ls(env), value = TRUE)
  for (g in gen) {
    r <- tryCatch(as.character(env[[g]]), error = function(e) NULL)
    if (is.character(r) && length(r) == 1L && nzchar(r)) .se <- c(.se, r)
  }
}

## (4) The nlmixr2est path: ui$symengineModelPrune expands the error model into
## rx_pred_/rx_r_/rx_lambda_ + rxTBS(), which the base rxNorm() does NOT contain.
## This is where the lnorm/logitNorm/boxCox/t transforms actually get exercised.
for (nm in names(.models)) {
  ui <- try(.models[[nm]](), silent = TRUE)
  if (inherits(ui, "try-error")) next
  pm <- try(ui$symengineModelPrune, silent = TRUE)
  if (inherits(pm, "try-error")) { message("skip prune ", nm); next }
  mv <- try(rxode2::rxModelVars(pm), silent = TRUE)
  if (inherits(mv, "try-error")) next
  lines <- strsplit(rxode2::rxNorm(mv), "\n")[[1]]
  lines <- lines[nzchar(lines)]
  for (ln in lines) {
    rhs <- sub(";$", "", sub("^[^=~]*[=~]", "", ln))
    if (!nzchar(rhs)) next
    .rx <- c(.rx, rhs)
    r <- .tryAdd(rhs, function() rxode2::rxToSE(rhs))
    if (length(r)) .se <- c(.se, r)
  }
  env <- try({
    e <- rxode2::rxS(pm)
    rxode2:::.rxJacobian(e)
    rxode2:::.rxSens(e)
    e
  }, silent = TRUE)
  if (inherits(env, "try-error")) { message("skip prune jac/sens ", nm); next }
  gen <- grep("^rx_", ls(env), value = TRUE)
  for (g in gen) {
    r <- tryCatch(as.character(env[[g]]), error = function(e) NULL)
    if (is.character(r) && length(r) == 1L && nzchar(r)) .se <- c(.se, r)
  }
}

.se <- c(.se, .seRaw)
.se <- unique(.se[nzchar(.se) & !is.na(.se)])
.rx <- unique(.rx[nzchar(.rx) & !is.na(.rx)])

## ------------------------------------------------------- expected output ----
## Capture in FRESH subprocesses, never in this one.  Stages (1)-(4) above build
## symengine environments, and that registers function names which change how
## rxFromSE() treats a later unknown function (eg "zeta(x)" errors in a clean
## session but succeeds once a model has been loaded).  Recording from this
## process would bake that contamination into the oracle.
##
## Capture twice -- once under library(), once under pkgload::load_all() -- and
## keep only the rows where they agree, so the fixture is valid both under
## devtools::test() and under R CMD check.  The four passes run in the SAME
## order the test file uses, because within one session an earlier pass can
## register a name that a later pass then sees.

.inputs <- list(se = .se, rx = .rx)
.inFile <- tempfile(fileext = ".rds")
saveRDS(.inputs, .inFile, version = 2)

.captureScript <- function(loader) {
  sprintf('
    %s
    .in <- readRDS("%s")
    .capture <- function(inputs, fn) {
      out <- character(length(inputs)); err <- logical(length(inputs))
      for (i in seq_along(inputs)) {
        r <- tryCatch(fn(inputs[i]),
                      error = function(e) structure(conditionMessage(e), class = "sgErr"))
        if (inherits(r, "sgErr")) { err[i] <- TRUE; out[i] <- as.character(r) }
        else if (is.character(r) && length(r) == 1L) out[i] <- r
        else { err[i] <- TRUE; out[i] <- "<non-character>" }
      }
      data.frame(input = inputs, output = out, isError = err, stringsAsFactors = FALSE)
    }
    res <- list(
      fromSE        = .capture(.in$se, function(x) rxode2::rxFromSE(x)),
      fromSEforward = .capture(.in$se, function(x) rxode2::rxFromSE(x, "forward")),
      fromSEcentral = .capture(.in$se, function(x) rxode2::rxFromSE(x, "central")),
      toSE          = .capture(.in$rx, function(x) rxode2::rxToSE(x)))
    saveRDS(res, "%%s", version = 2)
  ', loader, .inFile)
}

.runCapture <- function(loader, label) {
  outFile <- tempfile(fileext = ".rds")
  scr <- sprintf(.captureScript(loader), outFile)
  f <- tempfile(fileext = ".R")
  writeLines(scr, f)
  st <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)),
                stdout = FALSE, stderr = FALSE)
  if (!file.exists(outFile)) {
    stop("capture subprocess failed (", label, "), status ", st)
  }
  readRDS(outFile)
}

message("capturing under library(rxode2) ...")
.a <- .runCapture('suppressMessages(library(rxode2))', "library")
message("capturing under pkgload::load_all() ...")
.b <- .runCapture('suppressMessages(pkgload::load_all(".", quiet = TRUE))', "load_all")

## Keep only rows both loaders agree on.
.reconcile <- function(a, b, what) {
  stopifnot(identical(a$input, b$input))
  keep <- a$output == b$output & a$isError == b$isError
  if (any(!keep)) {
    message(sprintf("  %s: dropped %d of %d loader-dependent row(s): %s",
                    what, sum(!keep), nrow(a),
                    paste(utils::head(a$input[!keep], 5L), collapse = ", ")))
  }
  a[keep, , drop = FALSE]
}

fromSE    <- .reconcile(.a$fromSE,        .b$fromSE,        "fromSE")
fromSEfwd <- .reconcile(.a$fromSEforward, .b$fromSEforward, "fromSEforward")
fromSEcen <- .reconcile(.a$fromSEcentral, .b$fromSEcentral, "fromSEcentral")
toSE      <- .reconcile(.a$toSE,          .b$toSE,          "toSE")

fixture <- list(
  fromSE = fromSE, fromSEforward = fromSEfwd, fromSEcentral = fromSEcen,
  toSE = toSE,
  generatedBy = utils::packageVersion("rxode2"),
  symengineVersion = utils::packageVersion("symengine")
)

dir.create("tests/testthat", showWarnings = FALSE, recursive = TRUE)
saveRDS(fixture, "tests/testthat/symengine-translate-fixture.rds", version = 2)

cat("fromSE  pairs:", nrow(fromSE), " (", sum(fromSE$isError), "errors )\n")
cat("toSE    pairs:", nrow(toSE),   " (", sum(toSE$isError),   "errors )\n")
cat("written: tests/testthat/symengine-translate-fixture.rds\n")
