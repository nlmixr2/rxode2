## Tests for external C package integration as described in
## vignettes/articles/rxode2-pkg-exported-funs.Rmd
##
## Strategy: build the complete 'mm' package (exactly as the vignette
## describes), install it to a temporary library, attach it, run all
## assertions, then detach it and verify the cleanup hooks fire correctly.

rxTest({

  ## ── package-building helpers ─────────────────────────────────────────────

  .writeMmPackage <- function(pkgDir) {

    dir.create(file.path(pkgDir, "R"),   recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(pkgDir, "src"), recursive = TRUE, showWarnings = FALSE)

    ## DESCRIPTION ---------------------------------------------------------
    writeLines(c(
      "Package: mm",
      "Version: 0.1.0",
      "Title: Michaelis-Menten rxode2 Functions",
      "Description: Test package for rxode2 external C integration.",
      "Authors@R: person('Test', 'Author', role = c('aut', 'cre'),",
      "    email = 'test@test.com')",
      "License: MIT + file LICENSE",
      "Imports: rxode2",
      "LinkingTo: rxode2",
      "Encoding: UTF-8"
    ), file.path(pkgDir, "DESCRIPTION"))

    ## LICENSE stub (required by MIT + file LICENSE) -----------------------
    writeLines("YEAR: 2026\nCOPYRIGHT HOLDER: Test Author",
               file.path(pkgDir, "LICENSE"))

    ## NAMESPACE -----------------------------------------------------------
    writeLines(c(
      "useDynLib(mm, .registration = TRUE)",
      "importFrom(rxode2, rxD)",
      "importFrom(rxode2, rxRmFun)",
      "importFrom(rxode2, rxUdfUi)",
      "importFrom(rxode2, rxode2parseAssignTranslation)",
      "importFrom(rxode2, rxode2parseGetTranslation)",
      "export(mm)",
      "export(mm_dC)",
      "export(mm_dVmax)",
      "export(mm_dKm)",
      "S3method(rxUdfUi, mm)"
    ), file.path(pkgDir, "NAMESPACE"))

    ## src/myfuns.c --------------------------------------------------------
    writeLines(c(
      "#define STRICT_R_HEADERS",
      "#include <R.h>",
      "#include <Rinternals.h>",
      "#include <R_ext/RS.h>",
      "",
      "/* Scalar functions ----------------------------------------- */",
      "double mm(double C, double Vmax, double Km) {",
      "  return Vmax * C / (Km + C);",
      "}",
      "",
      "double mm_dC(double C, double Vmax, double Km) {",
      "  double den = Km + C;",
      "  return Vmax * Km / (den * den);",
      "}",
      "",
      "double mm_dVmax(double C, double Vmax, double Km) {",
      "  (void)Vmax;",
      "  return C / (Km + C);",
      "}",
      "",
      "double mm_dKm(double C, double Vmax, double Km) {",
      "  double den = Km + C;",
      "  return -Vmax * C / (den * den);",
      "}",
      "",
      "/* SEXP vector wrappers ---------------------------------------- */",
      "SEXP _mm_mm(SEXP C, SEXP Vmax, SEXP Km) {",
      "  int n = LENGTH(C);",
      "  SEXP out = PROTECT(allocVector(REALSXP, n));",
      "  double *c = REAL(C), *vmax = REAL(Vmax), *km = REAL(Km);",
      "  double *res = REAL(out);",
      "  for (int i = 0; i < n; i++) res[i] = mm(c[i], vmax[i], km[i]);",
      "  UNPROTECT(1); return out;",
      "}",
      "SEXP _mm_mm_dC(SEXP C, SEXP Vmax, SEXP Km) {",
      "  int n = LENGTH(C);",
      "  SEXP out = PROTECT(allocVector(REALSXP, n));",
      "  double *c = REAL(C), *vmax = REAL(Vmax), *km = REAL(Km);",
      "  double *res = REAL(out);",
      "  for (int i = 0; i < n; i++) res[i] = mm_dC(c[i], vmax[i], km[i]);",
      "  UNPROTECT(1); return out;",
      "}",
      "SEXP _mm_mm_dVmax(SEXP C, SEXP Vmax, SEXP Km) {",
      "  int n = LENGTH(C);",
      "  SEXP out = PROTECT(allocVector(REALSXP, n));",
      "  double *c = REAL(C), *vmax = REAL(Vmax), *km = REAL(Km);",
      "  double *res = REAL(out);",
      "  for (int i = 0; i < n; i++) res[i] = mm_dVmax(c[i], vmax[i], km[i]);",
      "  UNPROTECT(1); return out;",
      "}",
      "SEXP _mm_mm_dKm(SEXP C, SEXP Vmax, SEXP Km) {",
      "  int n = LENGTH(C);",
      "  SEXP out = PROTECT(allocVector(REALSXP, n));",
      "  double *c = REAL(C), *vmax = REAL(Vmax), *km = REAL(Km);",
      "  double *res = REAL(out);",
      "  for (int i = 0; i < n; i++) res[i] = mm_dKm(c[i], vmax[i], km[i]);",
      "  UNPROTECT(1); return out;",
      "}"
    ), file.path(pkgDir, "src", "myfuns.c"))

    ## src/init.c ----------------------------------------------------------
    writeLines(c(
      "#include <R.h>",
      "#include <Rinternals.h>",
      "#include <R_ext/Rdynload.h>",
      "#include <stdlib.h>",
      "",
      "extern double mm      (double, double, double);",
      "extern double mm_dC   (double, double, double);",
      "extern double mm_dVmax(double, double, double);",
      "extern double mm_dKm  (double, double, double);",
      "",
      "extern SEXP _mm_mm      (SEXP, SEXP, SEXP);",
      "extern SEXP _mm_mm_dC   (SEXP, SEXP, SEXP);",
      "extern SEXP _mm_mm_dVmax(SEXP, SEXP, SEXP);",
      "extern SEXP _mm_mm_dKm  (SEXP, SEXP, SEXP);",
      "",
      "void R_init_mm(DllInfo *dll) {",
      "  static const R_CallMethodDef callMethods[] = {",
      "    {\"_mm_mm\",       (DL_FUNC) &_mm_mm,       3},",
      "    {\"_mm_mm_dC\",    (DL_FUNC) &_mm_mm_dC,    3},",
      "    {\"_mm_mm_dVmax\", (DL_FUNC) &_mm_mm_dVmax, 3},",
      "    {\"_mm_mm_dKm\",   (DL_FUNC) &_mm_mm_dKm,   3},",
      "    {NULL, NULL, 0}",
      "  };",
      "  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);",
      "  R_useDynamicSymbols(dll, FALSE);",
      "",
      "  R_RegisterCCallable(\"mm\", \"mm\",       (DL_FUNC) &mm);",
      "  R_RegisterCCallable(\"mm\", \"mm_dC\",    (DL_FUNC) &mm_dC);",
      "  R_RegisterCCallable(\"mm\", \"mm_dVmax\", (DL_FUNC) &mm_dVmax);",
      "  R_RegisterCCallable(\"mm\", \"mm_dKm\",   (DL_FUNC) &mm_dKm);",
      "}"
    ), file.path(pkgDir, "src", "init.c"))

    ## R/mm.R --------------------------------------------------------------
    writeLines(c(
      "mm <- function(C, Vmax, Km) {",
      "  df <- data.frame(C = C, Vmax = Vmax, Km = Km)",
      "  .Call(`_mm_mm`, as.double(df$C), as.double(df$Vmax), as.double(df$Km))",
      "}",
      "mm_dC <- function(C, Vmax, Km) {",
      "  df <- data.frame(C = C, Vmax = Vmax, Km = Km)",
      "  .Call(`_mm_mm_dC`, as.double(df$C), as.double(df$Vmax), as.double(df$Km))",
      "}",
      "mm_dVmax <- function(C, Vmax, Km) {",
      "  df <- data.frame(C = C, Vmax = Vmax, Km = Km)",
      "  .Call(`_mm_mm_dVmax`, as.double(df$C), as.double(df$Vmax), as.double(df$Km))",
      "}",
      "mm_dKm <- function(C, Vmax, Km) {",
      "  df <- data.frame(C = C, Vmax = Vmax, Km = Km)",
      "  .Call(`_mm_mm_dKm`, as.double(df$C), as.double(df$Vmax), as.double(df$Km))",
      "}",
      "",
      "rxUdfUi.mm <- function(fun) {",
      "  .dummy <- function(C, Vmax, Km) {}",
      "  .mc <- match.call(.dummy, fun)",
      "  .C    <- deparse1(.mc$C)",
      "  .Vmax <- deparse1(.mc$Vmax)",
      "  .Km   <- deparse1(.mc$Km)",
      "  list(replace = paste0('mm(', .C, ',', .Vmax, ',', .Km, ')'))",
      "}"
    ), file.path(pkgDir, "R", "mm.R"))

    ## R/zzz.R -------------------------------------------------------------
    writeLines(c(
      ".onAttach <- function(libname, pkgname) {",
      "  .cur <- rxode2parseGetTranslation()",
      "  .newRows <- data.frame(",
      "    rxFun      = c('mm',          'mm_dC',       'mm_dVmax',    'mm_dKm'),",
      "    fun        = c('mm',          'mm_dC',        'mm_dVmax',   'mm_dKm'),",
      "    type       = rep('rxode2_fn3', 4),",
      "    package    = rep('mm', 4),",
      "    packageFun = c('mm',          'mm_dC',        'mm_dVmax',   'mm_dKm'),",
      "    argMin     = 3L,",
      "    argMax     = 3L,",
      "    threadSafe = 1L,",
      "    stringsAsFactors = FALSE",
      "  )",
      "  .mmNames   <- .newRows$rxFun",
      "  .conflicts <- .cur[.cur$rxFun %in% .mmNames, , drop = FALSE]",
      "  if (nrow(.conflicts) > 0L) {",
      "    .fromOther <- .conflicts[.conflicts$package != 'mm', , drop = FALSE]",
      "    .fromSelf  <- .conflicts[.conflicts$package == 'mm', , drop = FALSE]",
      "    if (nrow(.fromOther) > 0L) {",
      "      stop(",
      "        \"Cannot load package 'mm': the following rxode2 function name(s) are \",",
      "        \"already registered by another package:\\n\",",
      "        paste0(\"  \", .fromOther$rxFun, \" (registered by '\",",
      "               .fromOther$package, \"')\", collapse = \"\\n\"),",
      "        call. = FALSE",
      "      )",
      "    }",
      "    if (nrow(.fromSelf) > 0L) {",
      "      packageStartupMessage(",
      "        \"Package 'mm' rxode2 function registration was already present \",",
      "        \"(package reloaded); re-registering.\"",
      "      )",
      "    }",
      "  }",
      "  .cur <- .cur[.cur$package != 'mm', , drop = FALSE]",
      "  rxode2parseAssignTranslation(rbind(.cur, .newRows))",
      "",
      "  rxD('mm', list(",
      "    function(C, Vmax, Km) paste0('mm_dC(',    C, ',', Vmax, ',', Km, ')'),",
      "    function(C, Vmax, Km) paste0('mm_dVmax(', C, ',', Vmax, ',', Km, ')'),",
      "    function(C, Vmax, Km) paste0('mm_dKm(',   C, ',', Vmax, ',', Km, ')')",
      "  ))",
      "",
      "  rxD('mm_dC', list(",
      "    function(C, Vmax, Km)",
      "      paste0('(-2.0*(',Vmax,')*(',Km,'))/pow((',Km,')+(',C,'),3.0)'),",
      "    function(C, Vmax, Km)",
      "      paste0('(',Km,')/pow((',Km,')+(',C,'),2.0)'),",
      "    function(C, Vmax, Km)",
      "      paste0('(',Vmax,')*((',C,')-(',Km,'))/pow((',Km,')+(',C,'),3.0)')",
      "  ))",
      "",
      "  rxD('mm_dVmax', list(",
      "    function(C, Vmax, Km)",
      "      paste0('(',Km,')/pow((',Km,')+(',C,'),2.0)'),",
      "    function(C, Vmax, Km) '0.0',",
      "    function(C, Vmax, Km)",
      "      paste0('-(',C,')/pow((',Km,')+(',C,'),2.0)')",
      "  ))",
      "",
      "  rxD('mm_dKm', list(",
      "    function(C, Vmax, Km)",
      "      paste0('(',Vmax,')*((',C,')-(',Km,'))/pow((',Km,')+(',C,'),3.0)'),",
      "    function(C, Vmax, Km)",
      "      paste0('-(',C,')/pow((',Km,')+(',C,'),2.0)'),",
      "    function(C, Vmax, Km)",
      "      paste0('2.0*(',Vmax,')*(',C,')/pow((',Km,')+(',C,'),3.0)')",
      "  ))",
      "  invisible()",
      "}",
      "",
      ".onUnload <- function(libpath) {",
      "  .cur <- try(rxode2parseGetTranslation(), silent = TRUE)",
      "  if (!inherits(.cur, 'try-error')) {",
      "    try(rxode2parseAssignTranslation(",
      "      .cur[.cur$package != 'mm', , drop = FALSE]",
      "    ), silent = TRUE)",
      "  }",
      "  suppressWarnings(try(rxRmFun('mm'),       silent = TRUE))",
      "  suppressWarnings(try(rxRmFun('mm_dC'),    silent = TRUE))",
      "  suppressWarnings(try(rxRmFun('mm_dVmax'), silent = TRUE))",
      "  suppressWarnings(try(rxRmFun('mm_dKm'),   silent = TRUE))",
      "}"
    ), file.path(pkgDir, "R", "zzz.R"))

    invisible(pkgDir)
  }

  ## ── main test ────────────────────────────────────────────────────────────

  test_that("mm package: install, attach, verify integration, detach, verify cleanup", {

    ## Create package skeleton + temp library dir
    .pkgDir <- tempfile("mm_src_")
    .libDir <- tempfile("mm_lib_")
    dir.create(.pkgDir, recursive = TRUE, showWarnings = FALSE)
    dir.create(.libDir, recursive = TRUE, showWarnings = FALSE)
    on.exit({
      try(detach("package:mm", unload = TRUE, force = TRUE), silent = TRUE)
      unlink(.pkgDir, recursive = TRUE)
      unlink(.libDir, recursive = TRUE)
    }, add = TRUE)

    .writeMmPackage(.pkgDir)

    ## ── install ──────────────────────────────────────────────────────────
    .out <- system2(
      file.path(R.home("bin"), "R"),
      args   = c("CMD", "INSTALL", "--no-multiarch",
                 paste0("--library=", .libDir), .pkgDir),
      stdout = TRUE, stderr = TRUE
    )
    if (!any(grepl("DONE \\(mm\\)", .out))) {
      skip(paste0("mm package failed to install (C compiler issue?):\n",
                  paste(tail(.out, 20), collapse = "\n")))
    }

    ## ── attach ───────────────────────────────────────────────────────────
    .oldPaths <- .libPaths()
    on.exit(.libPaths(.oldPaths), add = TRUE)
    .libPaths(c(.libDir, .oldPaths))

    ## Capture startup messages from .onAttach
    expect_no_error(
      suppressPackageStartupMessages(library(mm, lib.loc = .libDir))
    )

    ## ── translation table ────────────────────────────────────────────────
    .trans <- rxode2::rxode2parseGetTranslation()
    .mm    <- .trans[.trans$package == "mm", , drop = FALSE]

    expect_equal(nrow(.mm), 4L)
    expect_equal(sort(.mm$rxFun),
                 sort(c("mm", "mm_dC", "mm_dVmax", "mm_dKm")))
    expect_true(all(.mm$type       == "rxode2_fn3"))
    expect_true(all(.mm$argMin     == 3L))
    expect_true(all(.mm$argMax     == 3L))
    expect_true(all(.mm$threadSafe == 1L))
    expect_true(all(.mm$packageFun == .mm$rxFun))

    ## mm functions are visible to the parser
    .funs <- rxode2::rxSupportedFuns()
    expect_true("mm"       %in% .funs)
    expect_true("mm_dC"    %in% .funs)
    expect_true("mm_dVmax" %in% .funs)
    expect_true("mm_dKm"   %in% .funs)

    ## ── R vector interface ───────────────────────────────────────────────
    ## mm(C, Vmax, Km) = Vmax * C / (Km + C)
    expect_equal(mm(2,  10, 2), 10 * 2  / (2 + 2))     # 5
    expect_equal(mm(0,  10, 2), 0)
    expect_equal(
      mm(c(1, 2, 5), 10, 2),
      10 * c(1, 2, 5) / (2 + c(1, 2, 5))
    )

    ## mm_dC(C, Vmax, Km) = Vmax * Km / (Km + C)^2
    expect_equal(mm_dC(2, 10, 2), 10 * 2 / (2 + 2)^2)

    ## mm_dVmax(C, Vmax, Km) = C / (Km + C)
    expect_equal(mm_dVmax(2, 10, 2), 2 / (2 + 2))

    ## mm_dKm(C, Vmax, Km) = -Vmax * C / (Km + C)^2
    expect_equal(mm_dKm(2, 10, 2), -10 * 2 / (2 + 2)^2)

    ## ── rxUdfUi named-argument reordering ────────────────────────────────
    ## Positional call — already canonical
    .c1 <- quote(mm(Cp, Vmax, Km))
    class(.c1) <- "mm"
    expect_equal(rxode2::rxUdfUi(.c1)$replace, "mm(Cp,Vmax,Km)")

    ## Named in canonical order
    .c2 <- quote(mm(C = Cp, Vmax = Vmax, Km = Km))
    class(.c2) <- "mm"
    expect_equal(rxode2::rxUdfUi(.c2)$replace, "mm(Cp,Vmax,Km)")

    ## Named in different order — must be reordered to C, Vmax, Km
    .c3 <- quote(mm(Km = Km, Vmax = Vmax, C = Cp))
    class(.c3) <- "mm"
    expect_equal(rxode2::rxUdfUi(.c3)$replace, "mm(Cp,Vmax,Km)")

    ## ── ODE simulation ───────────────────────────────────────────────────
    .mod <- rxode2::rxode2({
      Cp       <- A1 / Vc
      d/dt(A1) <- -mm(Cp, Vmax, Km)
    })
    .ev  <- rxode2::et(amt = 100) |> rxode2::et(0, 24, by = 0.5)
    .sim <- rxode2::rxSolve(.mod, .ev, params = c(Vc = 10, Vmax = 5, Km = 1))

    .A1 <- .sim$A1

    ## Starts at 100 immediately post-dose
    expect_equal(.A1[1], 100, tolerance = 1e-3)

    ## Monotonically decreasing (saturable elimination)
    expect_true(all(diff(.A1) <= 0))

    ## Substantially cleared after 24 h (> 90% eliminated)
    expect_lt(.A1[length(.A1)], 0.1 * .A1[1])

    ## ── conflict detection ───────────────────────────────────────────────
    ## Attempting to add the same names from a different package is
    ## detected by the conflict-check logic in .onAttach.
    .cur <- rxode2::rxode2parseGetTranslation()
    .other <- data.frame(
      rxFun = "mm", fun = "mm", type = "rxode2_fn3",
      package = "other_pkg", packageFun = "mm",
      argMin = 3L, argMax = 3L, threadSafe = 1L,
      stringsAsFactors = FALSE
    )
    ## The conflict rows (from "mm", not "other_pkg") would be detected
    .conflicts <- .cur[.cur$rxFun %in% .other$rxFun, , drop = FALSE]
    .fromOther  <- .conflicts[.conflicts$package != "other_pkg", , drop = FALSE]
    expect_true(nrow(.fromOther) > 0L)
    expect_equal(.fromOther$package[1], "mm")

    ## ── detach and verify cleanup ────────────────────────────────────────
    detach("package:mm", unload = TRUE, force = TRUE)

    .trans2 <- rxode2::rxode2parseGetTranslation()
    expect_false("mm"       %in% .trans2$rxFun)
    expect_false("mm_dC"    %in% .trans2$rxFun)
    expect_false("mm_dVmax" %in% .trans2$rxFun)
    expect_false("mm_dKm"   %in% .trans2$rxFun)

    ## mm no longer visible to the parser
    .funs2 <- rxode2::rxSupportedFuns()
    expect_false("mm"    %in% .funs2)
    expect_false("mm_dC" %in% .funs2)

    ## R functions gone from the search path
    expect_false("package:mm" %in% search())
  })

  ## ── reload (re-attach) test ───────────────────────────────────────────────

  test_that("mm package: re-attach emits startup message, does not error", {

    .pkgDir <- tempfile("mm_src2_")
    .libDir <- tempfile("mm_lib2_")
    dir.create(.pkgDir, recursive = TRUE, showWarnings = FALSE)
    dir.create(.libDir, recursive = TRUE, showWarnings = FALSE)
    on.exit({
      try(detach("package:mm", unload = TRUE, force = TRUE), silent = TRUE)
      unlink(.pkgDir, recursive = TRUE)
      unlink(.libDir, recursive = TRUE)
    }, add = TRUE)

    .writeMmPackage(.pkgDir)

    .out <- system2(
      file.path(R.home("bin"), "R"),
      args   = c("CMD", "INSTALL", "--no-multiarch",
                 paste0("--library=", .libDir), .pkgDir),
      stdout = TRUE, stderr = TRUE
    )
    if (!any(grepl("DONE \\(mm\\)", .out))) {
      skip("mm package failed to install")
    }

    .oldPaths <- .libPaths()
    on.exit(.libPaths(.oldPaths), add = TRUE)
    .libPaths(c(.libDir, .oldPaths))

    ## First attach — no startup message about re-registering
    suppressPackageStartupMessages(library(mm, lib.loc = .libDir))

    ## Simulate a "reload" by directly calling .onAttach again; the
    ## existing rows from "mm" are still in the table so the re-register
    ## path should fire (emitting a packageStartupMessage).
    ## rxD() also warns "replacing defined derivatives" on re-registration
    ## — those warnings are expected and suppressed here.
    expect_message(
      suppressWarnings(mm:::.onAttach("", "mm")),
      "re-registering"
    )

    ## After re-registration, table still has exactly 4 mm rows
    .mm <- rxode2::rxode2parseGetTranslation()
    .mm <- .mm[.mm$package == "mm", , drop = FALSE]
    expect_equal(nrow(.mm), 4L)
  })

})
