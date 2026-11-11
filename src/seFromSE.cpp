/*
 * seFromSE.c -- translate SymEngine printer output into rxode2/C syntax.
 *
 * This is the C replacement for the recursive R walker .rxFromSE()
 * (R/symengine.R).  rxFromSE() is ~90% of all symbolic-derivative time --
 * symengine::D() itself is ~0.5% -- because it re-parses each symengine string
 * with R's parse() and then emits with nested paste0() and a 9-deep sub()
 * regex chain, plus an options() save/restore per leaf.
 *
 * Correctness contract: the output must match the R implementation BYTE FOR
 * BYTE.  Anything this file is not certain it reproduces sets ctx->failed and
 * the R shim falls back to .rxFromSE().  Falling back is always safe; guessing
 * is not.  tests/testthat/test-symengine-translate-fixture.R pins the contract.
 *
 * No R API and no symengine call happens inside seFromSE1(); it is pure
 * string -> string over a private arena, which is what lets seBatch.h run it
 * across threads (symengine itself is built without thread-safe refcounting,
 * so only this half can be threaded).
 */
#include "seParse.h"
#include "seFromSE.g.d_parser.h"

#include "seFromSEemit.h"
#include "seBatch.h"
#include "seFromSEtables.h"

/* .rxUnXi(): symengine's dummy `_xi_<n>` is not a valid rxode2 name; R
   rewrites it to `rx_xi_<n>` (gsub("(?<!rx)_xi_", "rx_xi_", perl = TRUE))
   before anything else.  Done here so the R shim does not have to run a
   regular expression over every (possibly megabyte) line first. */
static const char *seUnXi(seCtx *ctx, const char *in) {
  const char *p = strstr(in, "_xi_");
  if (p == NULL) return in;
  const char *out = "", *seg = in;
  while (p != NULL) {
    if (!(p - in >= 2 && p[-2] == 'r' && p[-1] == 'x')) {
      out = seCat(ctx, out, seDup(ctx, seg, (size_t)(p - seg)), "rx",
                  NULL, NULL, NULL);
      seg = p;
    }
    p = strstr(p + 4, "_xi_");
  }
  return seCat(ctx, out, seg, NULL, NULL, NULL, NULL);
}

static const char *seFromSE1(seCtx *ctx, const char *in) {
  return seParseEmit(ctx, seUnXi(ctx, in), &parser_tables_rxode2seFromSE, seEmit);
}

/* resolve every fixed table once, before any thread starts */
static void seFromSEinit(void) {
  seNamedInit();
  seFnInit();
  seSymInit();
}

/* .Call entry; the R fallback for a declined element is .rxFromSE(). */
extern "C" SEXP _rxode2_rxFromSEChar(SEXP strVec, SEXP numDerS, SEXP tabs,
                                     SEXP stamp) {
  seTables tab;
  seFromSEinit();
  seTablesFromR(&tab, tabs);
  seSetScipen(stamp, SE_STAMP_SCIPEN);
  int cacheOn = seCacheBegin(stamp);
  return seRunBatch(strVec, seFromSE1, Rf_asInteger(numDerS), &tab,
                    SE_DIR_FROM, cacheOn);
}

/* .Call: as.character(<double>) as the emitter renders it, for the tests */
extern "C" SEXP _rxode2_seFormatDouble(SEXP x, SEXP scipen) {
  seScipen = Rf_asInteger(scipen) == NA_INTEGER ? 0 : Rf_asInteger(scipen);
  R_xlen_t n = Rf_xlength(x), i;
  SEXP ret = PROTECT(Rf_allocVector(STRSXP, n));
  char buf[400];
  for (i = 0; i < n; i++) {
    /* NA: this platform's printf may not print what R prints (Windows) */
    SET_STRING_ELT(ret, i, seDblFormat(REAL(x)[i], buf, sizeof(buf)) ?
                   Rf_mkChar(buf) : NA_STRING);
  }
  UNPROTECT(1);
  return ret;
}

/* .Call: the fixed name table, so a test can hold it to the R lists */
extern "C" SEXP _rxode2_seFromSEfnTable(void) {
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 7));
  SEXP nm = PROTECT(Rf_allocVector(STRSXP, 7));
  const char *cols[] = {"name", "kind", "flags", "a", "b", "c", "p"};
  SEXP v[7];
  int i, j;
  v[0] = PROTECT(Rf_allocVector(STRSXP, seNfnTab));
  v[1] = PROTECT(Rf_allocVector(INTSXP, seNfnTab));
  v[2] = PROTECT(Rf_allocVector(INTSXP, seNfnTab));
  for (j = 3; j < 7; j++) v[j] = PROTECT(Rf_allocVector(STRSXP, seNfnTab));
  for (i = 0; i < seNfnTab; i++) {
    const seFnInfo *f = &seFnTab[i];
    const char *s[4] = {f->a, f->b, f->c, f->p};
    SET_STRING_ELT(v[0], i, Rf_mkChar(f->name));
    INTEGER(v[1])[i] = f->kind;
    INTEGER(v[2])[i] = f->flags;
    for (j = 0; j < 4; j++) {
      SET_STRING_ELT(v[3 + j], i, s[j] == NULL ? NA_STRING : Rf_mkChar(s[j]));
    }
  }
  for (j = 0; j < 7; j++) {
    SET_VECTOR_ELT(ret, j, v[j]);
    SET_STRING_ELT(nm, j, Rf_mkChar(cols[j]));
  }
  Rf_setAttrib(ret, R_NamesSymbol, nm);
  UNPROTECT(9);
  return ret;
}
