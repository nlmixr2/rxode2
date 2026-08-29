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
 * string -> string over a private arena.  That is deliberate: the batch entry
 * point can later run the per-expression loop under OpenMP (symengine itself
 * is built without thread-safe refcounting, so only this half can be threaded).
 */
#include "seParse.h"
#include "seFromSE.g.d_parser.h"

#include "seFromSEemit.h"
#include "seBatch.h"

static const char *seFromSE1(seCtx *ctx, const char *in) {
  return seParseEmit(ctx, in, &parser_tables_rxode2seFromSE, seEmit);
}

/* .Call entry (below); the R fallback for a declined element is .rxFromSE().

   Bring the derivative templates across from R into plain C.  Done once, here,
   where the R API is allowed: CHAR() pointers stay valid while the caller's
   vectors are protected, so the walk itself never touches R.  Caller frees. */
static seDeriv *seDerivsFromR(SEXP dName, SEXP dWhich, SEXP dTmpl, int *nOut) {
  int n = (TYPEOF(dName) == STRSXP) ? (int) Rf_xlength(dName) : 0;
  seDeriv *d = NULL;
  int i;
  *nOut = 0;
  if (n <= 0) return NULL;
  /* R_alloc, not malloc: STRING_ELT/INTEGER below type check and can longjmp,
     and so can Rf_mkChar() inside seRunBatch() afterwards, either of which
     would leave a malloc here unfreed.  R_alloc is released when the .Call
     unwinds either way, so the caller has nothing to free. */
  d = (seDeriv*) R_alloc((size_t) n, sizeof(seDeriv));
  for (i = 0; i < n; i++) {
    d[i].name = CHAR(STRING_ELT(dName, i));
    d[i].which = INTEGER(dWhich)[i];
    d[i].tmpl = CHAR(STRING_ELT(dTmpl, i));
  }
  *nOut = n;
  return d;
}

extern "C" SEXP _rxode2_rxFromSEChar(SEXP strVec, SEXP numDerS,
                          SEXP dName, SEXP dWhich, SEXP dTmpl) {
  /* resolve the named-constant buckets once, before any thread starts */
  seNamedInit();
  int nd = 0;
  seDeriv *derivs = seDerivsFromR(dName, dWhich, dTmpl, &nd);
  return seRunBatch(strVec, seFromSE1, Rf_asInteger(numDerS), derivs, nd);
}
