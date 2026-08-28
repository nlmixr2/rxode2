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
#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* dparser function-pointer table.  tran.c has the dparserPtrIni definitions;
   here we only want the extern declarations. */
#include <dparserPtr.h>
#include "seFromSE.g.d_parser.h"

#include "seFromSEemit.h"

/* ------------------------------------------------------------ entry point --
   Pure string -> string: no R API, no symengine.  Returns a pointer into
   ctx's arena, or NULL when the caller must fall back to the R walker. */
static const char *seFromSE1(seCtx *ctx, const char *in) {
  ctx->failed = 0;
  D_Parser *p = new_D_Parser(&parser_tables_rxode2seFromSE,
                             sizeof(D_ParseNode_User));
  if (p == NULL) return NULL;
  p->save_parse_tree = 1;
  p->error_recovery = 0;
  D_ParseNode *pn = dparse(p, (char*) in, (int) strlen(in));
  const char *out = NULL;
  if (pn != NULL && p->syntax_errors == 0) {
    out = seEmit(ctx, pn);
    if (ctx->failed) out = NULL;
  }
  if (pn != NULL) free_D_ParseNode(p, pn);
  free_D_Parser(p);
  return out;
}

/* .Call entry: character vector in, character vector out.  Elements the C
   emitter declines are returned as NA_character_ so the R shim can route just
   those to .rxFromSE(). */
SEXP _rxode2_rxFromSEChar(SEXP strVec, SEXP numDerS) {
  if (TYPEOF(strVec) != STRSXP) {
    Rf_error("%s", "'strVec' must be a character vector");
  }
  R_xlen_t n = Rf_xlength(strVec), i;
  int numDer = Rf_asInteger(numDerS);
  SEXP ret = PROTECT(Rf_allocVector(STRSXP, n));
  /* resolve grammar symbol -> kind once; never inside the per-expression loop
     (and never inside a future parallel region -- it writes statics) */
  seKindsInit();
  seNamedInit();
  seCtx ctx;
  ctx.head = NULL; ctx.failed = 0; ctx.numDer = numDer;
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(strVec, i);
    if (el == NA_STRING) { SET_STRING_ELT(ret, i, NA_STRING); continue; }
    const char *out = seFromSE1(&ctx, CHAR(el));
    if (out == NULL) SET_STRING_ELT(ret, i, NA_STRING);
    else SET_STRING_ELT(ret, i, Rf_mkChar(out));
  }
  seArenaFree(&ctx);
  UNPROTECT(1);
  return ret;
}
