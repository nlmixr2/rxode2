/*
 * rxToSE.c -- translate rxode2 expression syntax into the text symengine parses.
 *
 * The companion of seFromSE.c and the other half of the R text translation
 * that dominates symbolic-derivative setup.  .rxToSE() walks R's own parse
 * tree through ~30 special-case handlers; after the seFromSE work it is the
 * largest remaining pure-R translation cost in the pipeline.
 *
 * Correctness contract, as for seFromSE.c: the output must match the R
 * implementation BYTE FOR BYTE, and anything this file is not certain it
 * reproduces sets ctx->failed so the R shim falls back to rxToSE().  Falling
 * back is always safe; guessing is not.
 *
 * No R API and no symengine call happens inside rxToSE1(); it is pure
 * string -> string over the same private arena seFromSE.c uses.
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
#include "rxToSE.g.d_parser.h"

#include "rxToSEemit.h"
#include "seBatch.h"

/* Pure string -> string: no R API, no symengine.  Returns a pointer into
   ctx's arena, or NULL when the caller must fall back to the R walker. */
static const char *rxToSE1(seCtx *ctx, const char *in) {
  ctx->failed = 0;
  D_Parser *p = new_D_Parser(&parser_tables_rxode2rxToSE,
                             sizeof(D_ParseNode_User));
  if (p == NULL) return NULL;
  p->save_parse_tree = 1;
  p->error_recovery = 0;
  D_ParseNode *pn = dparse(p, (char*) in, (int) strlen(in));
  const char *out = NULL;
  if (pn != NULL && p->syntax_errors == 0) {
    out = rtEmit(ctx, pn);
    if (ctx->failed) out = NULL;
  }
  if (pn != NULL) free_D_ParseNode(p, pn);
  free_D_Parser(p);
  return out;
}

/* .Call entry: character vector in, character vector out.  Elements the C
   emitter declines are returned as NA_character_ so the R shim can route just
   those to rxToSE(). */
SEXP _rxode2_rxToSEChar(SEXP strVec) {
  return seRunBatch(strVec, rxToSE1, 0, NULL, 0);
}
