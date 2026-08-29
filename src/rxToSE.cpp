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
#include "seParse.h"
#include "rxToSE.g.d_parser.h"

#include "rxToSEemit.h"
#include "seBatch.h"

static const char *rxToSE1(seCtx *ctx, const char *in) {
  return seParseEmit(ctx, in, &parser_tables_rxode2rxToSE, rtEmit);
}

/* .Call entry; the R fallback for a declined element is rxToSE(). */
extern "C" SEXP _rxode2_rxToSEChar(SEXP strVec) {
  return seRunBatch(strVec, rxToSE1, 0, NULL, 0);
}
