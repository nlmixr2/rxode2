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
extern "C" SEXP _rxode2_rxToSEChar(SEXP strVec, SEXP stamp) {
  rtSymInit();
  seSetScipen(stamp, SE_STAMP_SCIPEN);
  int cacheOn = seCacheBegin(stamp);
  return seRunBatch(strVec, rxToSE1, 0, NULL, SE_DIR_TO, cacheOn);
}

/* A string out of the walk's arena into R's (R_alloc), so it survives
   seArenaFree() and is released if an R error unwinds the .Call */
static const char *rtKeep(const char *s) {
  char *c = R_alloc(strlen(s) + 1, 1);
  strcpy(c, s);
  return c;
}

static const char **rtKeepAll(const char **s, int n) {
  const char **out = (const char**) R_alloc((size_t)(n + 1), sizeof(char*));
  int i;
  for (i = 0; i < n; i++) out[i] = rtKeep(s[i]);
  return out;
}

static SEXP rtStrings(const char **s, int n) {
  SEXP v = PROTECT(Rf_allocVector(STRSXP, n));
  int i;
  for (i = 0; i < n; i++) SET_STRING_ELT(v, i, Rf_mkChar(s[i]));
  UNPROTECT(1);
  return v;
}

/* doubles: R reads a THETA[]/ETA[] index as a parsed number */
static SEXP rtIndices(const int *x, int n) {
  SEXP v = PROTECT(Rf_allocVector(REALSXP, n));
  int i;
  for (i = 0; i < n; i++) REAL(v)[i] = (double) x[i];
  UNPROTECT(1);
  return v;
}

/* what one walk in collect mode found, copied out of its arena */
typedef struct {
  const char *text;
  const char **syms, **thetaFuns, **etaFuns;
  int *thetas, *etas;
  int nsyms, nthetas, netas;
} rtEnvFound;

static void rtEnvKeep(seCtx *ctx, const char *out, rtEnvFound *f) {
  int i;
  f->text = rtKeep(out);
  f->nsyms = ctx->nsyms;
  f->nthetas = ctx->nthetas;
  f->netas = ctx->netas;
  f->syms = rtKeepAll(ctx->syms, ctx->nsyms);
  f->thetaFuns = rtKeepAll(ctx->thetaFuns, ctx->nthetas);
  f->etaFuns = rtKeepAll(ctx->etaFuns, ctx->netas);
  f->thetas = (int*) R_alloc((size_t)(f->nthetas + 1), sizeof(int));
  f->etas = (int*) R_alloc((size_t)(f->netas + 1), sizeof(int));
  for (i = 0; i < f->nthetas; i++) f->thetas[i] = ctx->thetas[i];
  for (i = 0; i < f->netas; i++) f->etas[i] = ctx->etas[i];
}

/* .Call: one right-hand side being loaded into a symengine environment.
   Returns list(text, syms, theta, eta, thetaCall, etaCall) -- the
   translation, the symbol leaves it read, the THETA[]/ETA[] indices and the
   ..curCall path each was read under -- so R can make the same bindings
   .rxToSENameOrAtomic()/.rxToSESquareBracket() would; NULL when the emitter
   declines and the R walker must run.  Not cached: the leaves matter. */
extern "C" SEXP _rxode2_rxToSEenv(SEXP str, SEXP stamp) {
  if (TYPEOF(str) != STRSXP || Rf_xlength(str) != 1 ||
      STRING_ELT(str, 0) == NA_STRING) {
    return R_NilValue;
  }
  rtSymInit();
  seSetScipen(stamp, SE_STAMP_SCIPEN);
  seCtx ctx;
  memset(&ctx, 0, sizeof(seCtx));
  ctx.collect = 1;
  const char *out = rxToSE1(&ctx, CHAR(STRING_ELT(str, 0)));
  rtEnvFound f;
  /* copy out before anything below can allocate (and so error) */
  if (out != NULL) rtEnvKeep(&ctx, out, &f);
  seArenaFree(&ctx);
  if (out == NULL) return R_NilValue;

  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 6));
  SET_VECTOR_ELT(ret, 0, Rf_mkString(f.text));
  SET_VECTOR_ELT(ret, 1, rtStrings(f.syms, f.nsyms));
  SET_VECTOR_ELT(ret, 2, rtIndices(f.thetas, f.nthetas));
  SET_VECTOR_ELT(ret, 3, rtIndices(f.etas, f.netas));
  SET_VECTOR_ELT(ret, 4, rtStrings(f.thetaFuns, f.nthetas));
  SET_VECTOR_ELT(ret, 5, rtStrings(f.etaFuns, f.netas));
  UNPROTECT(1);
  return ret;
}
