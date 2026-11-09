/*
 * seBatch.h -- run a translation over a batch of expressions, in parallel.
 *
 * Shared by src/seFromSE.c and src/rxToSE.c, which differ only in which
 * per-expression translator they hand over.
 *
 * Threading is safe here for two reasons established rather than assumed:
 *
 *  - dparse() is re-entrant as long as each thread builds its OWN D_Parser
 *    from the shared read-only tables -- but only from dparser 1.3.2.  Before
 *    that, parse.c shared one static path vector across parses and clang
 *    builds crashed (#1427), so the batch runs serially unless
 *    rxDparserParallel says the loaded dparser is new enough.
 *  - the translators touch neither the R API nor symengine.  That is not an
 *    accident; symengine is built here WITHOUT thread-safe refcounting, so it
 *    could not be called from here even if we wanted to.
 *
 * There is deliberately no thread-indexed state.  Each thread declares its own
 * seCtx inside the parallel region and frees its own arena at the end of it,
 * so nothing is indexed by omp_get_thread_num() and the "buffer sized by
 * omp_get_max_threads() but indexed by something else" class of bug cannot
 * arise.  Results are strdup()ed out of the per-thread arena before it dies;
 * that is one malloc per expression against a parse, which does not show up.
 */
#ifndef __SE_BATCH_H__
#define __SE_BATCH_H__

#include <stdint.h>
#include <stdbool.h>
#include "rxomp.h"
#include "seCache.h"

/* rxode2's own thread count: respects setRxThreads() and throttles small
   batches, so it is also the "is this worth threading" gate */
extern "C" int getRxThreads(int64_t n, bool throttle);
/* 1 when the loaded dparser can run dparse() concurrently; src/tran.c */
extern "C" int rxDparserParallel;

/* below this many expressions the parallel region costs more than it saves */
#define SE_MIN_PARALLEL 250

typedef const char *(*seXlateFn)(seCtx *ctx, const char *in);

/* Convert the batch's results into the output vector, freeing each one as it
   goes, under R_UnwindProtect so that an allocation failure part way through
   cannot strand the rest. */
typedef struct seConvert {
  R_xlen_t n;
  char **out;
  const char **in;       /* NULL = already filled (cache hit) or NA */
  SEXP ret;
} seConvert;

static SEXP seConvertResults(void *data) {
  seConvert *c = (seConvert*) data;
  R_xlen_t i;
  for (i = 0; i < c->n; i++) {
    if (c->out[i] == NULL) {
      if (c->in[i] != NULL) SET_STRING_ELT(c->ret, i, NA_STRING);
    } else {
      SET_STRING_ELT(c->ret, i, Rf_mkChar(c->out[i]));
      free(c->out[i]);
      c->out[i] = NULL;
    }
  }
  return c->ret;
}

static void seFreeResults(void *data, Rboolean jump) {
  seConvert *c = (seConvert*) data;
  R_xlen_t i;
  (void) jump;   /* the same cleanup either way */
  for (i = 0; i < c->n; i++) {
    if (c->out[i] != NULL) {
      free(c->out[i]);
      c->out[i] = NULL;
    }
  }
}

/* .Call shape for both translators: character vector in, character vector out,
   one for one.  An element the emitter declines comes back NA_character_, and
   the R shim routes just those to the R walker; the reason for each decline is
   attached as attr(, "why") so the shim knows whether the R walker's answer
   may be cached.

   `dir` is SE_DIR_FROM/SE_DIR_TO and `cacheOn` the result of seCacheBegin():
   cached inputs are answered before the parallel region, and new
   translations stored after it, so the cache is never touched from a
   thread. */
static SEXP seRunBatch(SEXP strVec, seXlateFn xlate, int numDer,
                       const seTables *tab, int dir, int cacheOn) {
  if (TYPEOF(strVec) != STRSXP) {
    Rf_error("%s", "'strVec' must be a character vector");
  }
  R_xlen_t n = Rf_xlength(strVec), i;
  SEXP ret = PROTECT(Rf_allocVector(STRSXP, n));
  if (n == 0) { UNPROTECT(1); return ret; }

  /* CHAR() is R API, so pull every input out before any thread starts.  The
     pointers stay valid while strVec is protected.

     R_alloc, not malloc: Rf_mkChar() in the conversion loop below allocates,
     so it can longjmp out of this function on memory exhaustion, and any
     malloc here would then never be freed.  R_alloc is released when the .Call
     unwinds, whether it returns or errors.  Allocating on the main thread
     before the parallel region is what makes it legal -- the threads only READ
     these arrays, which is fine; it is R_alloc ITSELF that must not be called
     from a thread. */
  const char **in = (const char**) R_alloc((size_t) n, sizeof(char*));
  char **out = (char**) R_alloc((size_t) n, sizeof(char*));
  int *why = (int*) R_alloc((size_t) n, sizeof(int));
  R_xlen_t nWork = 0;
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(strVec, i);
    in[i] = (el == NA_STRING) ? NULL : CHAR(el);
    out[i] = NULL;
    why[i] = 0;
    if (el == NA_STRING) {                   /* NA in, NA out */
      SET_STRING_ELT(ret, i, NA_STRING);
      why[i] = -1;
      continue;
    }
    if (in[i] != NULL && cacheOn) {
      const char *hit = seCacheGet(dir, in[i]);
      if (hit != NULL) {
        /* converted now, while the cache entry is certainly alive -- storing
           the misses below may clear the cache */
        SET_STRING_ELT(ret, i, Rf_mkChar(hit));
        seStatsHit(dir);
        in[i] = NULL;
        why[i] = -1;                         /* answered from the cache */
        continue;
      }
    }
    if (in[i] != NULL) nWork++;
  }

  /* Creating a parallel region costs more than it saves on a small batch.
     Measured on the fixture: the translation breaks even by roughly a hundred
     expressions and is clearly ahead by five hundred, while a whole
     jacobian+sensitivity build of a two-to-eight state model came out 5-13%
     SLOWER with threads because its batches are far below that.  The real
     batches worth threading are the jacobian and sensitivity ones, which run
     450-900 expressions at fifteen states and grow from there. */
  int nthr = 1;
  if (nWork >= SE_MIN_PARALLEL && rxDparserParallel) {
    nthr = getRxThreads((int64_t) nWork, true);
    if (nthr < 1) nthr = 1;
    if ((R_xlen_t) nthr > nWork) nthr = (int) nWork;
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(nthr)
#endif
  {
    seCtx ctx;
    ctx.head = NULL; ctx.failed = 0; ctx.why = 0; ctx.numDer = numDer;
    ctx.nsubs = 0;
    ctx.collect = 0;
    ctx.syms = NULL; ctx.nsyms = 0; ctx.msyms = 0;
    ctx.thetas = NULL; ctx.nthetas = 0; ctx.mthetas = 0;
    ctx.etas = NULL; ctx.netas = 0; ctx.metas = 0;
    ctx.ncall = 0;
    ctx.thetaFuns = NULL; ctx.nthetaFuns = 0; ctx.mthetaFuns = 0;
    ctx.etaFuns = NULL; ctx.netaFuns = 0; ctx.metaFuns = 0;
    ctx.tab = tab;
#ifdef _OPENMP
#pragma omp for schedule(dynamic, 8)
#endif
    for (i = 0; i < n; i++) {
      if (in[i] == NULL) continue;                 /* NA, or a cache hit */
      ctx.why = 0;
      const char *r = xlate(&ctx, in[i]);
      /* NULL means the emitter declined; the R shim then falls back */
      if (r != NULL) {
        out[i] = strdup(r);
      } else {
        why[i] = ctx.why == 0 ? SE_WHY_PARSE : ctx.why;
      }
    }
    seArenaFree(&ctx);
  }

  /* bookkeeping on the main thread: statistics, and the cache */
  int anyNA = 0;
  for (i = 0; i < n; i++) {
    if (in[i] == NULL) continue;
    seStatsAdd(dir, why[i]);
    if (out[i] != NULL) {
      if (cacheOn) seCachePut(dir, CHAR(STRING_ELT(strVec, i)), out[i]);
    } else {
      anyNA = 1;
    }
  }

  /* Rf_mkChar() allocates, so the conversion below can unwind to the top level
     with strdup'd results still outstanding.  A thread cannot call R_alloc,
     which is why the results are malloc'd in the first place, so they have to
     be freed by hand -- and R_UnwindProtect is what guarantees that happens on
     the error path as well as the normal one.  seFreeResults() is idempotent:
     the conversion nulls each pointer as it frees it. */
  seConvert cv; cv.n = n; cv.out = out; cv.ret = ret; cv.in = in;
  SEXP cont = PROTECT(R_MakeUnwindCont());   /* R_NilValue here is "bad value" */
  R_UnwindProtect(seConvertResults, &cv, seFreeResults, &cv, cont);
  if (anyNA) {
    SEXP w = PROTECT(Rf_allocVector(INTSXP, n));
    for (i = 0; i < n; i++) INTEGER(w)[i] = why[i] < 0 ? 0 : why[i];
    Rf_setAttrib(ret, Rf_install("why"), w);
    UNPROTECT(1);
  }
  UNPROTECT(2);
  return ret;
}

#endif /* __SE_BATCH_H__ */
