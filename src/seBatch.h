/*
 * seBatch.h -- run a translation over a batch of expressions, in parallel.
 *
 * Shared by src/seFromSE.c and src/rxToSE.c, which differ only in which
 * per-expression translator they hand over.
 *
 * Threading is safe here for two reasons established rather than assumed:
 *
 *  - dparse() is re-entrant as long as each thread builds its OWN D_Parser
 *    from the shared read-only tables.  Verified in inst/tools/
 *    dparserReentrancy.c: 48000 concurrent parses with no mismatch, clean
 *    under both ThreadSanitizer and AddressSanitizer.
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

/* rxode2's own thread count: respects setRxThreads() and throttles small
   batches, so it is also the "is this worth threading" gate */
extern int getRxThreads(int64_t n, bool throttle);

/* below this many expressions the parallel region costs more than it saves */
#define SE_MIN_PARALLEL 250

typedef const char *(*seXlateFn)(seCtx *ctx, const char *in);

/* Convert the batch's results into the output vector, freeing each one as it
   goes, under R_UnwindProtect so that an allocation failure part way through
   cannot strand the rest. */
typedef struct seConvert {
  R_xlen_t n;
  char **out;
  SEXP ret;
} seConvert;

static SEXP seConvertResults(void *data) {
  seConvert *c = (seConvert*) data;
  R_xlen_t i;
  for (i = 0; i < c->n; i++) {
    if (c->out[i] == NULL) {
      SET_STRING_ELT(c->ret, i, NA_STRING);
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
   the R shim routes just those to the R walker. */
static SEXP seRunBatch(SEXP strVec, seXlateFn xlate, int numDer,
                       const seDeriv *derivs, int nd) {
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
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(strVec, i);
    in[i] = (el == NA_STRING) ? NULL : CHAR(el);
    out[i] = NULL;
  }

  /* Creating a parallel region costs more than it saves on a small batch.
     Measured on the fixture: the translation breaks even by roughly a hundred
     expressions and is clearly ahead by five hundred, while a whole
     jacobian+sensitivity build of a two-to-eight state model came out 5-13%
     SLOWER with threads because its batches are far below that.  The real
     batches worth threading are the jacobian and sensitivity ones, which run
     450-900 expressions at fifteen states and grow from there. */
  int nthr = 1;
  if (n >= SE_MIN_PARALLEL) {
    nthr = getRxThreads((int64_t) n, true);
    if (nthr < 1) nthr = 1;
    if ((R_xlen_t) nthr > n) nthr = (int) n;
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(nthr)
#endif
  {
    seCtx ctx;
    ctx.head = NULL; ctx.failed = 0; ctx.numDer = numDer;
    ctx.derivs = derivs; ctx.nderivs = nd;
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (i = 0; i < n; i++) {
      if (in[i] == NULL) continue;                 /* NA in, NA out */
      const char *r = xlate(&ctx, in[i]);
      /* NULL means the emitter declined; the R shim then falls back */
      if (r != NULL) out[i] = strdup(r);
    }
    seArenaFree(&ctx);
  }

  /* Rf_mkChar() allocates, so the conversion below can unwind to the top level
     with strdup'd results still outstanding.  A thread cannot call R_alloc,
     which is why the results are malloc'd in the first place, so they have to
     be freed by hand -- and R_UnwindProtect is what guarantees that happens on
     the error path as well as the normal one.  seFreeResults() is idempotent:
     the conversion nulls each pointer as it frees it. */
  seConvert cv; cv.n = n; cv.out = out; cv.ret = ret;
  SEXP cont = PROTECT(R_MakeUnwindCont());   /* R_NilValue here is "bad value" */
  R_UnwindProtect(seConvertResults, &cv, seFreeResults, &cv, cont);
  UNPROTECT(2);
  return ret;
}

#endif /* __SE_BATCH_H__ */
