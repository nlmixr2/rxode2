/*
 * rxCse.cpp -- the rxCse .Call: common subexpression elimination in C.
 *
 * rxOptExpr() counts subexpressions in a named R list and looks them up with
 * [[text]], a linear scan, so the search is O(k^2) in the number of distinct
 * subexpressions.  On a second-order sensitivity model -- n*ndir^2 ODEs, each
 * a derivative of a line that already had n+1 terms -- k is enormous and that
 * search is what a large model spends its time in.  Here the map is a hash and
 * the per-statement walks run under OpenMP.
 *
 * The output must match the R implementation BYTE FOR BYTE: downstream codegen
 * consumes this text and nlmixr2est's saem rewrites the rx_expr_ prefix by
 * name.  Anything this file is not certain it reproduces makes the whole call
 * decline (NA_character_) so the R walker runs -- all or nothing, because a
 * partial result cannot be mixed with R's global counts.
 *
 * See rxCseA.h / rxCseB.h for why there are two renderers rather than one.
 */
#include "seParse.h"
#include "rxCse.g.d_parser.h"

#include "rxCseCtx.h"
#include "rxCseNode.h"
#include "rxCseNum.h"
#include "rxCseA.h"
#include "rxCseB.h"
#include "rxCseLhs.h"
#include "rxCseSel.h"
#include "rxCseStmt.h"
#include "rxomp.h"
#include <time.h>

/* rxode2's own thread count; see src/seBatch.h for the same declaration */
extern "C" int getRxThreads(int64_t n, bool throttle);

/* Below this many statements the parallel region costs more than it saves.
   Deliberately its own constant rather than seBatch.h's SE_MIN_PARALLEL: the
   unit here is a statement, not an expression, and a statement is far more
   work. */
#define CS_MIN_PARALLEL 32

/* ------------------------------------------------------------ growable text */
typedef struct csBuf { char *s; size_t n, cap; } csBuf;

static int csBufAdd(csBuf *b, const char *s) {
  size_t l = strlen(s);
  if (b->n + l + 1 > b->cap) {
    size_t want = (b->n + l + 1) * 2;
    char *p = (char*) realloc(b->s, want);
    if (p == NULL) return 0;
    b->s = p; b->cap = want;
  }
  memcpy(b->s + b->n, s, l + 1);
  b->n += l;
  return 1;
}

/* ------------------------------------------------------------------ pass 1 */
/* set RXCSE_DEBUG=1 to see which statement made the pass decline */
int csTraceOn(void);
static int csDebug(void) {
  static int d = -1;
  if (d == -1) { const char *e = getenv("RXCSE_DEBUG"); d = (e != NULL && *e == '1'); }
  return d;
}

int csTraceOn(void) { return csDebug(); }

static double csNow(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double) ts.tv_sec + 1e-9 * (double) ts.tv_nsec;
}
#define CS_PHASE(lbl) do { if (csDebug()) { double _n = csNow();                \
      REprintf("  phase %-10s %7.3fs\n", lbl, _n - _t0); _t0 = _n; } } while (0)


/* Hand the finished text to R under R_UnwindProtect: Rf_mkChar() allocates and
   can longjmp, and `outText` is malloc'd, so the cleanup is what frees it on
   the error path.  Same pattern as src/seBatch.h. */
typedef struct csFinish { char *text; } csFinish;

static SEXP csFinishFun(void *data) {
  csFinish *f = (csFinish*) data;
  return Rf_ScalarString(Rf_mkChar(f->text));
}

static void csFinishClean(void *data, Rboolean jump) {
  csFinish *f = (csFinish*) data;
  (void) jump;
  free(f->text);
  f->text = NULL;
}

/* ---------------------------------------------------------------- the call */
/* Everything one call needs, so the phases below can be separate functions
   instead of one 240-line body with a dozen `ok = 0` exits.  Each phase returns
   0 to decline, and csRunFree() is the single teardown for all of it. */
typedef struct csRun {
  R_xlen_t n;
  const char **in;                /* R_alloc'd; not ours to free */
  int nthr;
  csCtx *ctxs;                    /* one per thread, alive until teardown so
                                     the merged map can borrow arena keys */
  csMap all;                      /* merged counts */
  csMap rep;                      /* reduced text -> candidate index */
  csCand *cand;
  int ncand;
  const char **repNames;
  const char **out;               /* one rewritten statement each */
  const char ***usedBy;           /* the temporaries each statement referenced */
  int *nUsedBy;
} csRun;

static void csRunInit(csRun *r, R_xlen_t n, const char **in, int nthr) {
  memset(r, 0, sizeof(*r));
  r->n = n; r->in = in; r->nthr = nthr;
}

static void csRunFree(csRun *r) {
  int t, j;
  R_xlen_t i;
  if (r->cand != NULL) {
    for (j = 0; j < r->ncand; j++) { free(r->cand[j].reduced); free(r->cand[j].name); }
    free(r->cand);
  }
  if (r->usedBy != NULL) {
    for (i = 0; i < r->n; i++) free((void*) r->usedBy[i]);
    free(r->usedBy);
  }
  free(r->nUsedBy);
  free((void*) r->out);
  free((void*) r->repNames);
  csMapFree(&r->rep);
  csMapFree(&r->all);
  if (r->ctxs != NULL) {
    for (t = 0; t < r->nthr; t++) {
      if (r->ctxs[t].count != NULL) { csMapFree(r->ctxs[t].count); free(r->ctxs[t].count); }
      free((void*) r->ctxs[t].used);
      seArenaFree(&r->ctxs[t].arena);
    }
    free(r->ctxs);
  }
  memset(r, 0, sizeof(*r));
}

static int csRunAlloc(csRun *r) {
  int t;
  r->ctxs = (csCtx*) calloc((size_t) r->nthr, sizeof(csCtx));
  if (r->ctxs == NULL) return 0;
  for (t = 0; t < r->nthr; t++) {
    r->ctxs[t].count = (csMap*) calloc(1, sizeof(csMap));
    if (r->ctxs[t].count == NULL || !csMapInit(r->ctxs[t].count, 1024)) return 0;
  }
  return 1;
}

/* Both passes are the same walk -- every statement once, each thread using its
   own context -- so the walk is written once and the pass is the argument. */
template <typename Fn>
static void csEachStmt(csRun *r, Fn job) {
#ifdef _OPENMP
#pragma omp parallel num_threads(r->nthr)
#endif
  {
    int me = 0;
#ifdef _OPENMP
    me = omp_get_thread_num();
#endif
    if (me < r->nthr) {
      R_xlen_t j;
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
      for (j = 0; j < r->n; j++) job(&r->ctxs[me], j);
    }
  }
}

/* A thread cannot touch the R API, so a decline is recorded in the context and
   reported once the region is over.  0 = some statement declined. */
static int csReportDeclines(csRun *r) {
  int t, ok = 1;
  for (t = 0; t < r->nthr; t++) {
    if (!r->ctxs[t].anyFail) continue;
    ok = 0;
    if (csDebug()) {
      REprintf("rxCse: declined (%s) [%s]\n",
               r->ctxs[t].failWhy == NULL ? "?" : r->ctxs[t].failWhy,
               r->ctxs[t].failLine == NULL ? "?" : r->ctxs[t].failLine);
    }
  }
  return ok;
}

/* min(firstSeen) on merge is what makes the naming independent of how the
   statements were distributed across threads. */
static int csMergeCounts(csRun *r) {
  int t;
  if (!csMapInit(&r->all, 4096)) return 0;
  for (t = 0; t < r->nthr; t++) {
    if (!csMapMerge(&r->all, r->ctxs[t].count)) return 0;
  }
  return 1;
}

/* pass 1: how many times does each subexpression appear across the model? */
static int csCountPhase(csRun *r) {
  const char **in = r->in;
  csEachStmt(r, [in](csCtx *c, R_xlen_t j) { csCountStmt(c, in[j], (int) j); });
  return csReportDeclines(r) && csMergeCounts(r);
}

/* count > 1, then drop bare numbers, bare THETA[n]/ETA[n] and anything leading
   with `-`, then order by nchar with first encounter breaking ties
   (R/rxOptExpr.R:1000-1013) */
static int csSelectPhase(csRun *r) {
  int j;
  r->cand = (csCand*) calloc((size_t) (r->all.used + 1), sizeof(csCand));
  if (r->cand == NULL) return 0;
  for (j = 0; j < r->all.n; j++) {
    csEntry *e = &r->all.e[j];
    if (e->key == NULL || e->count <= 1) continue;
    if (csIsNum(e->key) || csIsThetaEta(e->key) || e->key[0] == '-') continue;
    r->cand[r->ncand].key = e->key;
    r->cand[r->ncand].firstSeen = e->firstSeen;
    r->cand[r->ncand].len = strlen(e->key);
    r->ncand++;
  }
  if (r->ncand == 0) return 0;          /* nothing to do: let R take the exit */
  qsort(r->cand, (size_t) r->ncand, sizeof(csCand), csCandCmp);
  if (csDebug()) {
    int d;
    for (d = 0; d < r->ncand && d < 30; d++)
      REprintf("  cand %3d len=%2d stmt=%u pos=%u  [%s]\n", d,
               (int) r->cand[d].len, (unsigned)(r->cand[d].firstSeen >> 32),
               (unsigned)(r->cand[d].firstSeen & 0xffffffffu), r->cand[d].key);
  }
  return 1;
}

/* name them rx_expr_0..n and rewrite each in terms of the shorter ones
   (src/rxOptRep.cpp) */
static int csReducePhase(csRun *r) {
  int j, k;
  if (!csMapInit(&r->rep, 1024)) return 0;
  r->repNames = (const char**) calloc((size_t) r->ncand, sizeof(char*));
  if (r->repNames == NULL) return 0;
  for (j = 0; j < r->ncand; j++) {
    size_t cap = r->cand[j].len + 64;
    r->cand[j].reduced = (char*) malloc(cap);
    r->cand[j].name = (char*) malloc(32);
    if (r->cand[j].reduced == NULL || r->cand[j].name == NULL) return 0;
    memcpy(r->cand[j].reduced, r->cand[j].key, r->cand[j].len + 1);
    snprintf(r->cand[j].name, 32, "rx_expr_%d", j);
    for (k = 0; k < j; k++) {
      if (csReplace1(&r->cand[j].reduced, &cap,
                     r->cand[k].reduced, r->cand[k].name) < 0) {
        return 0;                       /* out of memory: decline, never guess */
      }
    }
    r->repNames[j] = r->cand[j].name;
    /* first wins on a duplicate reduced text, matching R's list, and it keeps
       the stored index exact (csMapAdd would SUM on a re-add) */
    if (csMapGet(&r->rep, r->cand[j].reduced) == NULL &&
        !csMapAdd(&r->rep, r->cand[j].reduced, j, 0)) return 0;
  }
  return 1;
}

/* the outputs pass 2 fills in, and the replacement map every thread reads */
static int csRewriteSetup(csRun *r) {
  int t;
  r->out = (const char**) calloc((size_t) r->n, sizeof(char*));
  r->usedBy = (const char***) calloc((size_t) r->n, sizeof(char**));
  r->nUsedBy = (int*) calloc((size_t) r->n, sizeof(int));
  if (r->out == NULL || r->usedBy == NULL || r->nUsedBy == NULL) return 0;
  for (t = 0; t < r->nthr; t++) {
    r->ctxs[t].rep = &r->rep;
    r->ctxs[t].repNames = r->repNames;
    r->ctxs[t].usedCap = r->ncand;
    r->ctxs[t].used = (const char**) calloc((size_t) r->ncand, sizeof(char*));
    if (r->ctxs[t].used == NULL) return 0;
  }
  return 1;
}

/* which temporaries this statement referenced, in the order it used them */
static void csRecordUsed(csRun *r, csCtx *c, R_xlen_t j) {
  int u;
  if (c->nused == 0) return;
  r->usedBy[j] = (const char**) malloc(sizeof(char*) * (size_t) c->nused);
  if (r->usedBy[j] == NULL) {
    /* Must NOT be silent: this list is what places each `rx_expr_i~...`
       definition before its first use, so losing it would emit a statement
       referencing a temporary that is never defined. */
    c->anyFail = 1;
    c->failWhy = "out of memory recording temporaries";
    return;
  }
  for (u = 0; u < c->nused; u++) r->usedBy[j][u] = c->used[u];
  r->nUsedBy[j] = c->nused;
}

/* pass 2: the same walk, substituting the chosen names in */
static int csRewritePhase(csRun *r) {
  R_xlen_t i;
  const char **in = r->in;
  if (!csRewriteSetup(r)) return 0;
  csEachStmt(r, [r, in](csCtx *c, R_xlen_t j) {
    r->out[j] = csOptStmt(c, in[j], (int) j);
    if (r->out[j] != NULL) csRecordUsed(r, c, j);
  });
  for (i = 0; i < r->n; i++) if (r->out[i] == NULL) return 0;
  return csReportDeclines(r);
}

/* the body of a definition is machine B over the REDUCED key, re-parsed */
static const char *csDefBody(csCtx *dc, const char *reduced) {
  csParse od = {NULL, NULL};
  const char *body = NULL;
  memset(dc, 0, sizeof(*dc));
  if (csParseOpen(&od, reduced)) {
    D_ParseNode *sd = csStmtNode(od.pn);
    if (!csStmtIsAssign(sd)) body = csB(dc, d_get_child(sd, 0));
  }
  csParseClose(&od);                    /* also on the failing path */
  return dc->arena.failed ? NULL : body;
}

/* did statement `i` reference the temporary named by candidate `j`? */
static int csUsesCand(csRun *r, R_xlen_t i, int j) {
  int u;
  for (u = 0; u < r->nUsedBy[i]; u++) {
    if (r->usedBy[i][u] == r->cand[j].name) return 1;
  }
  return 0;
}

/* Every temporary statement `i` is the FIRST to use, defined here and marked
   so no later statement defines it again.  Ascending index, so nested
   temporaries are defined before the ones that reference them. */
static int csEmitDefsFor(csRun *r, R_xlen_t i, char *emitted, csBuf *b) {
  int j;
  for (j = 0; j < r->ncand; j++) {
    const char *body;
    csCtx dc;
    int ok;
    if (emitted[j] || !csUsesCand(r, i, j)) continue;
    body = csDefBody(&dc, r->cand[j].reduced);
    ok = body != NULL &&
      csBufAdd(b, r->cand[j].name) && csBufAdd(b, "~") &&
      csBufAdd(b, body) && csBufAdd(b, "\n");
    seArenaFree(&dc.arena);
    if (!ok) return 0;
    emitted[j] = 1;
  }
  return 1;
}

/* assemble the model: each definition immediately before the first statement
   that uses it.  Returns the text, which the caller owns, or NULL. */
static char *csEmitPhase(csRun *r) {
  csBuf b;
  char *emitted = (char*) calloc((size_t) r->ncand, 1);
  R_xlen_t i;
  int ok = 1;
  memset(&b, 0, sizeof(b));
  if (emitted == NULL) return NULL;
  for (i = 0; ok && i < r->n; i++) {
    ok = csEmitDefsFor(r, i, emitted, &b) && csBufAdd(&b, r->out[i]) &&
      (i + 1 == r->n || csBufAdd(&b, "\n"));
  }
  free(emitted);
  if (ok) return b.s;
  free(b.s);
  return NULL;
}

/* the model's statements as C strings.  CHAR() before any thread starts, with
   R_alloc so an error cannot leak them (the rule from src/seBatch.h:88-104);
   NULL when any element is NA, which declines the whole call. */
static const char **csReadLines(SEXP linesVec, R_xlen_t n) {
  const char **in = (const char**) R_alloc((size_t) n, sizeof(char*));
  R_xlen_t i;
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(linesVec, i);
    if (el == NA_STRING) return NULL;
    in[i] = CHAR(el);
  }
  return in;
}

/* Debugging runs single threaded so the reported decline is deterministic --
   with several threads you get whichever one happened to fail.  Safety does
   not depend on this: nothing in the regions touches the R API, the reason is
   recorded and printed afterwards. */
static int csPickThreads(R_xlen_t n) {
  int nthr;
  if (n < CS_MIN_PARALLEL || csDebug()) return 1;
  nthr = getRxThreads((int64_t) n, true);
  if (nthr < 1) nthr = 1;
  if ((R_xlen_t) nthr > n) nthr = (int) n;
  return nthr;
}

/* count -> select -> reduce -> rewrite -> emit; NULL if any of them declines */
static char *csRunAll(csRun *r) {
  char *outText = NULL;
  double _t0 = csNow();
  if (csRunAlloc(r) && csCountPhase(r)) {
    CS_PHASE("count");
    if (csSelectPhase(r)) {
      CS_PHASE("select");
      if (csReducePhase(r)) {
        CS_PHASE("reduce");
        if (csRewritePhase(r)) {
          CS_PHASE("rewrite");
          outText = csEmitPhase(r);
          CS_PHASE("emit");
        }
      }
    }
  }
  return outText;
}

extern "C" SEXP _rxode2_rxCse(SEXP linesVec) {
  R_xlen_t n;
  const char **in;
  csRun r;
  char *outText;
  SEXP ret;

  if (TYPEOF(linesVec) != STRSXP) Rf_error("%s", "'linesVec' must be character");
  n = Rf_xlength(linesVec);
  if (n == 0) return Rf_ScalarString(NA_STRING);
  in = csReadLines(linesVec, n);
  if (in == NULL) return Rf_ScalarString(NA_STRING);

  csRunInit(&r, n, in, csPickThreads(n));
  outText = csRunAll(&r);
  csRunFree(&r);

  /* every other hand-managed allocation is released by here; outText is the
     last one, and R_UnwindProtect is what frees it if Rf_mkChar longjmps */
  if (outText == NULL) return Rf_ScalarString(NA_STRING);
  {
    csFinish fin; fin.text = outText;
    SEXP cont = PROTECT(R_MakeUnwindCont());
    ret = R_UnwindProtect(csFinishFun, &fin, csFinishClean, &fin, cont);
    /* csFinishClean() NULLs the pointer, so this frees it if the cleanup did
       not already run and is a no-op if it did.  Correct either way: whether
       R_UnwindProtect runs the cleanup on a NORMAL return is not something to
       take on faith. */
    free(fin.text);
    fin.text = NULL;
    UNPROTECT(1);
  }
  return ret;
}
