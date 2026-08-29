/*
 * rxCse.c -- common subexpression elimination in C.
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

/* --------------------------------------------------------------- one parse */
typedef struct csParse { D_Parser *p; D_ParseNode *pn; } csParse;

static int csParseOpen(csParse *o, const char *in) {
  o->p = new_D_Parser(&parser_tables_rxode2cse, sizeof(D_ParseNode_User));
  o->pn = NULL;
  if (o->p == NULL) return 0;
  o->p->save_parse_tree = 1;
  o->p->error_recovery = 0;
  o->pn = dparse(o->p, (char*) in, (int) strlen(in));
  if (o->pn == NULL || o->p->syntax_errors != 0) return 0;
  return 1;
}

static void csParseClose(csParse *o) {
  if (o->pn != NULL) free_D_ParseNode(o->p, o->pn);
  if (o->p != NULL) free_D_Parser(o->p);
  o->pn = NULL; o->p = NULL;
}

/* the `statement` node under translation_unit */
static D_ParseNode *csStmtNode(D_ParseNode *pn) {
  D_ParseNode *s = pn;
  while (d_get_number_of_children(s) == 1) s = d_get_child(s, 0);
  return s;
}

/* Is this statement `lhs <op> rhs`?  Otherwise it is a bare call such as
   dvid(3, 4), which ..rxOpt() renders with machine B alone -- machine A is
   only ever applied to an assignment's right-hand side
   (R/rxOptExpr.R:344). */
static int csStmtIsAssign(D_ParseNode *s) {
  return d_get_number_of_children(s) >= 3 &&
    !strcmp(csNodeName(csUnwrap(d_get_child(s, 1))), "assign_op");
}

/* `<-` becomes `=`; `=` and `~` are kept (R/rxOptExpr.R:355-357) */
static const char *csAssignOp(csCtx *c, D_ParseNode *s) {
  const char *op = csNodeText(csArena(c), csUnwrap(d_get_child(s, 1)));
  if (!strcmp(op, "<-")) return "=";
  return op;
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


static void csCountStmt(csCtx *c, const char *line, int idx) {
  csParse o = {NULL, NULL};
  D_ParseNode *s;
  c->stmt = idx; c->pos = 0; c->arena.failed = 0;
  if (!csParseOpen(&o, line)) {
    c->failWhy = "parse"; c->failLine = line;
    c->arena.failed = 1; csParseClose(&o); return;
  }
  s = csStmtNode(o.pn);
  if (csStmtIsAssign(s)) {
    (void) csLhs(c, d_get_child(s, 0));            /* validates the lhs form */
    if (c->arena.failed) {
      if (c->failWhy == NULL) c->failWhy = "lhs";
    } else {
      (void) csA(c, d_get_child(s, 2));
      if (c->arena.failed && c->failWhy == NULL) c->failWhy = "rhs";
    }
  }
  if (c->arena.failed) { c->anyFail = 1; c->failLine = line; }
  csParseClose(&o);
}

/* ------------------------------------------------------------------ pass 2 */
static const char *csOptStmt(csCtx *c, const char *line, int idx) {
  /* Initialized here, not by csParseOpen: the `!c->arena.failed &&` below can
     short circuit past the open and still reach csParseClose. */
  csParse o = {NULL, NULL}, o2 = {NULL, NULL};
  D_ParseNode *s;
  const char *out = NULL;
  c->stmt = idx; c->pos = 0; c->nused = 0; c->arena.failed = 0;
  if (!csParseOpen(&o, line)) { c->arena.failed = 1; c->anyFail = 1;
    csParseClose(&o); return NULL; }
  s = csStmtNode(o.pn);
  if (csStmtIsAssign(s)) {
    const char *lhs = csLhs(c, d_get_child(s, 0));
    const char *op = csAssignOp(c, s);
    const char *keyed = csA(c, d_get_child(s, 2));   /* machine A */
    /* csParseOpen() allocates the D_Parser BEFORE it can fail, so the failing
       path has to close it too */
    if (!c->arena.failed && csParseOpen(&o2, keyed)) {
      D_ParseNode *s2 = csStmtNode(o2.pn);
      const char *rhs = NULL;
      if (csStmtIsAssign(s2)) c->arena.failed = 1;
      else rhs = csB(c, d_get_child(s2, 0));         /* machine B */
      if (!c->arena.failed) out = seCat(csArena(c), lhs, op, rhs, NULL, NULL, NULL);
      csParseClose(&o2);
    } else {
      csParseClose(&o2);
      c->arena.failed = 1;
    }
  } else {
    out = csB(c, d_get_child(s, 0));                 /* machine B alone */
  }
  csParseClose(&o);
  if (c->arena.failed) { c->anyFail = 1; c->failLine = line; }
  return c->arena.failed ? NULL : out;
}

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

/* pass 1: count every statement's subexpressions, then merge the per-thread
   maps.  min(firstSeen) on merge is what makes the naming independent of how
   the statements were distributed across threads. */
static int csCountPhase(csRun *r) {
  int t, ok = 1;
  R_xlen_t n = r->n;
  csCtx *ctxs = r->ctxs;
  const char **in = r->in;
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
      for (j = 0; j < n; j++) csCountStmt(&ctxs[me], in[j], (int) j);
    }
  }
  for (t = 0; t < r->nthr; t++) {
    if (!ctxs[t].anyFail) continue;
    ok = 0;
    if (csDebug()) {                    /* printed HERE, outside the region */
      REprintf("rxCse: declined (%s) [%s]\n",
               ctxs[t].failWhy == NULL ? "?" : ctxs[t].failWhy,
               ctxs[t].failLine == NULL ? "?" : ctxs[t].failLine);
    }
  }
  if (!ok) return 0;
  if (!csMapInit(&r->all, 4096)) return 0;
  for (t = 0; t < r->nthr; t++) if (!csMapMerge(&r->all, r->ctxs[t].count)) return 0;
  return 1;
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

/* pass 2: the same walk, substituting the names in, recording which
   temporaries each statement used */
static int csRewritePhase(csRun *r) {
  int t;
  R_xlen_t i, n = r->n;
  csCtx *ctxs = r->ctxs;
  const char **in = r->in;
  const char **out;
  const char ***usedBy;
  int *nUsedBy;
  r->out = (const char**) calloc((size_t) n, sizeof(char*));
  r->usedBy = (const char***) calloc((size_t) n, sizeof(char**));
  r->nUsedBy = (int*) calloc((size_t) n, sizeof(int));
  if (r->out == NULL || r->usedBy == NULL || r->nUsedBy == NULL) return 0;
  out = r->out; usedBy = r->usedBy; nUsedBy = r->nUsedBy;
  for (t = 0; t < r->nthr; t++) {
    ctxs[t].rep = &r->rep;
    ctxs[t].repNames = r->repNames;
    ctxs[t].usedCap = r->ncand;
    ctxs[t].used = (const char**) calloc((size_t) r->ncand, sizeof(char*));
    if (ctxs[t].used == NULL) return 0;
  }
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
      for (j = 0; j < n; j++) {
        const char *o = csOptStmt(&ctxs[me], in[j], (int) j);
        out[j] = o;
        if (o != NULL && ctxs[me].nused > 0) {
          int u;
          usedBy[j] = (const char**) malloc(sizeof(char*) * (size_t) ctxs[me].nused);
          if (usedBy[j] == NULL) {
            /* Must NOT be silent: this list is what places each
               `rx_expr_i~...` definition before its first use, so losing it
               would emit a statement referencing a temporary never defined. */
            ctxs[me].anyFail = 1;
            ctxs[me].failWhy = "out of memory recording temporaries";
          } else {
            for (u = 0; u < ctxs[me].nused; u++) usedBy[j][u] = ctxs[me].used[u];
            nUsedBy[j] = ctxs[me].nused;
          }
        }
      }
    }
  }
  for (i = 0; i < n; i++) if (out[i] == NULL) return 0;
  for (t = 0; t < r->nthr; t++) if (ctxs[t].anyFail) return 0;
  return 1;
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

/* assemble, emitting each definition immediately before the first statement
   that uses it, ascending index, once globally.  Returns the text, which the
   caller owns, or NULL. */
static char *csEmitPhase(csRun *r) {
  csBuf b;
  char *emitted = (char*) calloc((size_t) r->ncand, 1);
  R_xlen_t i;
  int ok = 1;
  memset(&b, 0, sizeof(b));
  if (emitted == NULL) return NULL;
  for (i = 0; ok && i < r->n; i++) {
    int j;
    for (j = 0; j < r->ncand; j++) {              /* ascending index */
      int u, hit = 0;
      const char *body;
      csCtx dc;
      if (emitted[j]) continue;
      for (u = 0; u < r->nUsedBy[i]; u++)
        if (r->usedBy[i][u] == r->cand[j].name) { hit = 1; break; }
      if (!hit) continue;
      body = csDefBody(&dc, r->cand[j].reduced);
      if (body == NULL ||
          !csBufAdd(&b, r->cand[j].name) || !csBufAdd(&b, "~") ||
          !csBufAdd(&b, body) || !csBufAdd(&b, "\n")) ok = 0;
      seArenaFree(&dc.arena);
      if (!ok) break;
      emitted[j] = 1;
    }
    if (!ok) break;
    if (!csBufAdd(&b, r->out[i])) { ok = 0; break; }
    if (i + 1 < r->n && !csBufAdd(&b, "\n")) { ok = 0; break; }
  }
  free(emitted);
  if (ok) return b.s;
  free(b.s);
  return NULL;
}

extern "C" SEXP _rxode2_rxCse(SEXP linesVec) {
  R_xlen_t n, i;
  const char **in;
  int nthr = 1;
  csRun r;
  char *outText = NULL;
  double _t0 = csNow();
  SEXP ret;

  if (TYPEOF(linesVec) != STRSXP) Rf_error("%s", "'linesVec' must be character");
  n = Rf_xlength(linesVec);
  if (n == 0) return Rf_ScalarString(NA_STRING);

  /* CHAR() before any thread starts, with R_alloc so an error cannot leak it
     (the rule from src/seBatch.h:88-104) */
  in = (const char**) R_alloc((size_t) n, sizeof(char*));
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(linesVec, i);
    if (el == NA_STRING) return Rf_ScalarString(NA_STRING);
    in[i] = CHAR(el);
  }

  /* Debugging runs single threaded so the reported decline is deterministic --
     with several threads you get whichever one happened to fail.  Safety does
     not depend on this: nothing in the regions touches the R API, the reason
     is recorded and printed afterwards. */
  if (n >= CS_MIN_PARALLEL && !csDebug()) {
    nthr = getRxThreads((int64_t) n, true);
    if (nthr < 1) nthr = 1;
    if ((R_xlen_t) nthr > n) nthr = (int) n;
  }

  csRunInit(&r, n, in, nthr);
  if (csRunAlloc(&r) && csCountPhase(&r)) {
    CS_PHASE("count");
    if (csSelectPhase(&r)) {
      CS_PHASE("select");
      if (csReducePhase(&r)) {
        CS_PHASE("reduce");
        if (csRewritePhase(&r)) {
          CS_PHASE("rewrite");
          outText = csEmitPhase(&r);
          CS_PHASE("emit");
        }
      }
    }
  }
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
