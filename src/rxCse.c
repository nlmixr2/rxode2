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
extern int getRxThreads(int64_t n, bool throttle);

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
SEXP _rxode2_rxCse(SEXP linesVec) {
  R_xlen_t n, i;
  const char **in;
  int nthr = 1, t, ncand = 0, ok = 1;
  csCtx *ctxs = NULL;
  csMap all;
  csCand *cand = NULL;
  const char **out = NULL;
  const char ***usedBy = NULL;
  const char **repNames = NULL;
  int *nUsedBy = NULL;
  SEXP ret = R_NilValue;
  char *outText = NULL;
  double _t0 = 0;
  if (csDebug()) _t0 = csNow();

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
     with several threads you get whichever one happened to fail.  Safety no
     longer depends on this: nothing in the region touches the R API, the
     reason is recorded and printed afterwards. */
  if (n >= CS_MIN_PARALLEL && !csDebug()) {
    nthr = getRxThreads((int64_t) n, true);
    if (nthr < 1) nthr = 1;
    if ((R_xlen_t) nthr > n) nthr = (int) n;
  }

  /* One context per thread, alive past the region so the merged map can point
     at keys in the thread arenas rather than copying them. */
  ctxs = (csCtx*) calloc((size_t) nthr, sizeof(csCtx));
  if (ctxs == NULL) Rf_error("%s", "could not allocate the CSE contexts");
  for (t = 0; t < nthr; t++) {
    ctxs[t].count = (csMap*) calloc(1, sizeof(csMap));
    if (ctxs[t].count == NULL || !csMapInit(ctxs[t].count, 1024)) ok = 0;
  }

  if (ok) {
#ifdef _OPENMP
#pragma omp parallel num_threads(nthr)
#endif
    {
      int me = 0;
#ifdef _OPENMP
      me = omp_get_thread_num();
#endif
      if (me < nthr) {
        R_xlen_t j;
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
        for (j = 0; j < n; j++) csCountStmt(&ctxs[me], in[j], (int) j);
      }
    }
    for (t = 0; t < nthr; t++) {
      if (!ctxs[t].anyFail) continue;
      ok = 0;
      if (csDebug()) {                      /* printed HERE, outside the region */
        REprintf("rxCse: declined (%s) [%s]\n",
                 ctxs[t].failWhy == NULL ? "?" : ctxs[t].failWhy,
                 ctxs[t].failLine == NULL ? "?" : ctxs[t].failLine);
      }
    }
  }
  CS_PHASE("count");

  memset(&all, 0, sizeof(all));
  if (ok && !csMapInit(&all, 4096)) ok = 0;
  for (t = 0; ok && t < nthr; t++) if (!csMapMerge(&all, ctxs[t].count)) ok = 0;

  CS_PHASE("merge");
  /* ---- select, order, name, and reduce the candidates ------------------- */
  if (ok) {
    int j;
    cand = (csCand*) calloc((size_t) (all.used + 1), sizeof(csCand));
    if (cand == NULL) ok = 0;
    for (j = 0; ok && j < all.n; j++) {
      csEntry *e = &all.e[j];
      if (e->key == NULL || e->count <= 1) continue;
      if (csIsNum(e->key) || csIsThetaEta(e->key) || e->key[0] == '-') continue;
      cand[ncand].key = e->key;
      cand[ncand].firstSeen = e->firstSeen;
      cand[ncand].len = strlen(e->key);
      ncand++;
    }
    if (ok && ncand > 0) qsort(cand, (size_t) ncand, sizeof(csCand), csCandCmp);
    if (csDebug()) {
      int d;
      for (d = 0; d < ncand && d < 30; d++)
        REprintf("  cand %3d len=%2d stmt=%u pos=%u  [%s]\n", d,
                 (int) cand[d].len, (unsigned)(cand[d].firstSeen >> 32),
                 (unsigned)(cand[d].firstSeen & 0xffffffffu), cand[d].key);
    }
  }
  if (ok && ncand == 0) ok = 0;      /* nothing to do: let R take the exit */

  if (ok) {
    int j, k;
    csMap rep;
    memset(&rep, 0, sizeof(rep));
    if (!csMapInit(&rep, 1024)) ok = 0;
    repNames = (const char**) calloc((size_t) ncand, sizeof(char*));
    if (repNames == NULL) ok = 0;
    for (j = 0; ok && j < ncand; j++) {
      size_t cap = cand[j].len + 64;
      cand[j].reduced = (char*) malloc(cap);
      cand[j].name = (char*) malloc(32);
      if (cand[j].reduced == NULL || cand[j].name == NULL) { ok = 0; break; }
      memcpy(cand[j].reduced, cand[j].key, cand[j].len + 1);
      snprintf(cand[j].name, 32, "rx_expr_%d", j);
      for (k = 0; k < j; k++) {
        if (csReplace1(&cand[j].reduced, &cap, cand[k].reduced, cand[k].name) < 0) {
          ok = 0; break;                 /* out of memory: decline, never guess */
        }
      }
      if (!ok) break;
      repNames[j] = cand[j].name;
      /* first wins on a duplicate reduced text, matching R's list, and it
         keeps the stored index exact (csMapAdd would SUM on a re-add) */
      if (csMapGet(&rep, cand[j].reduced) == NULL &&
          !csMapAdd(&rep, cand[j].reduced, j, 0)) { ok = 0; break; }
    }

    CS_PHASE("reduce");
    /* ---- pass 2 ------------------------------------------------------- */
    if (ok) {
      out = (const char**) calloc((size_t) n, sizeof(char*));
      usedBy = (const char***) calloc((size_t) n, sizeof(char**));
      nUsedBy = (int*) calloc((size_t) n, sizeof(int));
      if (out == NULL || usedBy == NULL || nUsedBy == NULL) ok = 0;
    }
    for (t = 0; ok && t < nthr; t++) {
      ctxs[t].rep = &rep;
      ctxs[t].repNames = repNames;
      ctxs[t].usedCap = ncand;
      ctxs[t].used = (const char**) calloc((size_t) ncand, sizeof(char*));
      if (ctxs[t].used == NULL) ok = 0;
    }
    if (ok) {
#ifdef _OPENMP
#pragma omp parallel num_threads(nthr)
#endif
      {
        int me = 0;
#ifdef _OPENMP
        me = omp_get_thread_num();
#endif
        if (me < nthr) {
          R_xlen_t j2;
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
          for (j2 = 0; j2 < n; j2++) {
            const char *o = csOptStmt(&ctxs[me], in[j2], (int) j2);
            out[j2] = o;
            if (o != NULL && ctxs[me].nused > 0) {
              int u;
              usedBy[j2] = (const char**) malloc(sizeof(char*) * (size_t) ctxs[me].nused);
              if (usedBy[j2] == NULL) {
                /* Must NOT be silent: this list is what places each
                   `rx_expr_i~...` definition before its first use, so losing it
                   would emit a statement referencing a temporary that is never
                   defined. */
                ctxs[me].anyFail = 1;
                ctxs[me].failWhy = "out of memory recording temporaries";
              } else {
                for (u = 0; u < ctxs[me].nused; u++) usedBy[j2][u] = ctxs[me].used[u];
                nUsedBy[j2] = ctxs[me].nused;
              }
            }
          }
        }
      }
      for (i = 0; i < n; i++) if (out[i] == NULL) ok = 0;
      for (t = 0; t < nthr; t++) if (ctxs[t].anyFail) ok = 0;
    }

    CS_PHASE("rewrite");
    /* ---- emit: each definition immediately before its first use --------- */
    if (ok) {
      csBuf b; char *emitted = (char*) calloc((size_t) ncand, 1);
      memset(&b, 0, sizeof(b));
      if (emitted == NULL) ok = 0;
      for (i = 0; ok && i < n; i++) {
        int j2;
        for (j2 = 0; j2 < ncand; j2++) {           /* ascending index */
          int u, hit = 0;
          if (emitted[j2]) continue;
          for (u = 0; u < nUsedBy[i]; u++)
            if (usedBy[i][u] == cand[j2].name) { hit = 1; break; }
          if (!hit) continue;
          /* the body is machine B over the REDUCED key, re-parsed */
          { csParse od = {NULL, NULL}; csCtx dc; const char *body = NULL;
            memset(&dc, 0, sizeof(dc));
            if (csParseOpen(&od, cand[j2].reduced)) {
              D_ParseNode *sd = csStmtNode(od.pn);
              if (!csStmtIsAssign(sd)) body = csB(&dc, d_get_child(sd, 0));
            }
            csParseClose(&od);   /* also on the failing path */
            if (body == NULL || dc.arena.failed) { ok = 0; seArenaFree(&dc.arena); break; }
            if (!csBufAdd(&b, cand[j2].name) || !csBufAdd(&b, "~") ||
                !csBufAdd(&b, body) || !csBufAdd(&b, "\n")) ok = 0;
            seArenaFree(&dc.arena);
          }
          emitted[j2] = 1;
        }
        if (!ok) break;
        if (!csBufAdd(&b, out[i])) { ok = 0; break; }
        if (i + 1 < n && !csBufAdd(&b, "\n")) { ok = 0; break; }
      }
      free(emitted);
      /* b.s is handed to the caller below, AFTER teardown: Rf_mkChar allocates
         and can longjmp, and everything here is hand-managed memory that the
         teardown block would then never reach. */
      if (ok) { outText = b.s; b.s = NULL; }
      free(b.s);
    }
    CS_PHASE("emit");
    csMapFree(&rep);
  }

  /* ---- teardown ------------------------------------------------------- */
  if (cand != NULL) {
    int j;
    for (j = 0; j < ncand; j++) { free(cand[j].reduced); free(cand[j].name); }
    free(cand);
  }
  if (usedBy != NULL) { for (i = 0; i < n; i++) free((void*) usedBy[i]); free(usedBy); }
  free(nUsedBy);
  free((void*) out);
  free((void*) repNames);
  csMapFree(&all);
  for (t = 0; t < nthr; t++) {
    if (ctxs[t].count != NULL) { csMapFree(ctxs[t].count); free(ctxs[t].count); }
    free((void*) ctxs[t].used);
    seArenaFree(&ctxs[t].arena);
  }
  free(ctxs);

  /* every other hand-managed allocation is released by here; outText is the
     last one, and R_UnwindProtect is what frees it if Rf_mkChar longjmps */
  if (outText == NULL) return Rf_ScalarString(NA_STRING);
  {
    csFinish fin; fin.text = outText;
    SEXP cont = PROTECT(R_MakeUnwindCont());
    ret = R_UnwindProtect(csFinishFun, &fin, csFinishClean, &fin, cont);
    /* csFinishClean() NULLs the pointer, so this frees it if the cleanup did
       not already run and is a no-op if it did.  Correct either way, which
       matters because whether R_UnwindProtect runs the cleanup on a NORMAL
       return is not something to take on faith. */
    free(fin.text);
    fin.text = NULL;
    UNPROTECT(1);
  }
  return ret;
}
