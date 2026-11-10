/*
 * seFromSEarena.h -- bump allocator and translation context for seFromSE.c.
 *
 * Holds every string the emitter builds so the recursive walk can return
 * `const char *` with no ownership bookkeeping, and so nothing calls R's
 * allocator (which is not thread safe) inside the walk.  Freed in one go when
 * the batch finishes.
 */
#ifndef __SE_FROM_SE_ARENA_H__
#define __SE_FROM_SE_ARENA_H__

/* ------------------------------------------------------------------ arena --
   Bump allocator so the recursive emitter can return strings without any
   ownership bookkeeping, and so nothing calls R's allocator (not thread safe)
   inside the walk. */
#define SE_BLK (1 << 14)

typedef struct seBlk {
  struct seBlk *next;
  size_t used, cap;
  char *mem;
} seBlk;

/* One registered derivative: rxode2parseD()[[name]][[which]], reduced to a
   template by .rxDtemplates() so rendering it is substitution rather than a
   call back into R.  The char* point into the caller's protected STRSXP and
   are read-only for the life of the batch, which keeps the walk free of the
   R API. */
typedef struct {
  const char *name;
  int which;
  const char *tmpl;
  int kind;            /* 1 template, 2 NULL entry (finite difference) */
} seDeriv;

/* .SEeq (c(.rxSEeq, .rxSEeqUsr())): the functions .rxFromSE() emits through
   its generic call branch, with the argument count each one takes. */
typedef struct {
  const char *name;
  int nargs;
} seEq;

/* Open-addressing string -> index table.  Built once per .Call on the main
   thread, read-only afterwards, so the walk finds a name with one hash and one
   strcmp rather than scanning a list. */
typedef struct {
  int cap;             /* power of two */
  int *slot;           /* index into the keyed array, -1 = empty */
} seHash;

static inline unsigned int seHashStr(const char *s) {
  unsigned int h = 2166136261u;          /* FNV-1a */
  while (*s) { h ^= (unsigned char) *s++; h *= 16777619u; }
  return h;
}

/* Everything R hands the walk that does not change during a batch */
typedef struct {
  const seDeriv *derivs;   /* sorted by name: each name's rows are adjacent */
  int nderivs;
  seHash dHash;            /* name -> first row in derivs */
  const seEq *eq;
  int neq;
  seHash eqHash;           /* name -> row in eq */
  const char **rxdName;    /* ls(rxode2parseD()) */
  const int *rxdLen;       /* length() of each entry */
  int nrxd;
  seHash rxdHash;
  const char **baseFns;    /* functions callable in baseenv() */
  int nbase;
  seHash baseHash;
} seTables;

/* Why the walk declined, so the declines can be counted and the most common
   ported first.  Only the first reason is kept. */
typedef enum {
  SE_WHY_NONE = 0,
  SE_WHY_PARSE,          /* the grammar did not accept the text */
  SE_WHY_FOLD,           /* R would fold a constant call this cannot */
  SE_WHY_LOG,            /* log()/lgamma() shape not reproduced */
  SE_WHY_TRIG,           /* sin/cos/tan pi-factor shape not reproduced */
  SE_WHY_DERIV,          /* Derivative() needing symengine or an R error */
  SE_WHY_NUMDER,         /* Derivative() finite-difference fallback */
  SE_WHY_SUBS,           /* Subs() shape not reproduced */
  SE_WHY_ARITY,          /* wrong argument count: R raises the message */
  SE_WHY_UNKNOWN_FN,     /* not a known function: R user function or error */
  SE_WHY_SYMBOL,         /* a symbol R raises on (NA, dosenum0, ...) */
  SE_WHY_OTHER,
  SE_WHY_N
} seWhy;

/* One active Subs(F, v, w): while F is walked, the symbol v stands for w's
   parse node -- R splices w's TREE into F, so the walk must see w wherever F
   has v, structure checks included.  Inactive while w itself is walked, as R
   does not substitute inside the replacement. */
typedef struct {
  const char *v;
  struct D_ParseNode *w;
  int active;
  /* the reparsed F text: only its symbols are substituted.  w's nodes live
     in another buffer, so a v inside w is never replaced again -- R splices
     w in once and does not walk into it (Subs(exp(x), x, x + 1)) */
  const char *buf;
  size_t len;
} seSub;

#define SE_MAX_SUBS 16

typedef struct {
  seBlk *head;
  int failed;          /* 1 = hand this expression back to the R walker */
  seSub subs[SE_MAX_SUBS];
  int nsubs;
  int why;             /* seWhy: the first reason the walk declined */
  int numDer;          /* .rxFromNumDer: 0 error, 1 forward, 2 central */
  const seTables *tab; /* NULL for rxToSE */
  /* rxToSE() into a symengine environment: the leaves the walk met, so R
     can bind them the way .rxToSENameOrAtomic() would (collect = 1) */
  int collect;
  const char **syms;
  int nsyms, msyms;
  int *thetas;
  int nthetas, mthetas;
  int *etas;
  int netas, metas;
  /* the call names R's walk pushes on ..curCall, and for each THETA/ETA
     above the joined stack it was read under (.rxToSESquareBracket) */
  const char *callStack[64];
  int ncall;
  const char **thetaFuns;
  int nthetaFuns, mthetaFuns;
  const char **etaFuns;
  int netaFuns, metaFuns;
} seCtx;

static seBlk *seBlkNew(size_t need) {
  size_t cap = need > SE_BLK ? need : SE_BLK;
  seBlk *b = (seBlk*) malloc(sizeof(seBlk));
  if (b == NULL) return NULL;
  b->mem = (char*) malloc(cap);
  if (b->mem == NULL) { free(b); return NULL; }
  b->used = 0; b->cap = cap; b->next = NULL;
  return b;
}

static void seArenaFree(seCtx *ctx) {
  seBlk *b = ctx->head;
  while (b != NULL) {
    seBlk *n = b->next;
    free(b->mem); free(b);
    b = n;
  }
  ctx->head = NULL;
}

static char *seAlloc(seCtx *ctx, size_t n) {
  seBlk *b = ctx->head;
  if (b == NULL || b->used + n > b->cap) {
    seBlk *nb = seBlkNew(n);
    if (nb == NULL) { ctx->failed = 1; return NULL; }
    nb->next = ctx->head; ctx->head = nb; b = nb;
  }
  char *p = b->mem + b->used;
  b->used += n;
  return p;
}

static const char *seDup(seCtx *ctx, const char *s, size_t n) {
  char *p = seAlloc(ctx, n + 1);
  if (p == NULL) return "";
  memcpy(p, s, n); p[n] = '\0';
  return p;
}

static inline const char *seStr(seCtx *ctx, const char *s) {
  return seDup(ctx, s, strlen(s));
}

/* concatenate up to 6 pieces */
static const char *seCat(seCtx *ctx, const char *a, const char *b,
                         const char *c, const char *d, const char *e,
                         const char *f) {
  size_t n = 0;
  const char *v[6]; int i, nv = 0;
  v[nv++] = a; v[nv++] = b; v[nv++] = c; v[nv++] = d; v[nv++] = e; v[nv++] = f;
  for (i = 0; i < nv; i++) if (v[i] != NULL) n += strlen(v[i]);
  char *p = seAlloc(ctx, n + 1);
  if (p == NULL) return "";
  char *q = p;
  for (i = 0; i < nv; i++) {
    if (v[i] == NULL) continue;
    size_t l = strlen(v[i]); memcpy(q, v[i], l); q += l;
  }
  *q = '\0';
  return p;
}

/* append to an arena-backed growable array */
static int seGrow(seCtx *ctx, void **arr, int *n, int *m, size_t sz) {
  if (*n < *m) return 1;
  int nm = (*m == 0) ? 16 : 2 * (*m);
  void *na = (void*) seAlloc(ctx, (size_t) nm * sz);
  if (na == NULL) return 0;
  if (*n > 0) memcpy(na, *arr, (size_t)(*n) * sz);
  *arr = na;
  *m = nm;
  return 1;
}

static inline const char *seFailWhy(seCtx *ctx, int why) {
  if (!ctx->failed) ctx->why = why;
  ctx->failed = 1;
  return "";
}

static inline const char *seFail(seCtx *ctx) {
  return seFailWhy(ctx, SE_WHY_OTHER);
}

#endif /* __SE_FROM_SE_ARENA_H__ */
