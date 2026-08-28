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

typedef struct {
  seBlk *head;
  int failed;          /* 1 = hand this expression back to the R walker */
  int numDer;          /* .rxFromNumDer: 0 error, 1 forward, 2 central */
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

static const char *seStr(seCtx *ctx, const char *s) {
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

static const char *seFail(seCtx *ctx) {
  ctx->failed = 1;
  return "";
}

#endif /* __SE_FROM_SE_ARENA_H__ */
