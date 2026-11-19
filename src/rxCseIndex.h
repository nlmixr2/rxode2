/*
 * rxCseIndex.h -- the count map, and what makes the parallel count deterministic.
 *
 * rxOptExpr() counts subexpressions in a named R list and looks them up with
 * [[text]], which is a linear scan -- O(k^2) in the number of distinct
 * subexpressions, and k on a second-order sensitivity model is enormous.  This
 * is the same map as an open-addressing hash: O(1) per lookup.
 *
 * DETERMINISM is the reason `firstSeen` exists.  rxOptExpr names candidates in
 * ascending nchar of their text, ties broken by first encounter
 * (R/rxOptExpr.R:1001, order() being stable over a list whose names are in
 * insertion order).  Counting per line in parallel destroys insertion order, so
 * each entry carries the position where it was FIRST seen -- statement index in
 * the high bits, post-order position within the statement in the low bits -- and
 * merging takes the minimum.  min() is associative and commutative, so the
 * result cannot depend on how many threads ran or in what order they finished.
 */
#ifndef __RX_CSE_INDEX_H__
#define __RX_CSE_INDEX_H__

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

/* statement index in the high 32 bits, post-order position in the low 32 */
#define CS_FIRST_SEEN(stmt, pos) \
  ((((uint64_t)(uint32_t)(stmt)) << 32) | (uint64_t)(uint32_t)(pos))

typedef struct csEntry {
  const char *key;      /* into an arena that outlives the map */
  uint64_t firstSeen;
  int count;
} csEntry;

typedef struct csMap {
  csEntry *e;
  int n;                /* capacity, a power of two */
  int used;
} csMap;

static inline uint32_t csHash(const char *s) {
  uint32_t h = 2166136261u;
  while (*s != '\0') { h ^= (uint32_t)(unsigned char)(*s++); h *= 16777619u; }
  return h;
}

static inline int csMapInit(csMap *m, int cap) {
  int i;
  if (cap < 16) cap = 16;
  m->e = (csEntry*) malloc(sizeof(csEntry) * (size_t) cap);
  if (m->e == NULL) { m->n = 0; m->used = 0; return 0; }
  m->n = cap; m->used = 0;
  for (i = 0; i < cap; i++) { m->e[i].key = NULL; m->e[i].count = 0; m->e[i].firstSeen = 0; }
  return 1;
}

static inline void csMapFree(csMap *m) {
  if (m->e != NULL) free(m->e);
  m->e = NULL; m->n = 0; m->used = 0;
}

static inline int csMapAdd(csMap *m, const char *key, int by, uint64_t firstSeen);

/* double and reinsert; keys are borrowed pointers so this only re-hashes */
static inline int csMapGrow(csMap *m) {
  csMap old = *m;
  int i;
  if (!csMapInit(m, old.n * 2)) { *m = old; return 0; }
  for (i = 0; i < old.n; i++) {
    if (old.e[i].key != NULL &&
        !csMapAdd(m, old.e[i].key, old.e[i].count, old.e[i].firstSeen)) {
      csMapFree(m); *m = old; return 0;
    }
  }
  csMapFree(&old);
  return 1;
}

/* add `by` to key's count; firstSeen keeps the EARLIER of the two */
static inline int csMapAdd(csMap *m, const char *key, int by, uint64_t firstSeen) {
  uint32_t mask, p;
  if (m->e == NULL && !csMapInit(m, 256)) return 0;
  if ((m->used + 1) * 10 > m->n * 7 && !csMapGrow(m)) return 0;
  mask = (uint32_t)(m->n - 1);
  p = csHash(key) & mask;
  while (m->e[p].key != NULL) {
    if (strcmp(m->e[p].key, key) == 0) {
      m->e[p].count += by;
      if (firstSeen < m->e[p].firstSeen) m->e[p].firstSeen = firstSeen;
      return 1;
    }
    p = (p + 1) & mask;
  }
  m->e[p].key = key;
  m->e[p].count = by;
  m->e[p].firstSeen = firstSeen;
  m->used++;
  return 1;
}

/* the entry for key, or NULL */
static inline csEntry *csMapGet(csMap *m, const char *key) {
  uint32_t mask, p;
  if (m->e == NULL || m->n == 0) return NULL;
  mask = (uint32_t)(m->n - 1);
  p = csHash(key) & mask;
  while (m->e[p].key != NULL) {
    if (strcmp(m->e[p].key, key) == 0) return &m->e[p];
    p = (p + 1) & mask;
  }
  return NULL;
}

/* fold `src` into `dst`; see the header note on why this is order independent */
static inline int csMapMerge(csMap *dst, const csMap *src) {
  int i;
  if (src->e == NULL) return 1;
  for (i = 0; i < src->n; i++) {
    if (src->e[i].key != NULL &&
        !csMapAdd(dst, src->e[i].key, src->e[i].count, src->e[i].firstSeen)) return 0;
  }
  return 1;
}

#endif /* __RX_CSE_INDEX_H__ */
