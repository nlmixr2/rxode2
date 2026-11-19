/*
 * seCache.h -- the translation cache shared by rxToSE() and rxFromSE().
 *
 * Symbolic-derivative builds translate the same text over and over: nlmixr2est
 * converts each rx_f1_/rx_f2_ line separately, and a line the C emitter
 * declines is translated by the R walker every time it recurs.  The cache
 * remembers input -> output for both directions.
 *
 * Lifetime ("session + state key"):
 *  - every lookup carries the state the output depends on (the stamp: numDer,
 *    the derivative/.SEeq table generation, the sumProd flags, promoteLinB);
 *    a different stamp clears the cache before it is used;
 *  - rxS() and rxToSE(envir =) start a new session and clear it, as does
 *    rxSymEngineCacheReset();
 *  - a byte cap clears it when exceeded.
 *
 * Only the main thread touches it: seRunBatch() looks every input up before
 * its parallel region and stores the results after it, so the region itself
 * only translates.
 */
#ifndef __SE_CACHE_H__
#define __SE_CACHE_H__

#define SE_DIR_FROM 0
#define SE_DIR_TO 1
/* where .rxSEstamp() puts options(scipen=) (0-based; part of the key) */
#define SE_STAMP_SCIPEN 5

/* Apply the stamp (INTSXP: key..., enabled, capMB) for direction `dir`:
   clears the cache when the key changed.  Returns 1 when caching is on. */
extern "C" int seCacheBegin(SEXP stamp);
/* the cached translation, or NULL */
extern "C" const char *seCacheGet(int dir, const char *key);
extern "C" void seCachePut(int dir, const char *key, const char *val);
/* count a translated element: why == 0 accepted, else the decline reason */
extern "C" void seStatsAdd(int dir, int why);
extern "C" void seStatsHit(int dir);

#endif /* __SE_CACHE_H__ */
