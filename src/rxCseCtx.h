/*
 * rxCseCtx.h -- the walk context, and the two small text predicates the R
 * reference uses to decide what counts as a number.
 *
 * seCtx is the FIRST member so a csCtx* can be handed to the arena helpers in
 * seFromSEarena.h, which only ever touch head/failed.  seCtx has no user slot
 * (src/seFromSEarena.h:35) and adding one would change a struct two other
 * translators share, so embedding is the way in.
 */
#ifndef __RX_CSE_CTX_H__
#define __RX_CSE_CTX_H__

#include "seFromSEarena.h"
#include "rxCseIndex.h"

typedef struct csCtx {
  seCtx arena;          /* MUST be first */
  csMap *count;         /* pass 1: text -> {count, firstSeen} */
  csMap *rep;           /* pass 2: text -> index of the rx_expr_i that replaces it */
  const char **repNames; /* pass 2: index -> "rx_expr_i" */
  int stmt;             /* statement index, the high half of firstSeen */
  int pos;              /* post-order position, the low half */
  const char **used;    /* pass 2: the rx_expr_i this statement referenced */
  int nused;
  int usedCap;
  int anyFail;          /* sticky: did ANY statement decline in this context */
  const char *failWhy;  /* and why, reported AFTER the parallel region -- the
                           R API (REprintf) must not be reached from inside it */
  const char *failLine;
} csCtx;

#define csArena(c) (&(c)->arena)

/* `regNum` from R/dsl.R:44-47, anchored, with the surrounding whitespace that
   .rxOptBin allows (R/rxOptExpr.R:38-42): an optional sign, then an integer, a
   float, or a bare exponent form.  This is what decides "both operands are
   numeric, so render it but do not count it". */
static inline int csIsNum(const char *s) {
  const char *p = s;
  int digits = 0, dot = 0;
  if (p == NULL) return 0;
  while (*p == ' ' || *p == '\t') p++;
  if (*p == '-') p++;
  if (*p == '\0') return 0;
  while ((*p >= '0' && *p <= '9') || (*p == '.' && !dot)) {
    if (*p == '.') dot = 1; else digits++;
    p++;
  }
  if (digits == 0) return 0;
  if (*p == 'e' || *p == 'E') {
    p++;
    if (*p == '+' || *p == '-') p++;
    if (!(*p >= '0' && *p <= '9')) return 0;
    while (*p >= '0' && *p <= '9') p++;
  }
  while (*p == ' ' || *p == '\t') p++;
  return *p == '\0';
}

/* .rxModOperand (R/rxPrune.R:43-50): a bare number or a bare name is left
   alone, anything else is parenthesized -- including something ALREADY
   parenthesized, which is why `(b+1)%%(c+1)` renders `((b+1))%%((c+1))`. */
static inline int csModBare(const char *s) {
  const char *p = s;
  if (p == NULL || *p == '\0') return 0;
  if ((*p >= '0' && *p <= '9') || *p == '.') {
    int dot = 0, digits = 0;
    while ((*p >= '0' && *p <= '9') || (*p == '.' && !dot)) {
      if (*p == '.') dot = 1; else digits++;
      p++;
    }
    if (digits == 0) return 0;
    if (*p == 'e' || *p == 'E') {
      p++;
      if (*p == '+' || *p == '-') p++;
      if (!(*p >= '0' && *p <= '9')) return 0;
      while (*p >= '0' && *p <= '9') p++;
    }
    return *p == '\0';
  }
  if (!((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || *p == '.' || *p == '_'))
    return 0;
  p++;
  while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
         (*p >= '0' && *p <= '9') || *p == '.' || *p == '_') p++;
  return *p == '\0';
}

static inline const char *csModOperand(csCtx *c, const char *s) {
  if (csModBare(s)) return s;
  return seCat(csArena(c), "(", s, ")", NULL, NULL, NULL);
}

/* is the text an integerish exponent >= 2?  This is what decides the ^n
   expansion in .rxOptBin (R/rxOptExpr.R:45-47): checkmate::checkIntegerish on
   as.numeric(text), lower = 2.  as.numeric("(-1)") is NA, so a parenthesized
   exponent never expands; nor does "2.5". */
/* returns 1 to expand, 0 to leave alone, -1 to DECLINE (integerish but far too
   large to expand -- R has no upper limit and would expand it, so quietly
   leaving `x^5000` alone would differ from the R walker) */
static inline int csIntPow(const char *s, long *out) {
  const char *p = s;
  long v = 0;
  int digits = 0;
  if (p == NULL) return 0;
  while (*p == ' ') p++;
  while (*p >= '0' && *p <= '9') { v = v * 10 + (*p - '0'); digits++; p++; if (v > 4096) return -1; }
  if (digits == 0) return 0;
  if (*p == '.') { p++; while (*p == '0') p++; }   /* "3.0" is integerish */
  while (*p == ' ') p++;
  if (*p != '\0') return 0;
  if (v < 2) return 0;
  *out = v;
  return 1;
}

#endif /* __RX_CSE_CTX_H__ */
