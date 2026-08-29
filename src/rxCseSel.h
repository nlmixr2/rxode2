/*
 * rxCseSel.h -- choose, order and name the candidates, then rewrite each in
 * terms of the shorter ones.
 *
 * The filters and their ORDER are R/rxOptExpr.R:1000-1015; the ordering is
 * order(nchar(...)) over a list whose names are in first-encounter order, which
 * is stable, so ties break by first encounter -- that is what `firstSeen`
 * reconstructs after a parallel count.
 *
 * The rewrite is rxOptRep_ (src/rxOptRep.cpp), and its two odd properties are
 * reproduced on purpose: only the FIRST occurrence is replaced, and a match
 * only counts when both sides are a string edge or one of ()+*_/-^=<>&| .
 * The consequence -- a candidate containing the same sub-candidate twice
 * reduces to text machine A can never build, so it is defined but never used --
 * is real behavior that the fixture pins.
 */
#ifndef __RX_CSE_SEL_H__
#define __RX_CSE_SEL_H__

#include "rxCseCtx.h"

static inline int csIsBoundary(char ch) {
  return ch == '(' || ch == ')' || ch == '+' || ch == '*' || ch == '/' ||
    ch == '-' || ch == '^' || ch == '=' || ch == '<' || ch == '>' ||
    ch == '&' || ch == '|';
}

/* replace1 (src/rxOptRep.cpp:9-69): first occurrence only, both boundaries */
static inline int csReplace1(char *str, size_t cap, const char *from, const char *to) {
  char *at = strstr(str, from);
  size_t fl = strlen(from), tl = strlen(to), sl = strlen(str);
  size_t off;
  if (at == NULL) return 0;
  off = (size_t)(at - str);
  if (off > 0 && !csIsBoundary(str[off - 1])) return 0;
  if (off + fl != sl && !csIsBoundary(str[off + fl])) return 0;
  if (sl - fl + tl + 1 > cap) return 0;
  memmove(at + tl, at + fl, sl - off - fl + 1);
  memcpy(at, to, tl);
  return 1;
}

/* is the text a bare THETA[n] / ETA[n]? */
static inline int csIsThetaEta(const char *s) {
  const char *p = s;
  if (!strncmp(p, "THETA[", 6)) p += 6;
  else if (!strncmp(p, "ETA[", 4)) p += 4;
  else return 0;
  if (!(*p >= '0' && *p <= '9')) return 0;
  while (*p >= '0' && *p <= '9') p++;
  return p[0] == ']' && p[1] == '\0';
}

typedef struct csCand {
  const char *key;      /* the text machine A builds */
  char *reduced;        /* the same, with shorter candidates substituted in */
  char *name;           /* rx_expr_<i> */
  uint64_t firstSeen;
  size_t len;
} csCand;

static inline int csCandCmp(const void *a, const void *b) {
  const csCand *x = (const csCand*) a, *y = (const csCand*) b;
  if (x->len != y->len) return x->len < y->len ? -1 : 1;
  if (x->firstSeen != y->firstSeen) return x->firstSeen < y->firstSeen ? -1 : 1;
  return 0;
}

#endif /* __RX_CSE_SEL_H__ */
