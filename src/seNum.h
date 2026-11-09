/*
 * seNum.h -- numbers written the way R writes them.
 *
 * Both translators print numbers R has parsed, and R prints them with
 * as.character(), so both use this.
 */
#ifndef __SE_NUM_H__
#define __SE_NUM_H__

#include "seFromSEarena.h"

/* as.character(<double>): R's formatReal() at 15 significant digits.  The
   fewest significant digits (<= 15) that reproduce the value are kept, then
   fixed notation is used unless it is wider than scientific -- so 1e5 is
   "1e+05", 1e-4 is "1e-04", 123456 stays "123456" and 1234567890123456789
   prints every integer digit.  "%.15g" gets all of those wrong. */
static void seDblFormat(double v, char *buf, size_t n) {
  char m[64];
  int nsig, kp, neg, rgt, mxsl, wF, wE;
  char *e, *p;
  if (ISNAN(v)) { snprintf(buf, n, "%s", "NaN"); return; }
  if (!R_FINITE(v)) { snprintf(buf, n, "%s", v > 0 ? "Inf" : "-Inf"); return; }
  if (v == 0.0) { snprintf(buf, n, "%s", "0"); return; }
  neg = v < 0;
  /* 15 significant digits, rounded exactly as printf rounds them; the
     exponent printf reports already accounts for rounding that widens */
  snprintf(m, sizeof(m), "%.14e", fabs(v));
  e = strchr(m, 'e');
  kp = atoi(e + 1);
  /* significant digits left once trailing zeros of the mantissa are dropped */
  p = e - 1;
  while (p > m && *p == '0') p--;
  nsig = (*p == '.') ? 1 : (int)(p - m);   /* "d.ddd": digits minus the '.' */
  rgt = nsig - kp - 1;
  if (rgt < 0) rgt = 0;
  mxsl = kp >= 0 ? kp + 1 : 1;
  wF = neg + mxsl + rgt + (rgt != 0);
  wE = neg + (nsig > 1) + (nsig - 1) + 4 + ((kp >= 100 || kp <= -100) ? 2 : 1);
  if (wF <= wE) {
    snprintf(buf, n, "%.*f", rgt, v);
  } else {
    snprintf(buf, n, "%.*e", nsig - 1, v);
  }
}

static const char *seDblToStr(seCtx *ctx, double v) {
  char buf[400];
  seDblFormat(v, buf, sizeof(buf));
  return seStr(ctx, buf);
}

#endif /* __SE_NUM_H__ */
