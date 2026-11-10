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
   fixed notation is used unless it is wider than scientific (by more than
   scipen) -- so 1e5 is
   "1e+05", 1e-4 is "1e-04", 123456 stays "123456" and 1234567890123456789
   prints every integer digit.  "%.15g" gets all of those wrong. */
/* options(scipen=), which as.character() honours: fixed notation wins
   unless it is more than scipen characters wider than scientific.  Set on
   the main thread at each .Call entry (seSetScipen), read-only after. */
static int seScipen = 0;

static void seSetScipen(SEXP stamp, int at) {
  if (TYPEOF(stamp) == INTSXP && Rf_xlength(stamp) > at &&
      INTEGER(stamp)[at] != NA_INTEGER) {
    seScipen = INTEGER(stamp)[at];
  } else {
    seScipen = 0;
  }
}

static void seDblFormat(double v, char *buf, size_t n) {
  char m[64];
  int nsig, kp, neg, rgt, mxsl, wF, wE;
  char *e, *p;
  if (ISNAN(v)) { snprintf(buf, n, "%s", "NaN"); return; }
  if (!R_FINITE(v)) { snprintf(buf, n, "%s", v > 0 ? "Inf" : "-Inf"); return; }
  neg = v < 0;
  if (v == 0.0) {
    v = 0.0;                     /* -0 prints as 0, as R prints it */
    /* one significant digit at exponent 0: "0", or "0e+00" when scipen is
       negative enough to prefer scientific */
    kp = 0;
    nsig = 1;
  } else {
    /* 15 significant digits, rounded exactly as printf rounds them; the
       exponent printf reports already accounts for rounding that widens */
    snprintf(m, sizeof(m), "%.14e", fabs(v));
    e = strchr(m, 'e');
    kp = atoi(e + 1);
    /* significant digits left once trailing zeros of the mantissa drop */
    p = e - 1;
    while (p > m && *p == '0') p--;
    nsig = (*p == '.') ? 1 : (int)(p - m);   /* "d.ddd": digits minus '.' */
  }
  rgt = nsig - kp - 1;
  if (rgt < 0) rgt = 0;
  mxsl = kp >= 0 ? kp + 1 : 1;
  wF = neg + mxsl + rgt + (rgt != 0);
  wE = neg + (nsig > 1) + (nsig - 1) + 4 + ((kp >= 100 || kp <= -100) ? 2 : 1);
  if (wF <= wE + seScipen) {
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
