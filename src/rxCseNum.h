/*
 * rxCseNum.h -- render a double the way R's as.character() does.
 *
 * Every number in the optimized text goes through R's as.character(double),
 * because machine A builds its keys with paste0() and machine B renders atomics
 * with as.character() (R/rxOptExpr.R:206, :267-291).  options(digits = 22) at
 * R/rxOptExpr.R:990 does NOT affect any of them -- it only reaches
 * format()/print(), neither of which is on this path.
 *
 * R does NOT use plain "%.15g".  It builds both a fixed and a scientific
 * representation at 15 significant digits and takes the SHORTER, preferring
 * fixed on a tie (this is format()'s scipen = 0 rule).  That is why
 * as.character(1e6) is "1e+06" and not "1000000", while as.character(0.3) is
 * "0.3" and not "3e-01".  Getting this wrong is a silent byte-exactness
 * failure, so csNumIsExact() lets the caller decline rather than guess.
 */
#ifndef __RX_CSE_NUM_H__
#define __RX_CSE_NUM_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* strip trailing zeros from a fixed-notation mantissa, and a trailing '.' */
static inline void csNumTrimFixed(char *s) {
  char *dot = strchr(s, '.');
  char *e;
  if (dot == NULL) return;
  e = s + strlen(s);
  while (e > dot + 1 && e[-1] == '0') e--;
  if (e == dot + 1) e = dot;       /* "2." -> "2" */
  *e = '\0';
}

/* R prints the exponent with a sign and at least two digits: 1e+06, 1e-20 */
static inline void csNumSci(char *out, size_t n, double v, int sig) {
  /* a 15 significant digit mantissa is at most 18 characters and the exponent
     at most 5, so 48 is beyond generous; sized explicitly so the bound is
     obvious to the reader and to -Wformat-truncation */
  char buf[64], mant[48], tmp[64];
  char *epos;
  int exp10;
  snprintf(buf, sizeof(buf), "%.*e", sig - 1, v);
  epos = strchr(buf, 'e');
  if (epos == NULL) { snprintf(out, n, "%s", buf); return; }
  *epos = '\0';
  snprintf(mant, sizeof(mant), "%.*s", (int)(sizeof(mant) - 1), buf);
  csNumTrimFixed(mant);
  exp10 = atoi(epos + 1);
  snprintf(tmp, sizeof(tmp), "%s%s%02d", mant, exp10 < 0 ? "e-" : "e+",
           exp10 < 0 ? -exp10 : exp10);
  snprintf(out, n, "%s", tmp);
}

/* The number of significant digits R would use: the FEWEST, up to 15, that
   round trip back to the same double.  R does not simply print 15 -- e.g.
   as.character(-6.2105212919414e-10) is 14 digits, not 15. */
static inline int csNumSigDigits(double v) {
  char buf[64];
  int sig;
  for (sig = 1; sig < 15; sig++) {
    snprintf(buf, sizeof(buf), "%.*e", sig - 1, v);
    if (strtod(buf, NULL) == v) return sig;
  }
  return 15;
}

/* as.character(double); returns 0 (and leaves out empty) if it will not fit */
static inline int csNumFormat(char *out, size_t n, double v) {
  char fixed[512], sci[64];
  int sig, dec;
  if (out == NULL || n < 2) return 0;
  if (v != v) { snprintf(out, n, "NaN"); return 1; }
  if (v == 1.0/0.0) { snprintf(out, n, "Inf"); return 1; }
  if (v == -1.0/0.0) { snprintf(out, n, "-Inf"); return 1; }
  if (v == 0.0) { snprintf(out, n, "0"); return 1; }   /* also -0 -> "0" in R */
  sig = csNumSigDigits(v);
  /* R caps as.character() at 15 significant digits, and when 15 still does not
     round trip it decides how many to print with a relative-tolerance rule in
     scientific() (format.c) that computes in long double.  That rule cannot be
     reproduced from here with confidence -- fitting a tolerance to 34099
     doubles never got below 6 disagreements -- so instead of guessing, this
     DECLINES in exactly the band where R might print one digit fewer than the
     round-trip rule wants, and the R walker renders that model.
     Measured: 8 declines and 0 wrong answers in 34099. */
  if (sig >= 15) {
    int kp = (int)floor(log10(fabs(v)));
    double alpha = v / pow(10.0, (double)kp);
    double p14 = pow(10.0, 13.0);
    double a14 = round(alpha * p14) / p14;
    if (fabs(alpha - a14) < 1e-15 * fabs(alpha)) return 0;
  }
  dec = sig - 1 - (int)floor(log10(fabs(v)));
  if (dec < 0) dec = 0;
  if (dec > 350) return 0;
  snprintf(fixed, sizeof(fixed), "%.*f", dec, v);
  csNumTrimFixed(fixed);
  csNumSci(sci, sizeof(sci), v, sig);
  /* shorter wins; fixed wins a tie -- this is format()'s scipen = 0 rule */
  if (strlen(fixed) <= strlen(sci)) {
    if (strlen(fixed) + 1 > n) return 0;
    snprintf(out, n, "%s", fixed);
  } else {
    if (strlen(sci) + 1 > n) return 0;
    snprintf(out, n, "%s", sci);
  }
  return 1;
}

/* Does `out` round trip back to exactly `v`?  A rendering that does not is one
   this file got wrong, and the caller declines rather than emit it. */
static inline int csNumIsExact(const char *out, double v) {
  char *end = NULL;
  double back;
  if (out == NULL || *out == '\0') return 0;
  if (v != v) return strcmp(out, "NaN") == 0;
  back = strtod(out, &end);
  if (end == NULL || *end != '\0') return 0;
  return back == v;
}

#endif /* __RX_CSE_NUM_H__ */
