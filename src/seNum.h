/*
 * seNum.h -- numbers written the way R writes them.
 *
 * Both translators print numbers R has parsed, and R prints them with
 * as.character(), so both use this.  as.character(<double>) is
 * StringFromReal(): formatReal() at 15 digits, then EncodeRealDrop0().  The
 * digit count comes from R's scientific() (src/main/format.c), ported below
 * branch for branch -- it scales in long double where long double is wider
 * than double and in double where it is not (arm64 macOS), and the two can
 * keep a different number of digits.
 */
#ifndef __SE_NUM_H__
#define __SE_NUM_H__

#include <float.h>
#include "seFromSEarena.h"

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

#define SE_DIGITS 15 /* R_print.digits inside as.character(): DBL_DIG */

#if defined(__SIZEOF_LONG_DOUBLE__) && (__SIZEOF_LONG_DOUBLE__ > __SIZEOF_DOUBLE__)
#define SE_LDOUBLE 1
static const long double seTbl[] = {
  1e00L, 1e01L, 1e02L, 1e03L, 1e04L, 1e05L, 1e06L, 1e07L, 1e08L, 1e09L,
  1e10L, 1e11L, 1e12L, 1e13L, 1e14L, 1e15L, 1e16L, 1e17L, 1e18L, 1e19L,
  1e20L, 1e21L, 1e22L, 1e23L, 1e24L, 1e25L, 1e26L, 1e27L
};
#define SE_KP_MAX 27
#else
static const double seTbl[] = {
  1e00, 1e01, 1e02, 1e03, 1e04, 1e05, 1e06, 1e07, 1e08, 1e09,
  1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
  1e20, 1e21, 1e22
};
#define SE_KP_MAX 22
#endif

/* R's scientific() for r = |x| > 0: the power of ten, the significant
   digits kept (<= 15), and whether rounding widens the number */
static void seScientific(double r, int *kpower, int *nsig, int *widens) {
  double alpha;
  int kp = (int) floor(log10(r)) - SE_DIGITS + 1, j;
#ifdef SE_LDOUBLE
  long double rPrec = r;
  if (abs(kp) <= SE_KP_MAX) {
    if (kp > 0) rPrec /= seTbl[kp]; else if (kp < 0) rPrec *= seTbl[-kp];
  } else {
    rPrec /= powl(10.0L, (long double) kp);
  }
  if (rPrec < seTbl[SE_DIGITS - 1]) {
    rPrec *= 10.0;
    kp--;
  }
  alpha = (double) nearbyintl(rPrec);
#else
  double rPrec = r;
  if (abs(kp) <= SE_KP_MAX) {
    if (kp >= 0) rPrec /= seTbl[kp]; else rPrec *= seTbl[-kp];
  } else if (kp <= (int) floor(log10(DBL_MIN))) {
    rPrec = (rPrec * 1e+303) / pow(10.0, (double) (kp + 303));
  } else {
    rPrec /= pow(10.0, (double) kp);
  }
  if (rPrec < seTbl[SE_DIGITS - 1]) {
    rPrec *= 10.0;
    kp--;
  }
  alpha = nearbyint(rPrec);
#endif
  *nsig = SE_DIGITS;
  for (j = 1; j <= SE_DIGITS; j++) {
    alpha /= 10.0;
    if (alpha == floor(alpha)) (*nsig)--; else break;
  }
  if (*nsig == 0) {
    *nsig = 1;
    kp += 1;
  }
  *kpower = kp + SE_DIGITS - 1;
  int rgt = SE_DIGITS - *kpower;
  rgt = rgt < 0 ? 0 : rgt > SE_KP_MAX ? SE_KP_MAX : rgt;
  double fuzz = 0.5 / (double) seTbl[rgt];
  *widens = *kpower > 0 && *kpower <= SE_KP_MAX &&
    r < (double) seTbl[*kpower] - fuzz;
}

/* EncodeRealDrop0()'s trailing-zero drop: zeros after the '.' go, and the
   '.' with them when nothing is left after it */
static void seDrop0(char *buf) {
  char *p;
  for (p = buf; *p; p++) {
    if (*p == '.') {
      char *replace = p++;
      while ('0' <= *p && *p <= '9') {
        if (*(p++) != '0') replace = p;
      }
      if (replace != p) {
        while ((*(replace++) = *(p++))) ;
      }
      break;
    }
  }
}

/* formatReal() for one finite number: *e != 0 for scientific, with *d
   digits after the point and field width *w; *left = integer digits */
static void seFormatReal(double v, int *w, int *d, int *e, int *nsig,
                         int *left) {
  int kp, widens, neg = 0, rgt, mxsl, wF;
  if (v == 0.0) {
    kp = 0;
    *nsig = 1;
    widens = 0;
  } else {
    neg = v < 0;
    seScientific(fabs(v), &kp, nsig, &widens);
  }
  *left = kp + 1;
  if (widens) (*left)--;
  mxsl = neg + ((*left <= 0) ? 1 : *left);
  if (*left < 0) mxsl = 1 + neg;
  rgt = *nsig - *left;
  if (rgt < 0) rgt = 0;
  wF = mxsl + rgt + (rgt != 0);
  *e = (*left > 100 || *left <= -99) ? 2 : 1;
  *d = *nsig - 1;
  *w = neg + (*d > 0) + *d + 4 + *e;
  if (wF <= *w + seScipen) {
    *e = 0;
    *d = rgt;
    *w = wF;
  }
}

/* as.character(v) into buf.  Returns 0 when this platform's printf cannot
   be trusted to print what R prints: R for Windows prints with its own trio
   library, whose last digits differ from the C runtime's -- past 15 integer
   digits, and when the 15th digit is the one being rounded. */
static int seDblFormat(double v, char *buf, size_t n) {
  int w, d, e, nsig, left;
  char fmt[32];
  if (ISNAN(v)) { snprintf(buf, n, "%s", "NaN"); return 1; }
  if (!R_FINITE(v)) { snprintf(buf, n, "%s", v > 0 ? "Inf" : "-Inf"); return 1; }
  if (v == 0.0) v = 0.0;         /* -0 prints as 0, as R prints it */
  seFormatReal(v, &w, &d, &e, &nsig, &left);
#ifdef _WIN32
  if ((e == 0 && left > 15) || nsig == SE_DIGITS) return 0;
#endif
  /* EncodeRealDrop0() */
  if (e) {
    snprintf(fmt, sizeof(fmt), d ? "%%#%d.%de" : "%%%d.%de", w, d);
  } else {
    snprintf(fmt, sizeof(fmt), "%%%d.%df", w, d);
  }
  snprintf(buf, n, fmt, v);
  seDrop0(buf);
  return 1;
}

static const char *seDblToStr(seCtx *ctx, double v) {
  char buf[400];
  if (!seDblFormat(v, buf, sizeof(buf))) return seFailWhy(ctx, SE_WHY_NUMBER);
  return seStr(ctx, buf);
}

#endif /* __SE_NUM_H__ */
