#ifndef RXG_PRELUDE_H
#define RXG_PRELUDE_H
// Math used by rxode2 device (OpenCL C) model code.  The same text compiles
// as OpenCL C 1.2 and as host C, so a model's device functions can also be
// built and run on the CPU.
//
// Options (define before this header; they mirror rxSolve() settings):
//   RXG_SAFE_LOG   0, 1 or 2 (rxControl(safeLog=))
//   RXG_SAFE_ZERO  0 or 1    (rxControl(safeZero=))
//   RXG_SAFE_POW   0 or 1    (rxControl(safePow=))
//   RXG_STD_POW    0 or 1    (rxControl(useStdPow=))
//   RXG_FLOAT      solve in single precision (OpenCL only)
#ifndef RXG_SAFE_LOG
#define RXG_SAFE_LOG 1
#endif
#ifndef RXG_SAFE_ZERO
#define RXG_SAFE_ZERO 1
#endif
#ifndef RXG_SAFE_POW
#define RXG_SAFE_POW 1
#endif
#ifndef RXG_STD_POW
#define RXG_STD_POW 0
#endif

#ifdef __OPENCL_VERSION__
#ifdef RXG_FLOAT
#define double float
#define RXG_EPS FLT_EPSILON
#else
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#define RXG_EPS DBL_EPSILON
#endif
#define RXG_FN static inline
#else
#include <math.h>
#include <float.h>
#define RXG_EPS DBL_EPSILON
#define RXG_FN static inline
#endif

#ifndef M_1_SQRT_2PI
#define M_1_SQRT_2PI 0.398942280401432677939946059934
#endif
#ifndef NA_REAL
#define NA_REAL NAN
#endif
#ifndef R_NaN
#define R_NaN NAN
#endif
#ifndef R_PosInf
#define R_PosInf INFINITY
#endif
#ifndef R_NegInf
#define R_NegInf (-INFINITY)
#endif
// Device code cannot tell R's NA from NaN
#ifndef ISNA
#define ISNA(x) isnan(x)
#endif
#ifndef ISNAN
#define ISNAN(x) isnan(x)
#endif
#ifndef R_FINITE
#define R_FINITE(x) isfinite(x)
#endif

RXG_FN double _safe_log(double a) {
#if RXG_SAFE_LOG == 2
  if (a < 0) return R_NaN;
  return (a == 0) ? log(RXG_EPS) : log(a);
#elif RXG_SAFE_LOG == 1
  return (a <= 0) ? log(RXG_EPS) : log(a);
#else
  return log(a);
#endif
}

RXG_FN double _div0(double denom) {
#if RXG_SAFE_ZERO
  return (denom == 0.0) ? RXG_EPS : denom;
#else
  return denom;
#endif
}

// R's R_pow() (arithmetic.c) when x or y is infinite (neither is NaN)
RXG_FN double _rxg_pow_inf(double x, double y) {
  if (!R_FINITE(x)) {
    if (x > 0) return (y < 0.) ? 0. : R_PosInf;
    if (R_FINITE(y) && y == floor(y)) {
      return (y < 0.) ? 0. : (fmod(y, 2.) != 0 ? x : -x);
    }
  }
  if (!R_FINITE(y) && x >= 0) {
    if (y > 0) return (x >= 1) ? R_PosInf : 0.;
    return (x < 1) ? R_PosInf : 0.;
  }
  return R_NaN;
}

// R's R_pow() (arithmetic.c)
RXG_FN double R_pow(double x, double y) {
  if (x == 1. || y == 0.) return 1.;
  if (x == 0.) {
    if (y > 0.) return 0.;
    else if (y < 0) return R_PosInf;
    else return y; // NA or NaN
  }
  if (R_FINITE(x) && R_FINITE(y)) {
    if (y == 2.0) return x*x;
    return pow(x, y);
  }
  if (ISNAN(x) || ISNAN(y)) return x + y;
  return _rxg_pow_inf(x, y);
}

// R's R_pow_di() (arithmetic.c)
RXG_FN double R_pow_di(double x, int n) {
  double xn = 1.0;
  if (ISNAN(x)) return x;
  if (n != 0) {
    if (!R_FINITE(x)) return R_pow(x, (double)n);
    int isNeg = (n < 0);
    if (isNeg) n = -n;
    for (;;) {
      if (n & 01) xn *= x;
      if (n >>= 1) x *= x;
      else break;
    }
    if (isNeg) xn = 1. / xn;
  }
  return xn;
}

RXG_FN double Rx_pow(double a, double b) {
  double a0 = a;
#if RXG_SAFE_POW
  if (b < 0 && a == 0.0) a0 = RXG_EPS;
#endif
#if RXG_STD_POW
  return pow(a0, b);
#else
  return R_pow(a0, b);
#endif
}

RXG_FN double Rx_pow_di(double a, double b) {
  double a0 = a;
#if RXG_SAFE_POW
  if (b < 0 && a == 0.0) a0 = RXG_EPS;
#endif
#if RXG_STD_POW
  return pow(a0, b);
#else
  return R_pow_di(a0, (int)b);
#endif
}

RXG_FN double ceiling(double a) { return ceil(a); }
RXG_FN double loggamma(double a) { return lgamma(a); }
RXG_FN double gammafn(double a) { return tgamma(a); }
RXG_FN double lgammafn(double a) { return lgamma(a); }
#define factorial(a) exp(lgamma1p(a))
#define sign_exp(sgn, x)(((sgn) > 0.0) ? exp(x) : (((sgn) < 0.0) ? -exp(x) : 0.0))
#define abs_log1p(x) (((x) + 1.0 > 0.0) ? log1p(x) : (((x) + 1.0 > 0.0) ? log1p(-x) : 0.0))

// Rmath's sign(): NaN stays NaN
RXG_FN double rxg_sign(double x) {
  if (ISNAN(x)) return x;
  return (x > 0) ? 1.0 : ((x == 0) ? 0.0 : -1.0);
}
#undef sign
#define sign rxg_sign

RXG_FN double logit(double x, double low, double high) {
  if (ISNAN(x)) return x;
  if (!(low < high)) return R_NaN;
  if (x == low) return R_NegInf;
  if (x == high) return R_PosInf;
  if (x < low || x > high) return R_NaN;
  return log(x-low) - log(high-x);
}

RXG_FN double expit(double alpha, double low, double high) {
  if (ISNAN(alpha)) return alpha;
  if (!R_FINITE(alpha) && !ISNAN(low) && !ISNAN(high)) return alpha > 0 ? high : low;
  return (high-low)/(1+exp(-alpha))+low;
}
#define _logit1(x) logit(x, 0.0, 1.0)
#define _logit2(x, y) logit(x, y, 1.0)
#define _expit1(x) expit(x, 0.0, 1.0)
#define _expit2(x, y) expit(x, y, 1.0)
#define _invLogit1(x) expit(x, 0.0, 1.0)
#define _invLogit2(x, y) expit(x, y, 1.0)
#define _logitInv1(x) expit(x, 0.0, 1.0)
#define _logitInv2(x, y) expit(x, y, 1.0)

// standard normal cdf
RXG_FN double phi(double q) { return 0.5*erfc(-q*M_SQRT1_2); }
#define normcdf(x) phi(x)
#define _pnorm1(x) phi(x)

RXG_FN double ReLU(double x) { return (x > 0.0) ? x : 0.0; }

// _max()/_min()/_sign() are varargs on the host; the count is a literal, so
// select a fixed-arity version.  NaN propagates as in the host loop.
RXG_FN double _rxg_max2(double a, double b) { return (b > a) ? b : a; }
RXG_FN double _rxg_min2(double a, double b) { return (b < a) ? b : a; }
#define _rxg_max1(a) (a)
#define _rxg_max3(a,b,c) _rxg_max2(_rxg_max2(a,b),c)
#define _rxg_max4(a,b,c,d) _rxg_max2(_rxg_max3(a,b,c),d)
#define _rxg_max5(a,b,c,d,e) _rxg_max2(_rxg_max4(a,b,c,d),e)
#define _rxg_max6(a,b,c,d,e,f) _rxg_max2(_rxg_max5(a,b,c,d,e),f)
#define _rxg_max7(a,b,c,d,e,f,g) _rxg_max2(_rxg_max6(a,b,c,d,e,f),g)
#define _rxg_max8(a,b,c,d,e,f,g,h) _rxg_max2(_rxg_max7(a,b,c,d,e,f,g),h)
#define _rxg_min1(a) (a)
#define _rxg_min3(a,b,c) _rxg_min2(_rxg_min2(a,b),c)
#define _rxg_min4(a,b,c,d) _rxg_min2(_rxg_min3(a,b,c),d)
#define _rxg_min5(a,b,c,d,e) _rxg_min2(_rxg_min4(a,b,c,d),e)
#define _rxg_min6(a,b,c,d,e,f) _rxg_min2(_rxg_min5(a,b,c,d,e),f)
#define _rxg_min7(a,b,c,d,e,f,g) _rxg_min2(_rxg_min6(a,b,c,d,e,f),g)
#define _rxg_min8(a,b,c,d,e,f,g,h) _rxg_min2(_rxg_min7(a,b,c,d,e,f,g),h)
#define _rxg_sign1(a) rxg_sign(a)
#define _rxg_sign2(a,b) (rxg_sign(a)*rxg_sign(b))
#define _rxg_sign3(a,b,c) (_rxg_sign2(a,b)*rxg_sign(c))
#define _rxg_sign4(a,b,c,d) (_rxg_sign3(a,b,c)*rxg_sign(d))
#define _RXG_CAT(a, b) a##b
#define _max(n, ...) _RXG_CAT(_rxg_max, n)(__VA_ARGS__)
#define _min(n, ...) _RXG_CAT(_rxg_min, n)(__VA_ARGS__)
#define _sign(n, ...) _RXG_CAT(_rxg_sign, n)(__VA_ARGS__)

#include "rxode2gpu/rxg_special.h"

#endif // RXG_PRELUDE_H
