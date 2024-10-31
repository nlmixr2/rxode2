
#pragma once
#define STRICT_R_HEADERS
#ifndef __rxode2_H__
#define __rxode2_H__

#define rxLlikSaveSize 9

#include <stdbool.h>
#include "rxode2parse.h"
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>

#include "rxode2parseSbuf.h"

#include <float.h>
#include <stdio.h>
#include <stdarg.h>


#define isDose(evid) ((evid) == 3 || (evid) >= 100)
#define isObs(evid) ((evid) == 0 || (evid) == 2 || ((evid) >= 9 && (evid) <= 99))

#include "rxode2_control.h"
#include <stdint.h>    // for uint64_t rather than unsigned long long

#ifndef __RXODE2PTR_H__  // directly refer to abi need to be excluded
#define getAdvan(idx) ind->solve + (op->neq + op->nlin)*(idx) + op->neq
#define getSolve(idx) ind->solve + (op->neq + op->nlin)*(idx)
#endif

#ifdef _isrxode2_
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
#define isSameTime(xout, xp) (fabs((xout)-(xp))  <= DBL_EPSILON*max2(fabs(xout),fabs(xp)))

// use ~dop853 definition of same time
#define isSameTimeDop(xout, xp) (0.1 * fabs((xout)-(xp)) <= fabs(xout) * 2.3E-16)
#define isSameTimeOp(xout, xp) (op->stiff == 0 ? isSameTimeDop(xout, xp) : isSameTime(xout, xp))


#else

#if defined(__cplusplus)
#include "rxode2_RcppExports.h"

#endif

#endif // _isrxode2_

#define rxNEG_LOG_SQRT_TWO_PI

#if defined(__cplusplus)
extern "C" {
#endif


#ifdef _isrxode2_
  void parseFree(int last);
#endif

typedef void (*t_dydt)(int *neq, double t, double *A, double *DADT);
typedef void (*t_calc_jac)(int *neq, double t, double *A, double *JAC, unsigned int __NROWPD__);
typedef void (*t_calc_lhs)(int cSub, double t, double *A, double *lhs);
typedef void (*t_update_inis)(int cSub, double *);
typedef void (*t_dydt_lsoda_dum)(int *neq, double *t, double *A, double *DADT);
typedef void (*t_jdum_lsoda)(int *neq, double *t, double *A,int *ml, int *mu, double *JAC, int *nrowpd);
typedef int (*t_dydt_liblsoda)(double t, double *y, double *ydot, void *data);
typedef void (*t_ode_current)(void);

typedef void (*t_set_solve)(rx_solve *);

typedef rx_solve *(*t_get_solve)(void);

typedef void *(*t_assignFuns)(void);

#ifndef __RXODE2PTR_H__
rx_solve *getRxSolve_(void);
#endif
rx_solve *getRxSolve2_(void);
rx_solve *getRxSolve(SEXP ptr);

#ifndef __RXODE2PTR_H__
void par_solve(rx_solve *rx);
#endif

rx_solving_options *getRxOp(rx_solve *rx);

SEXP rxode2_df(int doDose, int doTBS);
SEXP rxode2_par_df(void);

void rxOptionsIniEnsure(int mx);

void rxUpdateFuns(SEXP trans);

#define _eps sqrt(DBL_EPSILON)

static inline double erfinv(double x)  __attribute__((unused));
static inline double erfinv(double x) {
  return Rf_qnorm5((1 + x)/2.0, 0, 1, 1, 0)*M_SQRT1_2;
}
#define rxDistributionNorm     1
#define rxDistributionPois     2
#define rxDistributionBinom    3
#define rxDistributionBeta     4
#define rxDistributionT        5
#define rxDistributionChisq    6
#define rxDistributionDexp     7
#define rxDistributionF        8
#define rxDistributionGeom     9
#define rxDistributionHyper   10
#define rxDistributionUnif    11
#define rxDistributionWeibull 12
#define rxDistributionCauchy  13
#define rxDistributionGamma   14
#define rxDistributionOrdinal 15
#define rxDistributionN2ll    16
#define rxDistributionDnorm   17

static inline void _splitYj(int *yj, int *dist,  int *trans) {
  *dist  = *yj/10;
  *trans = *yj - *dist*10;
  *dist  = *dist + 1;
}
// Inverse
static inline double _powerDi(double x, double lambda, int yj0, double low, double high)  __attribute__((unused));
static inline double _powerDi(double x, double lambda, int yj0, double low, double high){
  if (!R_finite(x)) return NA_REAL;
  double x0=x, ret, l2, yjd;
  int yj, dist;
  _splitYj(&yj0, &dist,  &yj);
  switch(yj){
  case 7: // inverse-Yeo Johnson followed by pnorm
    if (lambda == 1.0) {
      yjd = x;
    } else if (x >= 0){
      if (lambda == 0) yjd = log1p(x);
      else yjd = (pow(x + 1.0, lambda) - 1.0)/lambda;
    } else {
      if (lambda == 2.0) yjd = -log1p(-x);
      else {
	l2 = 2.0 - lambda;
	yjd = (1.0 - pow(1.0 - x, l2))/l2;
      }
    }
    return (high-low)*Rf_pnorm5(x, 0, 1, 1, 0)+low;
  case 6: // probitInverse
    return (high-low)*Rf_pnorm5(x, 0, 1, 1, 0)+low;
  case 5: // inverse-Yeo-Johnson followed by expit
    if (lambda == 1.0) {
      yjd = x;
    } else if (x >= 0){
      if (lambda == 0) yjd = log1p(x);
      else yjd = (pow(x + 1.0, lambda) - 1.0)/lambda;
    } else {
      if (lambda == 2.0) yjd = -log1p(-x);
      else {
	l2 = 2.0 - lambda;
	yjd = (1.0 - pow(1.0 - x, l2))/l2;
      }
    }
    return (high-low)/(1+exp(-yjd))+low;
  case 4:
    return (high-low)/(1+exp(-x))+low; // expit
  case 3:
    return exp(x);
  case 2:
    return x;
  case 0:
    if (lambda == 1.0) return (x+1.0);
    if (lambda == 0) return exp(x);
    // (x^lambda-1)/lambda=y
    // (lambda*y+1)^(1/lambda)
    x0 = x*lambda+1.0;
    if (x0 <= _eps) return _eps;
    ret = pow(x0, 1.0/lambda);
    if (ISNA(ret)) {
      // Warning?
      return _eps;
    }
    return ret;
  case 1:
    if (lambda == 1.0) return x;
    if (x >= 0){
      // log(x+1)= y; exp(y)-1=x
      if (lambda == 0) return expm1(x);
      // ((x+1)^lambda-1)/lambda=y
      // (y*lambda+1)^(1/y)-1=y
      return pow(x*lambda+1.0, 1.0/lambda)-1.0;
    } else {
      // (-(1-x)^(2-lambda)-1)/(2-lambda)
      if (lambda ==  2.0) return -expm1(-x);
      // (-(1-x)^(2-lambda)-1)/(2-lambda) = y
      l2 = (2.0 - lambda);
      return 1.0 - pow(1.0 - l2*x, 1.0/l2);
    }
  }
  return NA_REAL;
}

static inline double _powerD(double x, double lambda, int yj0, double low, double high)  __attribute__((unused));
static inline double _powerD(double x, double lambda, int yj0, double low, double high) {
  if (!R_finite(x)) return NA_REAL;
  double x0=x, l2, p;
  int yj, dist;
  _splitYj(&yj0, &dist,  &yj);
  switch (yj) {
  case 7:
    p = (x-low)/(high-low);
    if (p >= 1) return R_NaN;
    if (p <= 0) return R_NaN;
    /* REprintf("%f %f %f\n", x, p, -log(1/p-1)); */
    p = Rf_qnorm5(p, 0, 1, 1, 0);
    if (lambda == 1.0) return p;
    if (p >= 0){
      if (lambda == 0) return log1p(p);
      return (pow(p + 1.0, lambda) - 1.0)/lambda;
    } else {
      if (lambda == 2.0) return -log1p(-p);
      l2 = 2.0 - lambda;
      return (1.0 - pow(1.0 - p, l2))/l2;
    }
  case 6: // probitNorm
    p = (x-low)/(high-low);
    if (p >= 1) return R_NaN;
    if (p <= 0) return R_NaN;
    /* REprintf("%f %f %f\n", x, p, -log(1/p-1)); */
    return Rf_qnorm5(p, 0, 1, 1, 0);
  case 5: // logit followed by yeo-johnson
    p = (x-low)/(high-low);
    if (p >= 1) return R_NaN;
    if (p <= 0) return R_NaN;
    p = -log(1/p-1);
    if (lambda == 1.0) return p;
    if (p >= 0){
      if (lambda == 0) return log1p(p);
      return (pow(p + 1.0, lambda) - 1.0)/lambda;
    } else {
      if (lambda == 2.0) return -log1p(-p);
      l2 = 2.0 - lambda;
      return (1.0 - pow(1.0 - p, l2))/l2;
    }
  case 4: // logitNorm
    p = (x-low)/(high-low);
    if (p >= 1) return R_NaN;
    if (p <= 0) return R_NaN;
    /* REprintf("%f %f %f\n", x, p, -log(1/p-1)); */
    return -log(1/p-1);
  case 3: // logNorm
    if (x <= _eps) x0= _eps;
    return log(x0);
  case 2: // norm
    return x;
  case 0: // boxCoxNorm
    if (lambda == 1.0) return x-1.0;
    if (x <= _eps) x0= _eps;
    if (lambda ==  0.0) return log(x0);
    return (pow(x0, lambda) - 1.0)/lambda;
  case 1: // yeoJohnsonNorm
    if (lambda == 1.0) return x;
    if (x >= 0){
      if (lambda == 0) return log1p(x);
      return (pow(x + 1.0, lambda) - 1.0)/lambda;
    } else {
      if (lambda == 2.0) return -log1p(-x);
      l2 = 2.0 - lambda;
      return (1.0 - pow(1.0 - x, l2))/l2;
    }
  }
  return NA_REAL;
}

static inline double _powerDD(double x, double lambda, int yj0, double low, double high)  __attribute__((unused));
static inline double _powerDD(double x, double lambda, int yj0, double low, double high){
  if (!R_finite(x)) return NA_REAL;
  double x0 = x, xl, hl,eri;
  int yj, dist;
  _splitYj(&yj0, &dist,  &yj);
  switch(yj){
  case 7:
    // Subs(Derivative(yeoJohnson(_xi_1), _xi_1), (_xi_1), (logit(x)))*Derivative(logit(x), x)
    return _powerDD(_powerD(x, lambda, 6, low, high), lambda, 1, low, high)*_powerDD(x, lambda, 6, low, high);
  case 6: // derivative
    // 2.82842712474619*M_SQRT_PI/2*exp((erfinv(-1+2*(-low+x)/(high-low)))^2)/(high-low)
    hl = (high-low);
    eri = erfinv(-1+2*(-low+x)/hl);
    return 2.506628274631000241612*exp(eri*eri)/hl;
  case 5: // logit followed by yeo-johnson  yeoJohnson(logit(x))
    // Subs(Derivative(yeoJohnson(_xi_1), _xi_1), (_xi_1), (logit(x)))*Derivative(logit(x), x)
    return _powerDD(_powerD(x, lambda, 4, low, high), lambda, 1, low, high)*_powerDD(x, lambda, 4, low, high);
  case 4: // logitNorm
    xl = (x-low);
    hl = (high - low);
    return hl/(xl*(hl-xl));
  case 3:
    if (x <= _eps) return x0 = _eps;
    return 1/x0;
  case 2:
    return 1.0;
  case 0:
    if (lambda == 1.0) return 1.0;
    if (x <= _eps) return x0 = _eps;
    if (lambda == 0.0) return 1/x0;
    // pow(x,lambda)/lambda - 1/lambda
    return pow(x0, lambda-1);
  case 1: // Yeo Johnson derivative
    if (lambda ==  1.0) return 1.0;
    if (x >= 0){
      if (lambda == 0.0) return 1.0/(x + 1.0);
      return pow(x + 1.0, lambda-1.0);
    } else {
      if (lambda == 2.0) return -1/(1.0 - x);
      return pow(1.0 - x, 1.0-lambda);
    }
  }
  return NA_REAL;
}

static inline double _powerDDD(double x, double lambda, int yj0, double low, double high) __attribute__((unused));
static inline double _powerDDD(double x, double lambda, int yj0, double low, double high){
  if (!R_finite(x)) return NA_REAL;
  double x0 = x, hl, hl2, xl,  t1, dL, eri;
  int yj, dist;
  _splitYj(&yj0, &dist,  &yj);
  switch(yj){
  case 7:
    dL = _powerDD(x, lambda, 6, low, high);
    return dL*dL*_powerDD(_powerD(x, lambda, 6, low, high), lambda, 1, low, high);
  case 6: // derivative
    //10.026513098524*exp(erfinv(-1+2*(-low+x)/(high-low))^2)*M_SQRT_PI/2*exp((erfinv(-1+2*(-low+x)/(high-low)))^2)*erfinv(-1+2*(-low+x)/(high-low))/(high-low)^2
    hl = (high-low);
    eri = erfinv(-1+2*(-low+x)/hl);
    return 8.885765876316728650863*exp(2*eri*eri)*eri/(hl*hl);
  case 5:
    //Derivative(logit(x), x)^2*Subs(Derivative(yeoJohnson(_xi_1), _xi_1, _xi_1), (_xi_1), (logit(x))) + Subs(Derivative(yeoJohnson(_xi_1), _xi_1), (_xi_1), (logit(x)))*Derivative(logit(x), x, x)
    dL = _powerDD(x, lambda, 4, low, high);
    return dL*dL*_powerDD(_powerD(x, lambda, 4, low, high), lambda, 1, low, high);
  case 4: // logit
    // (high - low)^2/((-low + x)^4*(-1 + (high - low)/(-low + x))^2) - 2*(high - low)/((-low + x)^3*(-1 + (high - low)/(-low + x)))
    hl = (high - low);
    hl2 = hl*hl;
    xl = (-low + x);
    t1 = (-1.0 + hl/xl);
    return 1.0*hl2/(hl2*hl2*t1*t1) - 2.0*hl/(xl*xl*xl*t1);
  case 3:
    if (x <= _eps) x0 = _eps;
    return -1/(x0*x0);
  case 2:
    return 0;
  case 0:
    if (lambda == 1.0) return 0;
    if (x <= _eps) return x0 = _eps;
    if (lambda == 0.0) return -1/(x0*x0);
    // pow(x,lambda)/lambda - 1/lambda
    return (lambda-1)*pow(x0, lambda-2);
  case 1:
    if (lambda == 1.0) return 0;
    if (x >= 0){
      if (lambda ==  0.0) return -1/((x + 1.0)*(x + 1.0));
      return (lambda-1.0)*pow(x + 1.0, lambda-2.0);
    } else {
      if (lambda == 2.0) return -1/((1.0 - x)*(1.0 - x));
      return -(1.0-lambda)*pow(1.0 - x, -lambda);
    }
  }
  return NA_REAL;
}

static inline double _powerL(double x, double lambda, int yj0, double low, double high) __attribute__((unused));
static inline double _powerL(double x, double lambda, int yj0, double low, double high){
  if (!R_finite(x)) return NA_REAL;
  double x0 = x, hl, xl, hl2, eri;
  int yj, dist;
  _splitYj(&yj0, &dist,  &yj);
  switch(yj){
  case 7:
    return log(_powerDD(_powerD(x, lambda, 6, low, high), lambda, 1, low, high))+log(_powerDD(x, lambda, 6, low, high));
  case 6:
    hl = (high-low);
    eri = erfinv(-1+2*(-low+x)/hl);
    return 0.918938533204672669541 +eri*eri-log(hl);
  case 5:
    // Subs(Derivative(yeoJohnson(_xi_1), _xi_1), (_xi_1), (logit(x)))*Derivative(logit(x), x)
    return log(_powerDD(_powerD(x, lambda, 4, low, high), lambda, 1, low, high))+log(_powerDD(x, lambda, 4, low, high));
  case 4: // logit d/dx(logit(x))
    xl = (x-low);
    if (xl <= _eps) xl = _eps;
    hl = (high - low);
    hl2 = hl-xl;
    if (xl <= _eps) hl2 = _eps;
    return log(hl)-log(xl)-log(hl2);
    /* return 0; */
  case 3:
    if (x <= _eps) x0 = _eps;
    return -log(x0);
  case 2:
    return 0;
  case 0:
    if (lambda == 1.0) return 0;
    if (x <= _eps) x0 = _eps;
    return (lambda - 1.0)*log(x0);
  case 1:
    if (x >= 0) return (lambda - 1.0)*log1p(x);
    return (1.0-lambda)*log1p(-x);
  }
  return NA_REAL;
  // d = 0.0 for cox box
  // d = 1.0 fo  Yeo- Johnson
  // logLik approximation
  // y^(lambda)/lambda - 1/lambda
  // dh/dy = y^(lambda-1)
  // log(dh/dy) = (lambda-1)*log(y) + log(lambda)
  //
  // (x + 1.0)^(lambda)/lambda - 1/lambda
  // dh/dy = (x+1.0)^(lambda-1)
  // log(dh/dy) = (lambda-1)*log(x+1.0)

  // For negative values yj becomes
  // (-x+1)^(2-lambda)/(2-lambda) - 1/(2-lambda)
  // dh/dy = (-x+1)^(1-lambda)
  // log(dh/dy) = (1-lambda)*log(-x+1)
}

// extra liklihood
static inline double _powerDL(double x, double lambda, int yj0, double low, double hi) __attribute__((unused));
static inline double _powerDL(double x, double lambda, int yj0, double low, double hi) {
  // d(logLik/dlambda)
  if (!R_finite(x)) return NA_REAL;
  double x0 = x;
  int yj, dist;
  _splitYj(&yj0, &dist,  &yj);
  switch (yj){
  case 6:
    return 0; // does not depend on lambda
  case 5:
    return _powerDL(_powerD(x, lambda, 4, low, hi), lambda, 1, low, hi);
  case 4:
    // For logit norm, no dependence on lambda
    return 0;
  case 3:
    if (x <= _eps) x0 = _eps;
    return log(x0);
  case 2:
    // For normal transform no dependence of lambda
    return 0;
  case 0:
    if (lambda == 1.0) return 0;
    if (x <= _eps) x0 = _eps;
    return log(x0);
  case 1:
    if (lambda == 1.0) return 0;
    if (x >= 0) return log1p(x);
    return -log1p(x);
  }
  return NA_REAL;
  // d = 0.0 for cox box
  // d = 1.0 fo  Yeo- Johnson
  // logLik approximation
  // y^(lambda)/lambda - 1/lambda
  // dh/dy = y^(lambda-1)
  // log(dh/dy) = (lambda-1)*log(y) + log(lambda)
  //
  // (x + 1.0)^(lambda)/lambda - 1/lambda
  // dh/dy = (x+1.0)^(lambda-1)
  // log(dh/dy) = (lambda-1)*log(x+1.0)

  // For negative values yj becomes
  // (-x+1)^(2-lambda)/(2-lambda) - 1/(2-lambda)
  // dh/dy = (-x+1)^(1-lambda)
  // log(dh/dy) = (1-lambda)*log(-x+1)
}

static inline double abs1(double x){
  if (x == 0.0) return 1.0;
  return fabs(x);
}
static inline double dabs1(double x){
  return (x>0)-(x<0);
}

static inline double dabs(double x) {
  return (x>0)-(x<0);
}

static inline double dabs2(double x) {
  return 0.0;
}

#if defined(__cplusplus)
  extern "C" rx_solve rx_global;
  extern "C" rx_solving_options op_global;
  extern "C" rx_solving_options_ind *inds_global;
#else
  extern rx_solve rx_global;
  extern rx_solving_options op_global;
  extern rx_solving_options_ind *inds_global;
#endif



#endif
#if defined(__cplusplus)
}
#endif
