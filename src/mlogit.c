#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include "../inst/include/rxode2.h"
#include "mlogit.h"

// This is close to softmax, but adapted for multinomial logistic regression
void mexpit(double *x, double *p, int n) {
  double sum = 1.0;
  for (int i = 0; i < n; i++) {
    sum += x[i] = exp(p[i]);
  }
  for (int i = 0; i < n; i++) {
    x[i] /= sum;
  }
}

// Now write the R interface for mexpit
SEXP _rxode2_mexpit(SEXP x) {
  SEXP p;
  int n = LENGTH(x);
  PROTECT(p = Rf_allocVector(REALSXP, n));
  double *px = REAL(x);
  double *pp = REAL(p);

  mexpit(pp, px, n);

  UNPROTECT(1);
  return p;
}/* _rxode2_mexpit() */


// Calculate element-wise derivatives (diagonal of the Jacobian) of mexpit
void dmexpit(double *x, double *p, int n) {
  double sum = 1.0;
  for (int i = 0; i < n; i++) {
    sum += x[i] = exp(p[i]);
  }
  for (int i = 0; i < n; i++) {
    x[i] = x[i]/sum - (x[i]*x[i])/(sum*sum);
  }
}

// Now write the R interface for dmexpit
SEXP _rxode2_dmexpit(SEXP x) {
  SEXP p;
  int n = LENGTH(x);
  PROTECT(p = Rf_allocVector(REALSXP, n));
  double *px = REAL(x);
  double *pp = REAL(p);

  dmexpit(pp, px, n);

  UNPROTECT(1);
  return p;
}/* _rxode2_dmexpit() */


// First calculate the root for the mutiroot finding algorithm
SEXP _rxode2_mlogit_f(SEXP x, SEXP p) {
  SEXP r;
  int n = LENGTH(x);
  PROTECT(r = Rf_allocVector(REALSXP, n));

  double *px = REAL(x);
  double *pp = REAL(p);
  double *pr = REAL(r);

  mexpit(pr, px, n);
  for (int i = 0; i < n; i++) {
    pr[i] -= pp[i];
  }

  UNPROTECT(1);
  return r;
}

// This is the Jacobian of the root
SEXP _rxode2_mlogit_j(SEXP x) {
  SEXP j, xp;
  int n = LENGTH(x);
  PROTECT(j = Rf_allocMatrix(REALSXP, n, n));
  PROTECT(xp = Rf_allocVector(REALSXP, n));
  double *px = REAL(x);
  double *pxp = REAL(xp);
  double *pj = REAL(j);
  double sum = 0.0;
  for (int i = 0; i < n; i++) {
    sum += pxp[i] = exp(px[i]);
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      //
      // Diagonals
      //
      if (i == j) {
        // If sum = sum-exp(x1)
        //> D(S("exp(x1)/(1+exp(x1)+sum)+c"), "x1")
        //  (Add)exp(x1)/(1 + sum + exp(x1)) - exp(2*x1)/(1 + sum + exp(x1))^2
        pj[i + n * j] = pxp[i] / (1  + sum) - exp(2*px[i])/((1+sum)*(1+sum));
      } else {
        // if sum = sum-exp(x2)
        // > D(S("exp(x1)/(1+exp(x2)+sum)+c"), "x2")
        // (Mul) -exp(x1 + x2)/(1 + sum + exp(x2))^2
        pj[i + n * j] = -pxp[i] * pxp[j] / (1.0 + sum * sum);
      }
    }
  }

  UNPROTECT(2);
  return j;
}/* _rxode2_mlogit() */
