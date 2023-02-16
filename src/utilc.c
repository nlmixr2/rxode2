#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <sys/stat.h> 
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
#include <errno.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "../inst/include/rxode2.h"
#include <rxode2parseHandleEvid.h>

int _setSilentErr=0, _isRstudio2=0;
extern void setSilentErr(int silent){
  _setSilentErr = silent;
}

extern void setRstudioPrint(int rstudio){
  _isRstudio2=rstudio;
}


extern int getSilentErr(void){return _setSilentErr;}

extern int getRstudioPrint(void){return _isRstudio2;}

extern void RSprintf(const char *format, ...) {
  if (_setSilentErr == 0) {
    if(_isRstudio2){
      va_list args;
      va_start(args, format);
      REvprintf(format, args);
      va_end(args);
    } else{
      va_list args;
      va_start(args, format);
      Rvprintf(format, args);
      va_end(args);
    } 
  }
}

double gamma_p(double a, double z);
SEXP _gammap(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = gamma_p(reala ? aD[j] : (double)aI[j],
			realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = gamma_p(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_p(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double gamma_q(double a, double z);
SEXP _gammaq(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = gamma_q(reala ? aD[j] : (double)aI[j],
			realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = gamma_q(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_q(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double tgamma_lower(double a, double z);
SEXP _lowergamma(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = tgamma_lower(reala ? aD[j] : (double)aI[j],
			realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = tgamma_lower(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = tgamma_lower(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double tgamma_upper(double a, double z);
SEXP _uppergamma(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = tgamma_upper(reala ? aD[j] : (double)aI[j],
			realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = tgamma_upper(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = tgamma_upper(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double gamma_p_derivative(double a, double x);

SEXP _gammapDer(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = gamma_p_derivative(reala ? aD[j] : (double)aI[j],
			realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = gamma_p_derivative(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_p_derivative(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double gamma_p_inv(double a, double x);
SEXP _gammapInv(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = gamma_p_inv(reala ? aD[j] : (double)aI[j],
			realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = gamma_p_inv(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_p_inv(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double gamma_p_inva(double a, double x);
SEXP _gammapInva(SEXP a, SEXP z) {
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      retD[j] = gamma_p_inva(reala ? aD[j] : (double)aI[j],
			     realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      retD[j] = gamma_p_inva(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_p_inva(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double gamma_q_inv(double a, double x);
SEXP _gammaqInv(SEXP a, SEXP z) {
  // Returns a value x such that: q = gamma_q(a, x);
  // Requires: a > 0 and 1 >= p,q >= 0.
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      // Returns a value x such that: q = gamma_q(a, x);
      // Requires: a > 0 and 1 >= p,q >= 0.
      retD[j] = gamma_q_inv(reala ? aD[j] : (double)aI[j],
			    realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      // Returns a value x such that: q = gamma_q(a, x);
      // Requires: a > 0 and 1 >= p,q >= 0.
      retD[j] = gamma_q_inv(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_q_inv(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}


double gamma_q_inva(double a, double x);
SEXP _gammaqInva(SEXP a, SEXP z) {
  // Returns a value x such that: q = gamma_q(a, x);
  // Requires: a > 0 and 1 >= p,q >= 0.
  int typea = TYPEOF(a);
  int typez = TYPEOF(z);
  int pro=0;
  SEXP ret;
  double *aD, *zD;
  int *aI, *zI;
  int lena = Rf_length(a);
  int lenz = Rf_length(z);
  int reala=0, realz=0;
  if (typea == REALSXP){
    reala=1;
    aD = REAL(a);
  } else if (typea == INTSXP){
    aI = INTEGER(a);
  } else {
    Rf_errorcall(R_NilValue, _("'a' needs to be a number"));
  }
  if (typez == REALSXP){
    realz=1;
    zD = REAL(z);
  } else if (typez == INTSXP){
    zI = INTEGER(z);
  } else {
    Rf_errorcall(R_NilValue, _("'z' needs to be a number"));
  }
  if (lena == lenz) {
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    for (int j = lena; j--;){
      // Returns a value x such that: q = gamma_q(a, x);
      // Requires: a > 0 and 1 >= p,q >= 0.
      retD[j] = gamma_q_inva(reala ? aD[j] : (double)aI[j],
			    realz ? zD[j] : (double)zI[j]);
    }
  } else if (lena == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lenz));pro++;
    double *retD = REAL(ret);
    double a0 = reala ? aD[0] : (int)aI[0];
    for (int j = lenz; j--;){
      // Returns a value x such that: q = gamma_q(a, x);
      // Requires: a > 0 and 1 >= p,q >= 0.
      retD[j] = gamma_q_inva(a0, realz ? zD[j] : (double)zI[j]);
    }
  } else if (lenz == 1){
    ret = PROTECT(Rf_allocVector(REALSXP, lena));pro++;
    double *retD = REAL(ret);
    double z0 = realz ? zD[0] : (double)zI[0];
    for (int j = lena; j--;){
      retD[j] = gamma_q_inva(reala ? aD[j] : (double)aI[j], z0);
    }
  } else {
    Rf_errorcall(R_NilValue, _("inconsistent sizes"));
  }
  UNPROTECT(pro);
  return ret;
}

double logit(double x, double low, double high) {
  return _powerD(x, 1.0, 4, low, high);
}

double expit(double alpha, double low, double high) {
  return _powerDi(alpha, 1.0, 4, low, high);
}

double probit(double x, double low, double high) {
  return _powerD(x, 1.0, 6, low, high);
}

double probitInv(double alpha, double low, double high) {
  return _powerDi(alpha, 1.0, 6, low, high);
}

SEXP _probit(SEXP xS, SEXP lowS, SEXP highS) {
  int typex = TYPEOF(xS);
  int typelow = TYPEOF(lowS);
  int typehigh = TYPEOF(highS);
  double low, high;
  if (Rf_length(lowS) != 1){
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (Rf_length(highS) != 1){
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (typelow == INTSXP){
    low = (double)(INTEGER(lowS)[0]);
  } else if (typelow == REALSXP) {
    low = REAL(lowS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (typehigh == INTSXP){
    high = (double)(INTEGER(highS)[0]);
  } else if (typehigh == REALSXP) {
    high = REAL(highS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (high <= low) {
    Rf_errorcall(R_NilValue, _("'high' must be greater than 'low'"));
  }
  int lenx = Rf_length(xS);
  double *xD = NULL;
  int *xI = NULL;
  int isD=0;
  if (typex == REALSXP){
    isD=1;
    xD = REAL(xS);
  } else if (typex == INTSXP){
    xI = INTEGER(xS);
  }
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, lenx));
  double *retD = REAL(ret);
  if (isD){
    for (int i = lenx; i--;){
      retD[i] = probit(xD[i], low, high);
    }
  } else {
    for (int i = lenx; i--;){
      retD[i] = probit((double)(xI[i]), low, high);
    }
  }
  UNPROTECT(1);
  return ret;
}

SEXP _probitInv(SEXP xS, SEXP lowS, SEXP highS) {
  int typex = TYPEOF(xS);
  int typelow = TYPEOF(lowS);
  int typehigh = TYPEOF(highS);
  double low, high;
  if (Rf_length(lowS) != 1){
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (Rf_length(highS) != 1){
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (typelow == INTSXP){
    low = (double)(INTEGER(lowS)[0]);
  } else if (typelow == REALSXP) {
    low = REAL(lowS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (typehigh == INTSXP){
    high = (double)(INTEGER(highS)[0]);
  } else if (typehigh == REALSXP) {
    high = REAL(highS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (high <= low) {
    Rf_errorcall(R_NilValue, _("'high' must be greater than 'low'"));
  }
  int lenx = Rf_length(xS);
  double *xD = NULL;
  int *xI = NULL;
  int isD=0;
  if (typex == REALSXP){
    isD=1;
    xD = REAL(xS);
  } else if (typex == INTSXP){
    xI = INTEGER(xS);
  }
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, lenx));
  double *retD = REAL(ret);
  if (isD){
    for (int i = lenx; i--;){
      retD[i] = probitInv(xD[i], low, high);
    }
  } else {
    for (int i = lenx; i--;){
      retD[i] = probitInv((double)(xI[i]), low, high);
    }
  }
  UNPROTECT(1);
  return ret;
}

SEXP _logit(SEXP xS, SEXP lowS, SEXP highS) {
  int typex = TYPEOF(xS);
  int typelow = TYPEOF(lowS);
  int typehigh = TYPEOF(highS);
  double low, high;
  if (Rf_length(lowS) != 1){
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (Rf_length(highS) != 1){
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (typelow == INTSXP){
    low = (double)(INTEGER(lowS)[0]);
  } else if (typelow == REALSXP) {
    low = REAL(lowS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (typehigh == INTSXP){
    high = (double)(INTEGER(highS)[0]);
  } else if (typehigh == REALSXP) {
    high = REAL(highS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (high <= low) {
    Rf_errorcall(R_NilValue, _("'high' must be greater than 'low'"));
  }
  int lenx = Rf_length(xS);
  double *xD = NULL;
  int *xI = NULL;
  int isD=0;
  if (typex == REALSXP){
    isD=1;
    xD = REAL(xS);
  } else if (typex == INTSXP){
    xI = INTEGER(xS);
  }
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, lenx));
  double *retD = REAL(ret);
  if (isD){
    for (int i = lenx; i--;){
      retD[i] = logit(xD[i], low, high);
    }
  } else {
    for (int i = lenx; i--;){
      retD[i] = logit((double)(xI[i]), low, high);
    }
  }
  UNPROTECT(1);
  return ret;
}


SEXP _expit(SEXP xS, SEXP lowS, SEXP highS) {
  int typex = TYPEOF(xS);
  int typelow = TYPEOF(lowS);
  int typehigh = TYPEOF(highS);
  double low, high;
  if (Rf_length(lowS) != 1){
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (Rf_length(highS) != 1){
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (typelow == INTSXP){
    low = (double)(INTEGER(lowS)[0]);
  } else if (typelow == REALSXP) {
    low = REAL(lowS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'low' must be a numeric of length 1"));
  }
  if (typehigh == INTSXP){
    high = (double)(INTEGER(highS)[0]);
  } else if (typehigh == REALSXP) {
    high = REAL(highS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'high' must be a numeric of length 1"));
  }
  if (high <= low) {
    Rf_errorcall(R_NilValue, _("'high' must be greater than 'low'"));
  }
  int lenx = Rf_length(xS);
  double *xD = NULL;
  int *xI = NULL;
  int isD=0;
  if (typex == REALSXP){
    isD=1;
    xD = REAL(xS);
  } else if (typex == INTSXP){
    xI = INTEGER(xS);
  }
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, lenx));
  double *retD = REAL(ret);
  if (isD){
    for (int i = lenx; i--;){
      retD[i] = expit(xD[i], low, high);
    }
  } else {
    for (int i = lenx; i--;){
      retD[i] = expit((double)(xI[i]), low, high);
    }
  }
  UNPROTECT(1);
  return ret;
}
