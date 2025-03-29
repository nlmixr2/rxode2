#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
#include <errno.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "../inst/include/rxode2.h"

int _setSilentErr=0, _isRstudio2=0;
extern "C" void setSilentErr(int silent){
  _setSilentErr = silent;
}

extern "C" void setRstudioPrint(int rstudio){
  _isRstudio2=rstudio;
}


extern "C" int getSilentErr(void){return _setSilentErr;}

extern "C" int getRstudioPrint(void){return _isRstudio2;}

extern "C" void RSprintf(const char *format, ...) {
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

#if defined(__INTEL_LLVM_COMPILER) || defined(__INTEL_COMPILER__)
extern "C" SEXP _rxode2_isIntel(void) {
  SEXP ret = PROTECT(Rf_allocVector(LGLSXP, 1));
  INTEGER(ret)[0] = 1;
  UNPROTECT(1);
  return ret;
}

#else
extern "C" SEXP _rxode2_isIntel(void) {
  SEXP ret = PROTECT(Rf_allocVector(LGLSXP, 1));
  INTEGER(ret)[0] = 0;
  UNPROTECT(1);
  return ret;
}
#endif


extern "C" double gamma_p(double a, double z);
extern "C" SEXP _gammap(SEXP a, SEXP z) {
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

extern "C" double gamma_q(double a, double z);
extern "C" SEXP _gammaq(SEXP a, SEXP z) {
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

extern "C" double tgamma_lower(double a, double z);
extern "C" SEXP _lowergamma(SEXP a, SEXP z) {
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

extern "C" double tgamma_upper(double a, double z);
extern "C" SEXP _uppergamma(SEXP a, SEXP z) {
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

extern "C" double gamma_p_derivative(double a, double x);

extern "C" SEXP _gammapDer(SEXP a, SEXP z) {
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

extern "C" double gamma_p_inv(double a, double x);
extern "C" SEXP _gammapInv(SEXP a, SEXP z) {
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

extern "C" double gamma_p_inva(double a, double x);
extern "C" SEXP _gammapInva(SEXP a, SEXP z) {
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

extern "C" double gamma_q_inv(double a, double x);
extern "C" SEXP _gammaqInv(SEXP a, SEXP z) {
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

extern "C" double gamma_q_inva(double a, double x);
extern "C" SEXP _gammaqInva(SEXP a, SEXP z) {
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

extern "C" double logit(double x, double low, double high) {
  return _powerD(x, 1.0, 4, low, high);
}

extern "C" double expit(double alpha, double low, double high) {
  return _powerDi(alpha, 1.0, 4, low, high);
}

extern "C" double probit(double x, double low, double high) {
  return _powerD(x, 1.0, 6, low, high);
}

extern "C" double probitInv(double alpha, double low, double high) {
  return _powerDi(alpha, 1.0, 6, low, high);
}

extern "C" double ReLU(double x) {
  return (x > 0.0) ? x : 0.0;
}

extern "C" double dReLU(double x) {
  return (x > 0.0) ? 1.0 : 0.0;
}

//
extern "C" double GELU(double x) {
  return 0.5 * x * (1.0 + erf(x * M_SQRT1_2));
}

extern "C" double dGELU(double x) {
  return 0.5 * (1.0 + erf(x * M_SQRT1_2)) + x * M_1_SQRT_2PI * exp(-0.5 * x * x);
}

extern "C" double d2GELU(double x) {
  return (2.0- x*x) * exp(-0.5* x * x)*M_1_SQRT_2PI;
}

extern "C" double d3GELU(double x) {
  double x2 = x*x;
  return -8.0*x*exp(-x2*0.5)*0.5*M_1_SQRT_2PI + 4.0*x2*x*exp(-x2*0.5)*0.25*M_1_SQRT_2PI;
}

extern "C" double d4GELU(double x) {
  return exp(-0.5*x*x)*M_1_SQRT_2PI*(7.0*x*x - 4.0 - x*x*x*x);
}

extern "C" double ELU(double x, double alpha) {
  return (x > 0.0) ? x : (exp(x) - 1.0) * alpha;
}

// derivative of ELU with respect to x
extern "C" double dELU(double x, double alpha) {
  return (x > 0.0) ? 1.0 : exp(x)*alpha;
}

// derivative of dELU with respect to x
extern "C" double d2ELU(double x, double alpha) {
  return (x > 0.0) ? 0.0 : exp(x)*alpha;
}

// derivative of dELU with respect to alpha
extern "C" double d2aELU(double x, double alpha) {
  return (x > 0.0) ? 0.0 : exp(x);
}

// derivative of ELU with respect to alpha
extern "C" double dELUa(double x, double alpha) {
  return (x > 0.0) ? 0.0 : (exp(x) - 1.0);
}
// derivative of dELAa with respect to x
extern "C" double d2ELUa(double x, double alpha) {
  return (x > 0.0) ? 0.0 : exp(x);
}

extern "C" double softplus(double x) {
  return log(1.0 + exp(x));
}

extern "C" double dsoftplus(double x) {
  return 1.0 / (1.0 + exp(-x));
}

extern "C" double d2softplus(double x) {
  double ex = exp(x);
  return ex / ((1.0 + ex) * (1.0 + ex));
}

extern "C" double d3softplus(double x) {
  double ex = exp(-x);
  double ex1 = (1.0 + ex);
  return 2.0*exp(-2.0*x)/(ex1*ex1*ex1) - 1.0*ex/(ex1*ex1);
}

extern "C" double d4softplus(double x) {
  double ex = exp(-x);
  double ex1 = (1.0 + ex);
  return 6.0*exp(-3.0*x)/(ex1*ex1*ex1*ex1) -
    6.0*exp(-2.0*x)/(ex1*ex1*ex1) +
    ex/(ex1*ex1);
}

extern "C" double SELU(double x) {
#define alpha 1.6732632423543772848170429916717
#define scale 1.0507009873554804934193349852946
  return (x > 0.0) ? scale * x : scale * alpha * (exp(x) - 1.0);
#undef alpha
#undef scale
}

extern "C" double dSELU(double x) {
#define alpha 1.6732632423543772848170429916717
#define scale 1.0507009873554804934193349852946
  return (x > 0.0) ? scale : scale * alpha * exp(x);
#undef alpha
#undef scale
}

extern "C" double lReLU(double x) {
  return (x >= 0.0) ? x : 0.01 * x;
}

extern "C" double dlReLU(double x) {
  return (x > 0.0) ? 1.0 : 0.01;
}

extern "C" double PReLU(double x, double alpha) {
  return (x > 0.0) ? x : alpha * x;
}

extern "C" double dPReLU(double x, double alpha) {
  return (x > 0.0) ? 1.0 : alpha;
}

extern "C" double dPReLUa(double x, double alpha) {
  return (x > 0.0) ? 0.0 : x;
}

extern "C" double dPReLUa1(double x, double alpha) {
  return (x > 0.0) ? 0.0 : 1.0;
}

extern "C" double Swish(double x) {
  return x / (1.0 + exp(-x));
}

extern "C" double dSwish(double x) {
  double ex = exp(-x);
  double den = (1.0 + ex);
  return x*ex/(den*den) + 1.0/den;
}

extern "C" SEXP _rxode2_activationF2(SEXP xS, SEXP aS, SEXP typeS) {
  int type = INTEGER(typeS)[0];
  int typex = TYPEOF(xS);
  int typea = TYPEOF(aS);
  int lenx = Rf_length(xS);
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, lenx));
  for (int i = 0; i < lenx; ++i) {
    double x = (typex == REALSXP) ? REAL(xS)[i] : (double)INTEGER(xS)[i];
    double a = (typea == REALSXP) ? REAL(aS)[i] : (double)INTEGER(aS)[i];
    switch (type) {
    case 1:
      REAL(ret)[i] = ELU(x, a);
      break;
    case 2:
      REAL(ret)[i] = dELU(x, a);
      break;
    case 3:
      REAL(ret)[i] = d2ELU(x, a);
      break;
    case 4:
      REAL(ret)[i] = d2aELU(x, a);
      break;
    case 5:
      REAL(ret)[i] = dELUa(x, a);
      break;
    case 6:
      REAL(ret)[i] = d2ELUa(x, a);
      break;
    case 7:
      REAL(ret)[i] = PReLU(x, a);
      break;
    case 8:
      REAL(ret)[i] = dPReLU(x, a);
      break;
    case 9:
      REAL(ret)[i] = dPReLUa(x, a);
      break;
    case 10:
      REAL(ret)[i] = dPReLUa1(x, a);
      break;
    default:
      REAL(ret)[i] = NA_REAL;
      break;
    }
  }
  UNPROTECT(1);
  return(ret);
}

extern "C" SEXP _rxode2_activationF(SEXP xS, SEXP typeS) {
  int type = INTEGER(typeS)[0];
  int typex = TYPEOF(xS);
  int lenx = Rf_length(xS);
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, lenx));
  for (int i = 0; i < lenx; ++i) {
    double x = (typex == REALSXP) ? REAL(xS)[i] : (double)INTEGER(xS)[i];
    switch (type) {
    case 1:
      REAL(ret)[i] = GELU(x);
      break;
    case 2:
      REAL(ret)[i] = ReLU(x);
      break;
    case 3:
      REAL(ret)[i] = softplus(x);
      break;
    case 4:
      REAL(ret)[i] = SELU(x);
      break;
    case 5:
      REAL(ret)[i] = lReLU(x);
      break;
    case 6:
      REAL(ret)[i] = Swish(x);
      break;
    case 7:
      REAL(ret)[i] = dReLU(x);
      break;
    case 8:
      REAL(ret)[i] = dlReLU(x);
      break;
    case 9:
      REAL(ret)[i] = dGELU(x);
      break;
    case 10:
      REAL(ret)[i] = d2GELU(x);
      break;
    case 11:
      REAL(ret)[i] = d3GELU(x);
      break;
    case 12:
      REAL(ret)[i] = d4GELU(x);
      break;
    case 13:
      REAL(ret)[i] = dsoftplus(x);
      break;
    case 14:
      REAL(ret)[i] = d2softplus(x);
      break;
    case 15:
      REAL(ret)[i] = d3softplus(x);
      break;
    case 16:
      REAL(ret)[i] = d4softplus(x);
      break;
    case 17:
      REAL(ret)[i] = dSELU(x);
      break;
    case 18:
      REAL(ret)[i] = dSwish(x);
      break;
    default:
      REAL(ret)[i] = NA_REAL;
      break;
    }
  }
  UNPROTECT(1);
  return(ret);
}


extern "C" SEXP _rxode2_powerD(SEXP xS, SEXP lowS, SEXP highS, SEXP lambdaS, SEXP yjS, SEXP inverseS) {
  int typex = TYPEOF(xS);
  int typelow = TYPEOF(lowS);
  int typehigh = TYPEOF(highS);
  int typelambda = TYPEOF(lambdaS);
  int inverse = INTEGER(inverseS)[0];
  int yj = INTEGER(yjS)[0];
  double low, high, lambda;
  if (Rf_length(inverseS) != 1) {
    Rf_errorcall(R_NilValue, _("'inverse' must be an logical of length 1"));
  }
  if (Rf_length(yjS) != 1) {
    Rf_errorcall(R_NilValue, _("'yj' must be an integer of length 1"));
  }
  if (Rf_length(lambdaS) != 1){
    Rf_errorcall(R_NilValue, _("'lambda' must be a numeric of length 1"));
  }
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
  if (typelambda == INTSXP) {
    lambda = (double)(INTEGER(lambdaS)[0]);
  } else if (typelambda == REALSXP) {
    lambda = REAL(lambdaS)[0];
  } else {
    Rf_errorcall(R_NilValue, _("'lambda' must be a numeric of length 1"));
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
  if (inverse) {
    if (isD) {
      for (int i = lenx; i--;){
        retD[i] = _powerDi(xD[i], lambda, yj, low, high);
      }
    } else {
      for (int i = lenx; i--;){
        retD[i] = _powerDi((double)(xI[i]), lambda, yj, low, high);
      }
    }
  } else {
    if (isD) {
      for (int i = lenx; i--;){
        retD[i] = _powerD(xD[i], lambda, yj, low, high);
      }
    } else {
      for (int i = lenx; i--;){
        retD[i] = _powerD((double)(xI[i]), lambda, yj, low, high);
      }
    }
  }
  UNPROTECT(1);
  return ret;
}

extern "C" SEXP _vecDF(SEXP cv, SEXP n_) {
  int n=0;
  int typ = TYPEOF(n_);
  if (typ == REALSXP) {
    n = (int)(REAL(n_)[0]);
  } else if (typ == INTSXP) {
    n = INTEGER(n_)[0];
  }
  if (n <= 0) Rf_errorcall(R_NilValue, _("'n' must be greater than 0"));
  int pro = 0;
  int len = Rf_length(cv);
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, len)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, len)); pro++;
  SEXP cvN = Rf_getAttrib(cv, R_NamesSymbol);
  for (int i = len; i--;) {
    SEXP tmp = PROTECT(Rf_allocVector(REALSXP, n)); pro++;
    for (int j = n; j--;) {
      REAL(tmp)[j] = REAL(cv)[i];
    }
    SET_VECTOR_ELT(ret, i, tmp);
    SET_STRING_ELT(retN, i, STRING_ELT(cvN, i));
  }
  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -n;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);
  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,Rf_mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);
  Rf_setAttrib(ret, R_NamesSymbol, retN);
  UNPROTECT(pro);
  return ret;
}

extern "C" SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_) {
  int n = INTEGER(n_)[0];
  if (n <= 0) Rf_errorcall(R_NilValue, _("'n' must be greater than 0"));

  int len1 = Rf_length(et_);
  int len1a = 0;
  if (len1 > 0) {
    len1a = Rf_length(VECTOR_ELT(et_,0));
  }
  SEXP etN = Rf_getAttrib(et_, R_NamesSymbol);

  SEXP matD;
  SEXP matDN;
  int len2;
  int lenOut;
  int lenItem;
  int isNullEt = Rf_isNull(et_) || Rf_length(et_) == 0;
  if (!Rf_isNull(mat_) && !isNullEt) {
    matD = Rf_getAttrib(mat_, Rf_install("dim"));
    matDN = VECTOR_ELT(Rf_getAttrib(mat_, R_DimNamesSymbol), 1);
    len2 = INTEGER(matD)[1];
    lenOut = INTEGER(matD)[0];
    lenItem = lenOut/len1a;
  } else if (!isNullEt) {
    len2 = 0;
    lenOut = n*len1a;
    lenItem = n;
  } else {
    matD = Rf_getAttrib(mat_, Rf_install("dim"));;
    matDN = VECTOR_ELT(Rf_getAttrib(mat_, R_DimNamesSymbol), 1);
    len2 = INTEGER(matD)[1];
    lenOut = INTEGER(matD)[0];
    lenItem = n;
  }
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, len1+len2)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, len1+len2)); pro++;
  for (int i = len1; i--; ) {
    SEXP tmp = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
    SEXP in = VECTOR_ELT(et_, i);
    int l = lenOut;
    for (int j = len1a; j--;) {
      for (int k = lenItem; k--; ) {
        REAL(tmp)[--l] = REAL(in)[j];
      }
    }
    SET_VECTOR_ELT(ret, i, tmp);
    SET_STRING_ELT(retN, i, STRING_ELT(etN, i));
  }
  for (int i = len2; i--; ) {
    SEXP tmp = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
    memcpy(&(REAL(tmp)[0]), &(REAL(mat_)[lenOut*i]), lenOut*sizeof(double));
    SET_VECTOR_ELT(ret, i+len1, tmp);
    SET_STRING_ELT(retN, i+len1, STRING_ELT(matDN, i));
  }
  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);
  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,Rf_mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);
  Rf_setAttrib(ret, R_NamesSymbol, retN);
  UNPROTECT(pro);
  return ret;
}

extern "C" double phi(double q) {
  return pnorm(q, 0.0, 1.0, 1, 0);
}

extern "C" SEXP _rxode2_phi(SEXP q) {
  int type = TYPEOF(q);
  SEXP ret;
  int pro = 0;
  if (type == REALSXP) {
    int len = Rf_length(q);
    ret= PROTECT(Rf_allocVector(REALSXP, len));pro++;
    double *retD = REAL(ret);
    double *inD = REAL(q);
    for (int j = len; j--;){
      retD[j] = phi(inD[j]);
    }
  } else if (type == INTSXP){
    int len = Rf_length(q);
    ret= PROTECT(Rf_allocVector(REALSXP, len));pro++;
    double *retD = REAL(ret);
    int *inD = INTEGER(q);
    for (int j = len; j--;){
      retD[j] = phi((double)(inD[j]));
    }
  } else {
    Rf_errorcall(R_NilValue, _("'phi' requires numeric values"));
  }
  UNPROTECT(pro);
  return ret;
}

#include "../inst/include/rxode2parseHandleEvid.h"

extern "C" SEXP _rxode2_getWh(SEXP in) {
  int wh, cmt, wh100, whI, wh0;
  getWh(INTEGER(in)[0], &wh, &cmt, &wh100, &whI, &wh0);
  SEXP ret = PROTECT(Rf_allocVector(INTSXP, 5));
  int *retI = INTEGER(ret);
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 5));
  retI[0] = wh;
  SET_STRING_ELT(retN, 0,Rf_mkChar("wh"));
  retI[1] = cmt;
  SET_STRING_ELT(retN, 1,Rf_mkChar("cmt"));
  retI[2] = wh100;
  SET_STRING_ELT(retN, 2,Rf_mkChar("wh100"));
  retI[3] = whI;
  SET_STRING_ELT(retN, 3,Rf_mkChar("whI"));
  retI[4] = wh0;
  SET_STRING_ELT(retN, 4,Rf_mkChar("wh0"));
  Rf_setAttrib(ret, R_NamesSymbol, retN);
  UNPROTECT(2);
  return ret;
}

extern "C" SEXP _rxode2_getClassicEvid(SEXP cmtS, SEXP amtS, SEXP rateS,
                                       SEXP durS, SEXP iiS, SEXP evidS, SEXP ssS) {
  int *cmt= INTEGER(cmtS);
  double *amt = REAL(amtS);
  double *dur = REAL(durS);
  double *rate = REAL(rateS);
  double *ii = REAL(iiS);
  int *evid = INTEGER(evidS);
  double *ss = REAL(ssS);
  SEXP retS = PROTECT(Rf_allocVector(INTSXP, Rf_length(cmtS)));
  int *ret = INTEGER(retS);
  for (int i = Rf_length(cmtS); i--;) {
    ret[i] = getEvidClassic(cmt[i], amt[i], rate[i], dur[i], ii[i], evid[i], ss[i]);
  }
  UNPROTECT(1);
  return retS;
}
