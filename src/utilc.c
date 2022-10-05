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


SEXP _vecDF(SEXP cv, SEXP n_) {
  int n=0;
  int typ = TYPEOF(n_);
  if (typ == REALSXP) {
    n = (int)(REAL(n_)[0]);
  } else if (typ == INTSXP) {
    n = INTEGER(n_)[0];
  }
  if (n <= 0) Rf_errorcall(R_NilValue, _("'n' must be greater than 0"));
  int pro = 0;
  int len = length(cv);
  SEXP ret = PROTECT(allocVector(VECSXP, len)); pro++;
  SEXP retN = PROTECT(allocVector(STRSXP, len)); pro++;
  SEXP cvN = getAttrib(cv, R_NamesSymbol);
  for (int i = len; i--;) {
    SEXP tmp = PROTECT(allocVector(REALSXP, n)); pro++;
    for (int j = n; j--;) {
      REAL(tmp)[j] = REAL(cv)[i];
    }
    SET_VECTOR_ELT(ret, i, tmp);
    SET_STRING_ELT(retN, i, STRING_ELT(cvN, i));
  }
  SEXP sexp_rownames = PROTECT(allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -n;
  setAttrib(ret, R_RowNamesSymbol, sexp_rownames);
  SEXP sexp_class = PROTECT(allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,mkChar("data.frame"));
  setAttrib(ret, R_ClassSymbol, sexp_class);
  setAttrib(ret, R_NamesSymbol, retN);
  UNPROTECT(pro);
  return ret;
}

SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_) {
  int n = INTEGER(n_)[0];
  if (n <= 0) Rf_errorcall(R_NilValue, _("'n' must be greater than 0"));
  
  int len1 = length(et_);
  int len1a = 0;
  if (len1 > 0) {
    len1a = length(VECTOR_ELT(et_,0));
  }
  SEXP etN = getAttrib(et_, R_NamesSymbol);

  SEXP matD;
  SEXP matDN;
  int len2;
  int lenOut;
  int lenItem;
  int isNullEt = Rf_isNull(et_) || Rf_length(et_) == 0;
  if (!Rf_isNull(mat_) && !isNullEt) {
    matD = getAttrib(mat_, install("dim"));
    matDN = VECTOR_ELT(getAttrib(mat_, R_DimNamesSymbol), 1);
    len2 = INTEGER(matD)[1];
    lenOut = INTEGER(matD)[0];
    lenItem = lenOut/len1a;
  } else if (!isNullEt) {
    len2 = 0;
    lenOut = n*len1a;
    lenItem = n;
  } else {
    matD = getAttrib(mat_, install("dim"));;
    matDN = VECTOR_ELT(getAttrib(mat_, R_DimNamesSymbol), 1);
    len2 = INTEGER(matD)[1];
    lenOut = INTEGER(matD)[0];
    lenItem = n;
  }
  int pro = 0;
  SEXP ret = PROTECT(allocVector(VECSXP, len1+len2)); pro++;
  SEXP retN = PROTECT(allocVector(STRSXP, len1+len2)); pro++;
  for (int i = len1; i--; ) {
    SEXP tmp = PROTECT(allocVector(REALSXP, lenOut)); pro++;
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
    SEXP tmp = PROTECT(allocVector(REALSXP, lenOut)); pro++;
    memcpy(&(REAL(tmp)[0]), &(REAL(mat_)[lenOut*i]), lenOut*sizeof(double));
    SET_VECTOR_ELT(ret, i+len1, tmp);
    SET_STRING_ELT(retN, i+len1, STRING_ELT(matDN, i));
  }
  SEXP sexp_rownames = PROTECT(allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  setAttrib(ret, R_RowNamesSymbol, sexp_rownames);
  SEXP sexp_class = PROTECT(allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,mkChar("data.frame"));
  setAttrib(ret, R_ClassSymbol, sexp_class);
  setAttrib(ret, R_NamesSymbol, retN);
  UNPROTECT(pro);
  return ret;
}

double phi(double q) {
  return pnorm(q, 0.0, 1.0, 1, 0);
}

SEXP _phi(SEXP q) {
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
