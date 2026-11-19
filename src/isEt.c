#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "rxProtect.h"

int _rxIsEt(SEXP objSexp) {
  rxProtectGuard;
  SEXP cls = rxP(Rf_getAttrib(objSexp, R_ClassSymbol));
  if (Rf_isNull(cls)) {
    rxUPAll();
    return 0;
  }
  if (TYPEOF(objSexp) != VECSXP) {
    rxUPAll();
    return 0;
  }
  if (!Rf_inherits(objSexp, "rxEt")) {
    rxUPAll();
    return 0;
  }
  SEXP dfs = rxP(Rf_allocVector(STRSXP,1));
  SET_STRING_ELT(dfs, 0, Rf_mkChar("data.frame"));

  if (Rf_length(objSexp) != 12) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    rxUPAll();
    return 0;
  }
  SEXP ce = rxP(Rf_getAttrib(cls, Rf_install(".rxode2.lst")));
  if (TYPEOF(ce) != VECSXP) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    rxUPAll();
    return 0;
  }
  SEXP cen = rxP(Rf_getAttrib(ce, R_NamesSymbol));
  if (TYPEOF(cen) != STRSXP) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    rxUPAll();
    return 0;
  }
  int nobs = -1, ndose = -1;
  for (unsigned int i = Rf_length(cen); i--;) {
    const char *curChar= CHAR(STRING_ELT(cen, i));
    if (!strcmp(curChar, "nobs")) {
      SEXP nobsSexp = rxP(VECTOR_ELT(ce, i));
      if (TYPEOF(nobsSexp) == REALSXP) {
        nobs = (int)(REAL(nobsSexp)[0]);
      } else if (TYPEOF(nobsSexp) == INTSXP) {
        nobs = INTEGER(nobsSexp)[0];
      } else {
        Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
        rxUPAll();
        return 0;
      }
      if (ndose != -1) break;
    } else if (!strcmp(curChar, "ndose")) {
      SEXP ndoseSexp = rxP(VECTOR_ELT(ce, i));
      if (TYPEOF(ndoseSexp) == REALSXP) {
        ndose = (int)(REAL(ndoseSexp)[0]);
      } else if (TYPEOF(ndoseSexp) == INTSXP) {
        ndose = INTEGER(ndoseSexp)[0];
      } else {
        Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
        rxUPAll();
        return 0;
      }
      if (nobs != -1) break;
    }
  }
  SEXP o1 = rxP(VECTOR_ELT(objSexp, 0));
  if (Rf_length(o1) != ndose + nobs) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    rxUPAll();
    return 0;
  }
  rxUPAll();
  return 1;
}

SEXP _rxode2_rxIsEt2(SEXP objSexp) {
  rxProtectGuard;
  SEXP dfs = rxP(Rf_allocVector(LGLSXP,1));
  INTEGER(dfs)[0] = _rxIsEt(objSexp);
  rxUPAll();
  return dfs;
}
