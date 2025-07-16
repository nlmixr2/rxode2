#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

int _rxIsEt(SEXP objSexp) {
  int pro = 0;
  SEXP cls = PROTECT(Rf_getAttrib(objSexp, R_ClassSymbol)); pro++;
  if (Rf_isNull(cls)) {
    UNPROTECT(pro);
    return 0;
  }
  if (TYPEOF(objSexp) != VECSXP) {
    UNPROTECT(pro);
    return 0;
  }
  if (!Rf_inherits(objSexp, "rxEt")) {
    UNPROTECT(pro);
    return 0;
  }
  SEXP dfs = PROTECT(Rf_allocVector(STRSXP,1)); pro++;
  SET_STRING_ELT(dfs, 0, Rf_mkChar("data.frame"));

  if (Rf_length(objSexp) != 12) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    UNPROTECT(pro);
    return 0;
  }
  SEXP ce = PROTECT(Rf_getAttrib(cls, Rf_install(".rxode2.lst"))); pro++;
  if (TYPEOF(ce) != VECSXP) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    UNPROTECT(pro);
    return 0;
  }
  SEXP cen = PROTECT(Rf_getAttrib(ce, R_NamesSymbol)); pro++;
  if (TYPEOF(cen) != STRSXP) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    UNPROTECT(pro);
    return 0;
  }
  int nobs = -1, ndose = -1;
  for (unsigned int i = Rf_length(cen); i--;) {
    const char *curChar= CHAR(STRING_ELT(cen, i));
    if (!strcmp(curChar, "nobs")) {
      SEXP nobsSexp = PROTECT(VECTOR_ELT(ce, i)); pro++;
      if (TYPEOF(nobsSexp) == REALSXP) {
        nobs = (int)(REAL(nobsSexp)[0]);
      } else if (TYPEOF(nobsSexp) == INTSXP) {
        nobs = INTEGER(nobsSexp)[0];
      } else {
        Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
        UNPROTECT(pro);
        return 0;
      }
      if (ndose != -1) break;
    } else if (!strcmp(curChar, "ndose")) {
      SEXP ndoseSexp = PROTECT(VECTOR_ELT(ce, i)); pro++;
      if (TYPEOF(ndoseSexp) == REALSXP) {
        ndose = (int)(REAL(ndoseSexp)[0]);
      } else if (TYPEOF(ndoseSexp) == INTSXP) {
        ndose = INTEGER(ndoseSexp)[0];
      } else {
        Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
        UNPROTECT(pro);
        return 0;
      }
      if (nobs != -1) break;
    }
  }
  SEXP o1 = PROTECT(VECTOR_ELT(objSexp, 0)); pro++;
  if (Rf_length(o1) != ndose + nobs) {
    Rf_setAttrib(objSexp, R_ClassSymbol, dfs);
    UNPROTECT(pro);
    return 0;
  }
  UNPROTECT(pro);
  return 1;
}

SEXP _rxode2_rxIsEt2(SEXP objSexp) {
  SEXP dfs = PROTECT(Rf_allocVector(LGLSXP,1));
  INTEGER(dfs)[0] = _rxIsEt(objSexp);
  UNPROTECT(1);
  return dfs;
}
