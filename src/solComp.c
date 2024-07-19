#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
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
#include "solComp.h"

SEXP _rxode2_solComp2(SEXP sK10, SEXP sK12, SEXP sK21) {
  int pro = 0;
  SEXP L = PROTECT(allocVector(REALSXP, 2)); pro++;
  SEXP C1 = PROTECT(allocVector(REALSXP, 4)); pro++;
  SEXP dm = PROTECT(allocVector(INTSXP, 2)); pro++;
  SEXP C2 = PROTECT(allocVector(REALSXP, 4)); pro++;
  int *dmi = INTEGER(dm);
  dmi[0] = dmi[1] = 2;
  Rf_setAttrib(C1, R_DimSymbol, dm);
  Rf_setAttrib(C2, R_DimSymbol, dm);
  if (!solComp2C(REAL(sK10), REAL(sK12), REAL(sK21),
                REAL(L), REAL(C1), REAL(C2))) {
    UNPROTECT(pro);
    return R_NilValue;
  }
  SEXP lst   = PROTECT(allocVector(VECSXP, 3)); pro++;
  SEXP names = PROTECT(allocVector(STRSXP, 3)); pro++;
  SET_STRING_ELT(names,0,mkChar("L"));
  SET_VECTOR_ELT(lst,  0, L);
  SET_STRING_ELT(names,1,mkChar("C1"));
  SET_VECTOR_ELT(lst,  1, C1);
  SET_STRING_ELT(names,2,mkChar("C2"));
  SET_VECTOR_ELT(lst,  2, C2);
  Rf_setAttrib(lst, R_NamesSymbol, names);
  UNPROTECT(pro);
  return lst;
}

SEXP _rxode2_solComp3(SEXP sK10, SEXP sK12, SEXP sK21, SEXP sK13, SEXP sK31) {
  int pro=0;
  SEXP L = PROTECT(allocVector(REALSXP, 3)); pro++;
  SEXP C1 = PROTECT(allocVector(REALSXP, 9)); pro++;
  SEXP dm = PROTECT(allocVector(INTSXP, 2)); pro++;
  SEXP C2 = PROTECT(allocVector(REALSXP, 9)); pro++;
  SEXP C3 = PROTECT(allocVector(REALSXP, 9)); pro++;
  int *dmi = INTEGER(dm);
  dmi[0] = dmi[1] = 3;
  Rf_setAttrib(C1, R_DimSymbol, dm);
  Rf_setAttrib(C2, R_DimSymbol, dm);
  Rf_setAttrib(C3, R_DimSymbol, dm);
  if (!solComp3C(REAL(sK10), REAL(sK12), REAL(sK21), REAL(sK13), REAL(sK31),
                 REAL(L), REAL(C1), REAL(C2), REAL(C3))) {
    UNPROTECT(pro);
    return R_NilValue;
  }
  SEXP lst   = PROTECT(allocVector(VECSXP, 4)); pro++;
  SEXP names = PROTECT(allocVector(STRSXP, 4)); pro++;
  SET_STRING_ELT(names,0,mkChar("L"));
  SET_VECTOR_ELT(lst,  0, L);
  SET_STRING_ELT(names,1,mkChar("C1"));
  SET_VECTOR_ELT(lst,  1, C1);
  SET_STRING_ELT(names,2,mkChar("C2"));
  SET_VECTOR_ELT(lst,  2, C2);
  SET_STRING_ELT(names,3,mkChar("C3"));
  SET_VECTOR_ELT(lst,  3, C3);
  Rf_setAttrib(lst, R_NamesSymbol, names);
  UNPROTECT(pro);
  return lst;
}
