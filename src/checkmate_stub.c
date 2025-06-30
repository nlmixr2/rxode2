#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include "checkmate.h"
#include <ctype.h>

// Some unexported functions which were re-implemented here (with some changes)

R_xlen_t find_missing_string(SEXP x) {
    if (STRING_NO_NA(x))
        return 0;
    const R_xlen_t nx = Rf_xlength(x);
    for (R_xlen_t i = 0; i < nx; i++) {
        if (STRING_ELT(x, i) == NA_STRING)
            return i + 1;
    }
    return 0;
}
R_xlen_t check_strict_names(SEXP x) {
    const R_xlen_t nx = Rf_xlength(x);
    const char *str;
    for (R_xlen_t i = 0; i < nx; i++) {
        str = CHAR(STRING_ELT(x, i));
        while (*str == '.')
            str++;
        if (!isalpha(*str))
            return i + 1;
        for (; *str != '\0'; str++) {
            if (!isalnum(*str) && *str != '.' && *str != '_')
                return i + 1;
        }
    }
    return 0;
}

// Modified by Matt
void qstrict0(SEXP nn, const char *what){
  R_xlen_t pos = find_missing_string(nn);
  if (pos > 0) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, "Must have %s, but is NA at position %i", what, (int)(pos));
  }
  pos = Rf_any_duplicated(nn, FALSE);

  if (pos > 0) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, "Must have unique %s, but element %i is duplicated", what, (int)(pos));
  }

  if (Rf_isNull(nn)) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, "Must have %s", what);
  }
  pos = Rf_any_duplicated(nn, FALSE);
  if (pos > 0){
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, "Must have unique %s, but element %i is duplicated", what, (int)(pos));
  }
  pos = check_strict_names(nn);
  if (pos > 0){
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, "Must have %s according to R's variable naming conventions, but element %i does not comply", what, (int)(pos));
  }
  UNPROTECT(1);
}

// By Matt
void qstrict(SEXP x, const char *what) {
  SEXP nn = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  qstrict0(nn, what);
  UNPROTECT(1);
}
