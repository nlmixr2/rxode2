#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>

int R_get_option(const char *option, int def) {
  SEXP s, t;
  int ret, pro=0;
  PROTECT(t = s = LCONS(R_NilValue, Rf_allocList(2)));pro++;
  SETCAR(t,  Rf_install("getOption")); t = CDR(t);
  SETCAR(t, Rf_mkString(option)); t = CDR(t);
  if (def){
    SETCAR(t, Rf_ScalarLogical(1));
  } else {
    SETCAR(t, Rf_ScalarLogical(0));
  }
  ret = INTEGER(Rf_eval(s,R_GlobalEnv))[0];
  UNPROTECT(pro);
  return ret;
}
