#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>

int R_get_option(const char *option, int def) {
  SEXP s, t;
  int ret, pro=0;
  PROTECT(t = s = allocList(3));pro++;
  SET_TYPEOF(s, LANGSXP);
  SETCAR(t, install("getOption")); t = CDR(t);
  SETCAR(t, mkString(option)); t = CDR(t);
  if (def){
    SETCAR(t, ScalarLogical(1));
  } else {
    SETCAR(t, ScalarLogical(0));
  }
  ret = INTEGER(eval(s,R_GlobalEnv))[0];
  UNPROTECT(pro);
  return ret;
}
