#define USE_FC_LEN_T
#define STRICT_R_HEADER
#include <Rcpp.h>
using namespace Rcpp;

Function loadNamespace("loadNamespace", R_BaseNamespace);
//Function requireNamespace("requireNamespace", R_BaseNamespace);

extern "C" SEXP rxode2parse_getUdf2(const char *fun, const int nargs) {
BEGIN_RCPP
  Environment rxode2parseNS = loadNamespace("rxode2parse");
  Function rxode2parse_getUdf_ = as<Function>(rxode2parseNS[".getUdfInfo"]);
  return rxode2parse_getUdf_(fun, nargs);
END_RCPP
}

extern "C" SEXP _rxode2parse_evalUdfS(const char *fun, int n, const double *args) {
BEGIN_RCPP
  Environment rxode2parseNS = loadNamespace("rxode2parse");
  Function rxode2parse_evalUdf = as<Function>(rxode2parseNS[".udfCall"]);
  List retL(n);
  CharacterVector funC(1);
  funC = fun;
  for (int i = 0; i < n; ++i) {
    NumericVector nv(1);
    nv[0] = args[i];
    retL[i] = nv;
  }
  NumericVector ret0 = rxode2parse_evalUdf(funC, retL);
  NumericVector ret(1);
  ret[0] = ret0[0];
  return wrap(ret);
END_RCPP
}

extern "C" double _rxode2parse_evalUdf(const char *fun, int n, const double *args) {
  SEXP ret = PROTECT(_rxode2parse_evalUdfS(fun, n, args));
  double r = REAL(ret)[0];
  UNPROTECT(1);
  return r;
}

extern "C" SEXP _rxode2parse_resetUdf() {
BEGIN_RCPP
  Environment rxode2parseNS = loadNamespace("rxode2parse");
  Function resetUdf = as<Function>(rxode2parseNS[".udfReset"]);
  resetUdf();
  return R_NilValue;
END_RCPP
}

extern "C" SEXP _rxode2parse_getUdf() {
BEGIN_RCPP
  Environment rxode2parseNS = loadNamespace("rxode2parse");
  Function getUdf = as<Function>(rxode2parseNS[".udfInfo"]);
  return getUdf();
END_RCPP
}
