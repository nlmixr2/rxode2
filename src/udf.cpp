#define USE_FC_LEN_T
#define STRICT_R_HEADER
#include <Rcpp.h>
using namespace Rcpp;

Function getRxFn(std::string name);

extern "C" SEXP _rxode2_assignUdf(SEXP in) {
  BEGIN_RCPP
    if (Rf_length(in) == 0 || Rf_length(in) == 1) {
      return wrap(LogicalVector::create(false));
    }
  if (TYPEOF(in) != INTSXP) {
    return wrap(LogicalVector::create(false));
  }
  if (Rf_isNull(Rf_getAttrib(in, R_NamesSymbol))) {
    return wrap(LogicalVector::create(false));
  }
  Function fun = as<Function>(getRxFn(".setupUdf"));
  LogicalVector needRecompile = fun(in);
  return wrap(needRecompile);
  END_RCPP
}

extern "C" SEXP _rxode2_udfEnvSet(SEXP udf) {
  BEGIN_RCPP
    if (Rf_isNull(udf)) {
      return R_NilValue;
    }
  if (Rf_length(udf) == 0 || Rf_length(udf) == 1) {
    return R_NilValue;
  }
  if (TYPEOF(udf) != INTSXP) {
    return R_NilValue;
  }
  if (Rf_isNull(Rf_getAttrib(udf, R_NamesSymbol))) {
    return R_NilValue;
  }
  Function fun = as<Function>(getRxFn(".udfEnvSetUdf"));
  fun(udf);
  return R_NilValue;
  END_RCPP
    }

extern "C" SEXP _rxode2_udfReset() {
  BEGIN_RCPP
    Function fun2 = as<Function>(getRxFn(".udfEnvReset"));
  fun2();
  return R_NilValue;
  END_RCPP
    }


extern "C" SEXP rxode2_getUdf2(const char *fun, const int nargs) {
BEGIN_RCPP
  Function rxode2_getUdf_ = as<Function>(getRxFn(".getUdfInfo"));
  return rxode2_getUdf_(fun, nargs);
END_RCPP
}

extern "C" SEXP _rxode2_evalUdfS(const char *fun, int n, const double *args) {
BEGIN_RCPP
  Function rxode2_evalUdf = as<Function>(getRxFn(".udfCall"));
  List retL(n);
  CharacterVector funC(1);
  funC = fun;
  for (int i = 0; i < n; ++i) {
    NumericVector nv(1);
    nv[0] = args[i];
    retL[i] = nv;
  }
  NumericVector ret0 = rxode2_evalUdf(funC, retL);
  NumericVector ret(1);
  ret[0] = ret0[0];
  return wrap(ret);
END_RCPP
}

extern "C" double _rxode2_evalUdf(const char *fun, int n, const double *args) {
  SEXP ret = PROTECT(_rxode2_evalUdfS(fun, n, args));
  double r = REAL(ret)[0];
  UNPROTECT(1);
  return r;
}

extern "C" SEXP _rxode2_resetUdf() {
BEGIN_RCPP
  Function resetUdf = as<Function>(getRxFn(".udfReset"));
  resetUdf();
  return R_NilValue;
END_RCPP
}

extern "C" SEXP _rxode2parse_getUdf() {
BEGIN_RCPP
  Function getUdf = as<Function>(getRxFn(".udfInfo"));
  return getUdf();
END_RCPP
}
