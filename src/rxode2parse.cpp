#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

using namespace Rcpp;
using namespace arma;

extern Function loadNamespace;
bool rxode2parse_loaded = false;
Environment rxode2parse;

extern "C" {
  typedef SEXP (*_rxode2_convertId_type)(SEXP);

  _rxode2_convertId_type _rxode2parse__convertId_;

  typedef SEXP (*_rxode2_etTransParse_type)(SEXP, SEXP, SEXP, SEXP, SEXP,
                                            SEXP, SEXP, SEXP, SEXP, SEXP,
                                            SEXP);
  _rxode2_etTransParse_type _rxode2_etTransParseP;

  typedef SEXP (*_rxode2_chin_type)(SEXP, SEXP);

  _rxode2_chin_type _rxode2_chin;
  typedef SEXP (*_rxode2parse_getForder_type)(void);
  _rxode2parse_getForder_type getForder;
  typedef int (*_rxode2parse_useForder_type)(void);
  _rxode2parse_useForder_type useForder;

}

extern "C" SEXP assignRxode2ParsePtrs(void) {
  BEGIN_RCPP
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
    Function funPtrs = rxode2parse[".rxode2parseFunPtrs"];
    List ptr = as<List>(funPtrs());
    _rxode2parse__convertId_ = (_rxode2_convertId_type)(R_ExternalPtrAddr(ptr[0]));
    _rxode2_etTransParseP=(_rxode2_etTransParse_type) (R_ExternalPtrAddr(ptr[2]));
    _rxode2_chin=(_rxode2_chin_type) (R_ExternalPtrAddr(ptr[3]));
    getForder=(_rxode2parse_getForder_type) (R_ExternalPtrAddr(ptr[4]));
    useForder=(_rxode2parse_useForder_type) (R_ExternalPtrAddr(ptr[5]));
  }
  END_RCPP
}

extern "C" SEXP _rxode2_convertId_(SEXP id) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  return _rxode2parse__convertId_(id);
  END_RCPP
}

extern "C" SEXP chin(SEXP a, SEXP b) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  return _rxode2_chin(a, b);
  END_RCPP
}

extern "C" SEXP _rxode2_codeLoaded(void) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".codeLoaded"]);
  return fun();
  END_RCPP
}

extern "C" SEXP _rxode2parse_rxC(SEXP in) {
BEGIN_RCPP
  if (TYPEOF(in) != STRSXP) return R_NilValue;
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".rxC"]);
  return wrap(fun(in));
END_RCPP
}

extern "C" SEXP _rxode2parse_assignUdf(SEXP in) {
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
 assignRxode2ParsePtrs();
 Function fun = as<Function>(rxode2parse[".setupUdf"]);
 LogicalVector needRecompile = fun(in);
 return wrap(needRecompile);
END_RCPP
}

extern "C" SEXP _rxode2parse_udfEnvSet(SEXP udf) {
BEGIN_RCPP
  if (Rf_length(udf) == 0 || Rf_length(udf) == 1) {
    return R_NilValue;
  }
 if (TYPEOF(udf) != INTSXP) {
   return R_NilValue;
 }
 if (Rf_isNull(Rf_getAttrib(udf, R_NamesSymbol))) {
   return R_NilValue;
 }
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".udfEnvSetUdf"]);
  fun(udf);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP _rxode2parse_udfReset() {
  BEGIN_RCPP
    assignRxode2ParsePtrs();
  Function fun2 = as<Function>(rxode2parse[".udfEnvReset"]);
  fun2();
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP _rxode2_codegen(SEXP c_file, SEXP prefix, SEXP libname, SEXP pMd5, SEXP timeId, SEXP lastMv, SEXP goodFuns) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".codegen"]);
  return fun(c_file, prefix, libname, pMd5, timeId, lastMv, goodFuns);
  END_RCPP
}

extern "C" SEXP _rxode2_parseModel(SEXP type) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".parseModel"]);
  return fun(type);
  END_RCPP
}

extern "C" SEXP _rxode2_isLinCmt(void) {
  BEGIN_RCPP
    if (!rxode2parse_loaded) {
      rxode2parse_loaded = true;
      rxode2parse = loadNamespace("rxode2parse");
    }
  Function fun = as<Function>(rxode2parse[".isLinCmt"]);
  return fun();
  END_RCPP
}

extern "C" SEXP _rxode2_trans(SEXP parse_file, SEXP prefix, SEXP model_md5, SEXP parseStr,
                              SEXP isEscIn, SEXP inME, SEXP goodFuns, SEXP fullPrintIn) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".trans"]);
  return fun(parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn);
  END_RCPP
}

extern "C" SEXP _linCmtParse(SEXP vars, SEXP inStr, SEXP verbose) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".linCmtParse"]);
  return fun(vars, inStr, verbose);
  END_RCPP
}

extern "C" SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".linCmtGen"]);
  return fun(linCmt, vars, linCmtSens, verbose);
  END_RCPP
}

extern "C" SEXP parseFreeSexp(SEXP last) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".parseFreeSexp"]);
  return fun(last);
  END_RCPP
}


extern "C" void parseFree(int last) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  SEXP iv = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(iv)[0] = last;
  parseFreeSexp(iv);
  UNPROTECT(1);
  VOID_END_RCPP
}


extern "C" SEXP _calcDerived(SEXP ncmtSXP, SEXP transSXP, SEXP inp, SEXP sigdigSXP) {
  BEGIN_RCPP
  assignRxode2ParsePtrs();
  Function fun = as<Function>(rxode2parse[".calcDerived"]);
  return fun(ncmtSXP, transSXP, inp, sigdigSXP);
  END_RCPP
}
