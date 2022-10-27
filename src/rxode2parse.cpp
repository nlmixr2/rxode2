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

extern "C" SEXP _rxode2_codeLoaded(void) {
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".codeLoaded"]);
  return fun();
}

extern "C" SEXP _rxode2_codegen(SEXP c_file, SEXP prefix, SEXP libname, SEXP pMd5, SEXP timeId, SEXP lastMv) {
  BEGIN_RCPP
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".codegen"]);
  return fun(c_file, prefix, libname, pMd5, timeId, lastMv);
  END_RCPP
}

extern "C" SEXP _rxode2_parseModel(SEXP type) {
  BEGIN_RCPP
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
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
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".trans"]);
  return fun(parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn);
  END_RCPP
}

extern "C" SEXP _linCmtParse(SEXP vars, SEXP inStr, SEXP verbose) {
  BEGIN_RCPP
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".linCmtParse"]);
  return fun(vars, inStr, verbose);
  END_RCPP
}

extern "C" SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose) {
  BEGIN_RCPP
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".linCmtGen"]);
  return fun(linCmt, vars, linCmtSens, verbose);
  END_RCPP
}

extern "C" SEXP parseFreeSexp(SEXP last) {
  BEGIN_RCPP
    if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".parseFreeSexp"]);
  return fun(last);
  END_RCPP
}


extern "C" void parseFree(int last) {
  BEGIN_RCPP
  SEXP iv = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(iv)[0] = last;
  parseFreeSexp(iv);
  UNPROTECT(1);
   END_RCPP
}


extern "C" SEXP _calcDerived(SEXP ncmtSXP, SEXP transSXP, SEXP inp, SEXP sigdigSXP) {
  BEGIN_RCPP
  if (!rxode2parse_loaded) {
    rxode2parse_loaded = true;
    rxode2parse = loadNamespace("rxode2parse");
  }
  Function fun = as<Function>(rxode2parse[".calcDerived"]);
  return fun(ncmtSXP, transSXP, inp, sigdigSXP);
  END_RCPP
}
