#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "../inst/include/rxode2.h"
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif


SEXP _rxode2_codeLoaded() {
  static SEXP (*fun)()=NULL;
  if (fun == NULL) fun = (SEXP (*)()) R_GetCCallable("rxode2parse","_rxode2parse_codeLoaded");
  return fun();
}

SEXP _rxode2_codegen(SEXP c_file, SEXP prefix, SEXP libname, SEXP pMd5, SEXP timeId, SEXP lastMv) {
  static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)=NULL;
  if (fun == NULL){
    fun = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("rxode2parse","_rxode2parse_codegen");
  } 
  return fun(c_file, prefix, libname, pMd5, timeId, lastMv);
}

SEXP _rxode2_parseModel(SEXP type) {
  static SEXP (*fun)(SEXP)=NULL;
  if (fun == NULL){
    fun = (SEXP (*)(SEXP)) R_GetCCallable("rxode2parse","_rxode2parse_parseModel");
  } 
  return fun(type);
}

SEXP _rxode2_isLinCmt() {
  static SEXP (*fun)()=NULL;
  if (fun == NULL) fun = (SEXP (*)()) R_GetCCallable("rxode2parse","_rxode2parse_isLinCmt");
  return fun();
}

SEXP _rxode2_trans(SEXP parse_file, SEXP prefix, SEXP model_md5, SEXP parseStr,
                   SEXP isEscIn, SEXP inME, SEXP goodFuns, SEXP fullPrintIn) {
  static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("rxode2parse","_rxode2parse_trans");
  }
  return fun(parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn);
}


SEXP _linCmtParse(SEXP vars, SEXP inStr, SEXP verbose) {
  static SEXP (*fun)(SEXP, SEXP, SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rxode2parse","_rxode2parse_linCmtParse");
  }
  return fun(vars, inStr, verbose);
}

SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose) {
  static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("rxode2parse","_rxode2parse_linCmtGen");
  }
  return fun(linCmt, vars, linCmtSens, verbose);
}

void parseFree(int last) {
  static void (*fun)(int)=NULL;
  if (fun == NULL) {
    fun = (void (*)(int)) R_GetCCallable("rxode2parse","_rxode2parse_parseFree");
  }
  fun(last);
}

SEXP _calcDerived(SEXP ncmtSXP, SEXP transSXP, SEXP inp, SEXP sigdigSXP) {
  static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("rxode2parse","_rxode2parse_calcDerived");
  }
  return fun(ncmtSXP, transSXP, inp, sigdigSXP);
}
