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

bool qtest(SEXP in, const char *test) {
  static bool (*fun)(SEXP, const char *)=NULL;
  if (fun == NULL) {
    fun = (bool (*)(SEXP, const char *)) R_GetCCallable("rxode2random","_rxode2random_qtest");
  }
  return fun(in, test);
}

SEXP qstrictS(SEXP nn, const char *what) {
  static SEXP (*fun)(SEXP, const char *)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, const char *)) R_GetCCallable("rxode2random","_rxode2random_qstrictS");
  }
  return fun(nn, what);
}

SEXP qstrictSn(SEXP x_, const char *what) {
  static SEXP (*fun)(SEXP, const char *)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, const char *)) R_GetCCallable("rxode2random","_rxode2random_qstrictSn");
  }
  return fun(x_, what);
}
SEXP qstrictSdn(SEXP x_, const char *what) {
  static SEXP (*fun)(SEXP, const char *)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, const char *)) R_GetCCallable("rxode2random","_rxode2random_qstrictSdn");
  }
  return fun(x_, what);
}

SEXP qassertS(SEXP in, const char *test, const char *what) {
  static SEXP (*fun)(SEXP, const char *, const char *)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, const char *, const char *)) R_GetCCallable("rxode2random","_rxode2random_qassertS");
  }
  return fun(in, test, what);
}

SEXP _vecDF(SEXP cv, SEXP n_) {
  static SEXP (*fun)(SEXP, SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_vecDF");
  }
  return fun(cv, n_);
}

SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_) {
  static SEXP (*fun)(SEXP, SEXP, SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_cbindOme");
  }
  return fun(et_, mat_, n_);
}

SEXP _phi(SEXP q) {
  static SEXP (*fun)(SEXP)=NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP)) R_GetCCallable("rxode2random","__rxode2random_phi");
  }
  return fun(q);
}

SEXP _rxSetSeed(SEXP x) {
  static SEXP (*fun)(SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP)) R_GetCCallable("rxode2random","_rxode2random_rxSetSeed");
  }
  return fun(x);
}


SEXP _rxode2_nestingInfo_(SEXP omegaSEXP, SEXP dataSEXP) {
  static SEXP (*fun)(SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_nestingInfo_");
  }
  return fun(omegaSEXP, dataSEXP);
}

SEXP _rxode2_cvPost_(SEXP nuSSEXP, SEXP omegaSSEXP, SEXP nSSEXP, SEXP omegaIsCholSSEXP, SEXP returnCholSSEXP, SEXP typeSSEXP, SEXP diagXformTypeSSEXP) {
  static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_cvPost_");
  }
  return fun(nuSSEXP, omegaSSEXP, nSSEXP, omegaIsCholSSEXP, returnCholSSEXP, typeSSEXP, diagXformTypeSSEXP);
}

SEXP _rxode2_rinvchisq(SEXP nSEXP, SEXP nuSEXP, SEXP scaleSEXP) {
  static SEXP (*fun)(SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_rinvchisq_");
  }
  return fun(nSEXP, nuSEXP, scaleSEXP);  
}

SEXP _rxode2_rLKJ1(SEXP dSEXP, SEXP etaSEXP, SEXP choleskySEXP) {
  static SEXP (*fun)(SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_rLKJ1");
  }
  return fun(dSEXP, etaSEXP, choleskySEXP);
}

SEXP _rxode2_rLKJcv1(SEXP sdSEXP, SEXP etaSEXP) {
  static SEXP (*fun)(SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_rLKJcv1");
  }
  return fun(sdSEXP, etaSEXP);
}

SEXP _rxode2_rLKJcvLsd1(SEXP logSdSEXP, SEXP logSdSDSEXP, SEXP etaSEXP) {
  static SEXP (*fun)(SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_rLKJcvLsd1");
  }
  return fun(logSdSEXP, logSdSDSEXP, etaSEXP);
}

SEXP _rxode2_rcvC1(SEXP sdEstSEXP, SEXP nuSEXP, SEXP diagXformTypeSEXP, SEXP rTypeSEXP, SEXP returnCholSEXP) {
    static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("rxode2random","_rxode2random_rcvC1");
  }
  return fun(sdEstSEXP, nuSEXP, diagXformTypeSEXP, rTypeSEXP, returnCholSEXP);
}
