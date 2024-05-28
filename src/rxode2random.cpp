#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "../inst/include/rxode2.h"
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

#include "seed.h"

using namespace Rcpp;
extern Function loadNamespace;

#define rxode2random_loaded rxode2_rxode2random_loaded
#define rxode2random rxode2_rxode2random

extern Function loadNamespace;
bool rxode2random_loaded = false;
Environment rxode2random;


extern "C" bool qtest(SEXP in, const char *test) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qtest"]);
  CharacterVector c(1);
  c[0]= test;
  return as<bool>(fun(in, c));
  END_RCPP
}

extern "C" SEXP qstrictS(SEXP nn, const char *what) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qstrictS"]);
  CharacterVector c(1);
  c[0]= what;
  return (fun(nn, c));
  END_RCPP
}

extern "C" SEXP qstrictSn(SEXP x_, const char *what) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qstrictSn"]);
  CharacterVector c(1);
  c[0]= what;
  return (fun(x_, c));
  END_RCPP
}

extern "C" SEXP qstrictSdn(SEXP x_, const char *what) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qstrictSdn"]);
  CharacterVector c(1);
  c[0]= what;
  return (fun(x_, c));
  END_RCPP
}

extern "C" SEXP qassertS(SEXP in, const char *test, const char *what) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qassertS"]);
  CharacterVector c(1);
  c[0]= test;
  CharacterVector c2(1);
  c2[0]= what;
  return (fun(in, c, c2));
  END_RCPP
}

extern "C" SEXP _rxode2_expandPars_(SEXP objectSSEXP, SEXP paramsSSEXP, SEXP eventsSSEXP, SEXP controlSSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".expandPars"]);
  return fun(objectSSEXP, paramsSSEXP, eventsSSEXP, controlSSEXP);
  END_RCPP
}

extern "C" SEXP _vecDF(SEXP cv, SEXP n_) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".vecDF"]);
  return fun(cv, n_);
  END_RCPP
}

extern "C" SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".cbindOme"]);
  return fun(et_, mat_, n_);
  END_RCPP
}

extern "C" SEXP _phi(SEXP q) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["phi"]);
  return fun(q);
  END_RCPP
}

extern "C" SEXP _rxSetSeed(SEXP x) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxSetSeed"]);
  return fun(x);
  END_RCPP
}

extern "C" SEXP _rxode2_cvPost_(SEXP nuSSEXP, SEXP omegaSSEXP, SEXP nSSEXP, SEXP omegaIsCholSSEXP, SEXP returnCholSSEXP, SEXP typeSSEXP, SEXP diagXformTypeSSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["cvPost"]);
  return fun(nuSSEXP, omegaSSEXP, nSSEXP, omegaIsCholSSEXP, returnCholSSEXP, typeSSEXP, diagXformTypeSSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_nestingInfo_(SEXP omegaSEXP, SEXP dataSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["nestingInfo"]);
  return fun(omegaSEXP, dataSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rinvchisq(SEXP nSEXP, SEXP nuSEXP, SEXP scaleSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rinvchisq"]);
  return fun(nSEXP, nuSEXP, scaleSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rLKJ1(SEXP dSEXP, SEXP etaSEXP, SEXP choleskySEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rLKJ1"]);
  return fun(dSEXP, etaSEXP, choleskySEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rLKJcv1(SEXP sdSEXP, SEXP etaSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rLKJcv1"]);
  return fun(sdSEXP, etaSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rLKJcvLsd1(SEXP logSdSEXP, SEXP logSdSDSEXP, SEXP etaSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rLKJcvLsd1"]);
  return fun(logSdSEXP, logSdSDSEXP, etaSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rcvC1(SEXP sdEstSEXP, SEXP nuSEXP, SEXP diagXformTypeSEXP, SEXP rTypeSEXP, SEXP returnCholSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rcvC1"]);
  return fun(sdEstSEXP, nuSEXP, diagXformTypeSEXP, rTypeSEXP, returnCholSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxRmvn_(SEXP A_SEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP ncoresSEXP, SEXP isCholSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxRmvn_"]);
  return fun(A_SEXP, muSEXP, sigmaSEXP, ncoresSEXP, isCholSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxMvnrnd(SEXP nSEXP, SEXP LSEXP, SEXP lSEXP, SEXP uSEXP, SEXP muSEXP, SEXP aSEXP, SEXP tolSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxMvnrnd"]);
  return fun(nSEXP, LSEXP, lSEXP, uSEXP, muSEXP, aSEXP, tolSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxCholperm(SEXP SigSEXP, SEXP lSEXP, SEXP uSEXP, SEXP epsSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxCholperm"]);
  return fun(SigSEXP, lSEXP, uSEXP, epsSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxGradpsi(SEXP ySEXP, SEXP LSEXP, SEXP lSEXP, SEXP uSEXP) {
  //rxGradpsi
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxGradpsi"]);
  return fun(ySEXP, LSEXP, lSEXP, uSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxNleq(SEXP lSEXP, SEXP uSEXP, SEXP LSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxNleq"]);
  return fun(lSEXP, uSEXP, LSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxMvrandn_(SEXP A_SEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP ncoresSEXP, SEXP aSEXP, SEXP tolSEXP, SEXP nlTolSEXP, SEXP nlMaxiterSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxMvrandn_"]);
  return fun(A_SEXP, muSEXP, sigmaSEXP, lowerSEXP, upperSEXP, ncoresSEXP, aSEXP, tolSEXP, nlTolSEXP, nlMaxiterSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxSeedEng(SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxSeedEng"]);
  return fun(ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxnbinom_(SEXP sizeSEXP, SEXP muSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxnbinom_"]);
  return fun(sizeSEXP, muSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxgamma_(SEXP sizeSEXP, SEXP muSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxgamma_"]);
  return fun(sizeSEXP, muSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxpois_(SEXP lambdaSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxpois_"]);
  return fun(lambdaSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxnbinomMu_(SEXP sizeSEXP, SEXP muSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxnbinomMu_"]);
  return fun(sizeSEXP, muSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxbinom_(SEXP n0SEXP, SEXP probSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxbinom_"]);
  return fun(n0SEXP, probSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxcauchy_(SEXP locationSEXP, SEXP scaleSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxcauchy_"]);
  return fun(locationSEXP, scaleSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxchisq_(SEXP dfSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxchisq_"]);
  return fun(dfSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxexp_(SEXP rateSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxexp_"]);
  return fun(rateSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxf_(SEXP df1SEXP, SEXP df2SEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxf_"]);
  return fun(df1SEXP, df2SEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxbeta_(SEXP shape1SEXP, SEXP shape2SEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxbeta_"]);
  return fun(shape1SEXP, shape2SEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxgeom_(SEXP probSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxgeom_"]);
  return fun(probSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxnorm_(SEXP meanSEXP, SEXP sdSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxnorm_"]);
  return fun(meanSEXP, sdSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxt__(SEXP dfSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
    if (!rxode2random_loaded) {
      rxode2random_loaded = true;
      rxode2random = loadNamespace("rxode2random");
    }
  Function fun = as<Function>(rxode2random["rxt__"]);
  return fun(dfSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxunif_(SEXP lowSEXP, SEXP hiSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxunif_"]);
  return fun(lowSEXP, hiSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxweibull_(SEXP shapeSEXP, SEXP scaleSEXP, SEXP nSEXP, SEXP ncoresSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxweibull_"]);
  return fun(shapeSEXP, scaleSEXP, nSEXP, ncoresSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxRmvn0(SEXP A_SEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP ncoresSEXP, SEXP isCholSEXP, SEXP aSEXP, SEXP tolSEXP, SEXP nlTolSEXP, SEXP nlMaxiterSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxRmvn0"]);
  return fun(A_SEXP, muSEXP, sigmaSEXP, lowerSEXP, upperSEXP, ncoresSEXP, isCholSEXP, aSEXP, tolSEXP, nlTolSEXP, nlMaxiterSEXP);
  END_RCPP
}


extern "C" SEXP _rxode2_rxRmvnSEXP(SEXP nSSEXP, SEXP muSSEXP, SEXP sigmaSSEXP, SEXP lowerSSEXP, SEXP upperSSEXP, SEXP ncoresSSEXP, SEXP isCholSSEXP, SEXP keepNamesSSEXP, SEXP aSSEXP, SEXP tolSSEXP, SEXP nlTolSSEXP, SEXP nlMaxiterSSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxRmvnSEXP"]);
  return fun(nSSEXP, muSSEXP, sigmaSSEXP, lowerSSEXP, upperSSEXP, ncoresSSEXP, isCholSSEXP, keepNamesSSEXP, aSSEXP, tolSSEXP, nlTolSSEXP, nlMaxiterSSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rpp_(SEXP nSSEXP, SEXP lambdaSSEXP, SEXP gammaSSEXP, SEXP probSSEXP, SEXP t0SSEXP, SEXP tmaxSSEXP, SEXP randomOrderSSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rpp_"]);
  return fun(nSSEXP, lambdaSSEXP, gammaSSEXP, probSSEXP, t0SSEXP, tmaxSSEXP, randomOrderSSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxordSelect(SEXP uSEXP, SEXP csSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxordSelect"]);
  return fun(uSEXP, csSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_rxGetSeed(void) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["rxGetSeed"]);
  return fun();
  END_RCPP
}

extern "C" SEXP _rxode2_expandTheta_(SEXP thetaSSEXP, SEXP thetaMatSSEXP, SEXP thetaLowerSSEXP, SEXP thetaUpperSSEXP, SEXP nStudSSEXP, SEXP nCoresRVSSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["expandTheta_"]);
  return fun(thetaSSEXP, thetaMatSSEXP, thetaLowerSSEXP, thetaUpperSSEXP, nStudSSEXP, nCoresRVSSEXP);
  END_RCPP
}

extern "C" SEXP _rxode2_invWR1d(SEXP dSEXP, SEXP nuSEXP, SEXP omegaIsCholSEXP) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random["invWR1d"]);
  return fun(dSEXP, nuSEXP, omegaIsCholSEXP);
  END_RCPP
}

getRxSeed1_t getRxSeed1;
setSeedEng1_t setSeedEng1;
setRxSeedFinal_t setRxSeedFinal;
seedEng_t seedEng;
rxunif_t rxodeUnif;


extern "C" SEXP _rxode2_assignSeedInfo(void) {
  BEGIN_RCPP
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".funPtrs"]);
  List ptr = as<List>(fun());
  getRxSeed1 = (getRxSeed1_t)(R_ExternalPtrAddr(ptr[0]));
  setSeedEng1 = (setSeedEng1_t)(R_ExternalPtrAddr(ptr[1]));
  setRxSeedFinal = (setRxSeedFinal_t)(R_ExternalPtrAddr(ptr[2]));
  seedEng = (seedEng_t) (R_ExternalPtrAddr(ptr[3]));
  rxodeUnif = (rxunif_t) (R_ExternalPtrAddr(ptr[4]));
  return R_NilValue;
  END_RCPP
}
