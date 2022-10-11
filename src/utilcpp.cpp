#define USE_FC_LEN_T
#define STRICT_R_HEADERS
// [[Rcpp::interfaces(r,cpp)]]
//#undef NDEBUG
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

extern "C" SEXP _rxode2_isNullZero(SEXP); 

//[[Rcpp::export]]
LogicalVector isNullZero(RObject obj) {
  if (Rf_isNull(obj)) {
    return true;
  }
  int t = TYPEOF(obj);
  if (t == INTSXP || t == REALSXP) {
    // This tests for thetaMat
    if (obj.hasAttribute("dim")) {
      mat cur = as<arma::mat>(obj);
      if (cur.is_zero()) {
	return true;
      }
    }
  } else if (t == VECSXP) {
    List cur = as<List>(obj);
    for (int i = cur.size(); i--;) {
      RObject curs = cur[i];
      t = TYPEOF(wrap(curs));
      if (t == INTSXP || t == REALSXP) {
	if (curs.hasAttribute("dim")) {
	  mat curm = as<arma::mat>(curs);
	  if (curm.is_zero()) {
	    return true;
	  }
	} else {
	  return false;
	}
      } else {
	return false;
      }
    }
  }
  return false;
}

extern Function loadNamespace;
bool rxode2et_loaded = false;
Environment rxode2et;

extern "C" SEXP _rxode2_chin(SEXP x, SEXP table) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function chin2 = as<Function>(rxode2et[".chin"]);
  return chin2(x, table);
}

extern "C" SEXP _rxode2_etTrans(SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5,
                                SEXP x6, SEXP x7, SEXP x8) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function etTrans2 = as<Function>(rxode2et["etTrans"]);
  return etTrans2(x1, x2, x3, x4, x5, x6, x7, x8);
}

extern "C" SEXP _rxode2_et_(SEXP x1, SEXP x2) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function et_ = as<Function>(rxode2et[".et_"]);
  return et_(x1, x2);
}

extern "C" SEXP _rxode2_etUpdate(SEXP x1, SEXP x2, SEXP x3, SEXP x4) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function etUp = as<Function>(rxode2et[".etUpdate"]);
  return etUp(x1, x2, x3, x4);
}

extern "C" SEXP _rxode2_etSeq_(SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5,
                               SEXP x6, SEXP x7, SEXP x8, SEXP x9, SEXP x10,
                               SEXP x11) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function etSeq = as<Function>(rxode2et[".etSeq"]);
  return etSeq(x1, x2, x3, x4, x5,
               x6, x7, x8, x9, x10,
               x11);
}

extern "C" SEXP _rxode2_etRep_(SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5,
                               SEXP x6, SEXP x7) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function etRep = as<Function>(rxode2et[".etRep"]);
  return etRep(x1, x2, x3, x4, x5,
               x6, x7);
}
extern "C" SEXP _rxode2_rxSetIni0(SEXP x1) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function rxSetIni0 = as<Function>(rxode2et["rxSetIni0"]);
  return rxSetIni0(x1);
}

extern "C" SEXP _rxode2_forderForceBase(SEXP x1) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function forderForceBase = as<Function>(rxode2et[".forderForceBase"]);
  return forderForceBase(x1);
}

extern "C" SEXP _rxode2_rxEtTransAsDataFrame_(SEXP x1) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function asDF = as<Function>(rxode2et["rxEtTransAsDataFrame_"]);
  return asDF(x1);
}
extern "C" SEXP _rxode2_setEvCur(SEXP x1) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function setEvCur = as<Function>(rxode2et[".setEvCur"]);
  return setEvCur(x1);
}
extern "C" SEXP _rxode2_useForder(void) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function useForder = as<Function>(rxode2et[".useForder"]);
  return useForder();
}

extern "C" SEXP _rxode2_getForder(void) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function getForder = as<Function>(rxode2et[".getForder"]);
  return getForder();
}

List cbindThetaOmega(RObject inputParametersRO, List individualParameters) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function f = as<Function>(rxode2et[".cbindThetaOmega"]);
  return as<List>(f(wrap(inputParametersRO), wrap(individualParameters)));
}

//[[Rcpp::export]]
NumericVector rxErf(NumericVector v) {
  NumericVector ret(v.size());
  for (int i = v.size(); i--;) {
    ret[i] = std::erf(v[i]);
  }
  return ret;
}
