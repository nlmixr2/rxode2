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

extern "C" SEXP _rxode2_setEvCur(SEXP x1) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function setEvCur = as<Function>(rxode2et[".setEvCur"]);
  return setEvCur(x1);
}

List cbindThetaOmega(RObject inputParametersRO, List &individualParameters) {
  if (!rxode2et_loaded) {
    rxode2et_loaded = true;
    rxode2et = loadNamespace("rxode2et");
  }
  Function f = as<Function>(rxode2et[".cbindThetaOmega"]);
  List ret = as<List>(f(wrap(inputParametersRO), wrap(individualParameters)));
  individualParameters=ret[1];
  return ret[0];
}

//[[Rcpp::export]]
NumericVector rxErf(NumericVector v) {
  NumericVector ret(v.size());
  for (int i = v.size(); i--;) {
    ret[i] = std::erf(v[i]);
  }
  return ret;
}

#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

//[[Rcpp::export]]
NumericVector binomProbs_(NumericVector x, NumericVector probs, bool naRm, bool pred,
                         int nIn, int mIn) {
  double oldM = 0.0, newM = 0.0;
  int n = 0;
  // For CI use Wilson score interval  with continuity correction.
  // Described in Newcombe, R. G. (1998). "Two-sided confidence intervals for
  // the single proportion: comparison of seven methods". Statistics
  // in Medicine. 17 (8):
  // 857–872. doi:10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E. PMID
  // 9595616.
  // For PI use the Frequentist PI described in Hezhi Lu, Hua Jin,
  // A new prediction interval for binomial random variable based on inferential models,
  // Journal of Statistical Planning and Inference,
  // Volume 205,
  // 2020,
  // Pages 156-174,
  // ISSN 0378-3758,
  // https://doi.org/10.1016/j.jspi.2019.07.001.
  // in that case we are using the Nelson's prediction interval
  // With the case where m=n=1 because this is the bernoulli case
  for (int i = 0; i <  x.size(); ++i) {
    double cur = x[i];
    if (ISNA(cur)) {
      if (naRm) {
        continue;
      } else  {
        NumericVector ret(4+probs.size());
        for (int j = 0; j < ret.size(); ++j) {
          ret[j] = NA_REAL;
        }
        return ret;
      }
    }
    n++;
    if (n == 1) {
      oldM = newM = cur;
    } else {
      newM = oldM + (cur - oldM)/n;
      oldM = newM;
    }
  }
  int nC;
  if (nIn == 0) {
    nC = n;
  } else {
    nC = nIn;
  }
  int m = 1;
  if (mIn == 0) {
    m = n;
  } else {
    m = mIn;
  }
  double var=0;
  double sd=0;
  if (n > 1) {
    var = oldM*(1.0-oldM);
    sd = sqrt(var);
  } else {
    var = 0.0;
    sd = 0.0;
  }
  // mean, var, sd
  NumericVector ret(4+probs.size());
  ret[0] = oldM;
  ret[1] = var;
  ret[2] = sd;
  ret[3] = (double)n;

  for (int i = 0; i < probs.size(); ++i) {
    double p = probs[i];
    std::string str = std::to_string(p*100) + "%";
    if (p == 0) {
      ret[i+4] = 0;
    } else if (p == 1) {
      ret[i+4] = 1;
    } else if (p == 0.5) {
      ret[i+4] = oldM;
    } else {
      double z;
      if (p > 0.5) {
        z = Rf_qnorm5(p, 0.0, 1.0, 1, 0);
      } else {
        z = Rf_qnorm5(1.0-p, 0.0, 1.0, 1, 0);
      }
      if (pred) {
#define y oldM
        double z2 = z*z;
        double A = m*nC*(2*y*z2*(nC+z2+m)+(2*y+z2)/((m+nC)*(m+nC)));
        double B1 = m*nC*(m+nC)*z2*(m+nC+z2)*(m+nC+z2);
        double B2 = 2*(nC-y)*(nC*nC*(2*y+z2)+4.0*m*nC*y+2.0*m*m*y);
        double B3 = nC*z2*(nC*(2*y+z2)+3.0*m*nC+m*m);
        double B = sqrt(B1*(B2+B3));
        double C1 = (nC+z2)*(m*m+nC*(nC+z2));
        double C2 = m*nC*(2*nC+3*z2);
        double C = 2*nC*(C1+C2);
        if (p < 0.5) {
          ret[i+4] = max2(0.0, (A-B)/C);
        } else {
          ret[i+4] = min2(1.0, (A+B)/C);
        }
#undef y
      } else {
        double z2 = z*z;
        double coef1 = 2*nC*oldM + z2;
        double coef2 = z*sqrt(z2 - 1.0/nC + 4*nC*var + (4*oldM-2))+1.0;
        double coef3 = 2*(nC+z2);
        if (p < 0.5) {
          if (p == 0.0) {
            ret[i+4] = 0.0;
          } else {
            ret[i+4] = max2(0, (coef1-coef2)/coef3);
          }
        } else {
          if (p == 1.0) {
            ret[i+4] = 1.0;
          } else {
            ret[i+4] = min2(1, (coef1+coef2)/coef3);
          }
        }
      }
    }
  }
  return ret;
}

//[[Rcpp::export]]
NumericVector meanProbs_(NumericVector x, NumericVector probs, bool naRm, bool useT,
                         bool pred, int nIn) {
  double oldM = 0.0, newM = 0.0,
    oldS = 0.0, newS = 0.0, mx=R_NegInf, mn=R_PosInf;
  int n = 0;
  for (int i = 0; i <  x.size(); ++i) {
    double cur = x[i];
    if (ISNA(cur)) {
      if (naRm) {
        continue;
      } else  {
        NumericVector ret(6+probs.size());
        for (int j = 0; j < ret.size(); ++j) {
          ret[j] = NA_REAL;
        }
        return ret;
      }
    }
    n++;
    mn = min2(cur, mn);
    mx = max2(cur, mx);
    if (n == 1) {
      oldM = newM = cur;
      oldS = 0.0;
    } else {
      newM = oldM + (cur - oldM)/n;
      newS = oldS + (cur - oldM)*(cur - newM);
      oldM = newM;
      oldS = newS;
    }
  }
  double var;
  if (n > 1) {
    var = newS/(n-1);
  } else {
    var = 0.0;
  }
  double sd = sqrt(var);
  // mean, var, sd, min, max
  NumericVector ret(6+probs.size());
  ret[0] = oldM;
  ret[1] = var;
  ret[2] = sd;
  ret[3] = mn;
  ret[4] = mx;
  ret[5] = (double)n;
  double c;
  int nC;
  if (nIn == 0) {
    nC = n;
  } else {
    nC = nIn;
  }
  if (pred) {
    c = sd*sqrt(1.0+(1.0/nC));
  } else {
    c = sd/sqrt((double)(nC));
  }
  for (int i = 0; i < probs.size(); ++i) {
    double p = probs[i];
    std::string str = std::to_string(p*100) + "%";
    if (p == 0) {
      ret[i+6] = mn;
    } else if (p == 1) {
      ret[i+6] = mx;
    } else if (p == 0.5) {
      ret[i+6] = oldM;
    } else if (useT) {
      ret[i+6] = oldM + c * Rf_qt(p, (double)(nC-1), 1, 0);
    } else {
      ret[i+6] = oldM + c * Rf_qnorm5(p, 0.0, 1.0, 1, 0);
    }
  }
  return ret;
}
