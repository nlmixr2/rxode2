#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
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

double binomProbsLimF(double theta, double w, int n, int Y, double U) {
  return w*Rf_pbeta(1-theta, n-Y+1, Y, 1, 0) + (1-w)*Rf_pbeta(1-theta, n-Y, Y+1, 1, 0) - U;
}

//[[Rcpp::export]]
NumericVector binomProbsPredVec_(int n, int m, int Y, int M, bool doP=true, double tol=1e-7) {
  NumericVector ret(M);
  for (int i = 0; i < M; i++) {
    double w = unif_rand();
    double U = unif_rand();
    double V = unif_rand();
    // root finding using bisection
    double x0 = 0.0, x1 = 1.0;
    double root = -1.0;
    double f0 = binomProbsLimF(x0, w, n, Y, U);
    if (f0 == 0) root = x0;
    double f1 = binomProbsLimF(x1, w, n, Y, U);
    if (f1 == 0) root = x1;
    if (root == -1.0) {
      double xc, fc;
      for (;;) {
        xc = 0.5*(x0+x1);
        if(fabs(x0-x1) < tol) {
          root = xc;
          break;
        }
        fc = binomProbsLimF(xc, w, n, Y, U);
        if(fabs(fc) < tol) {
          root = xc;
          break;
        }
        if(f0*fc > 0.0) {
          x0 = xc; f0 = fc;
        } else {
          x1 = xc; f1 = fc;
        }
      }
    }
    if (doP) {
      ret[i] = Rf_qbinom(V, m, root, 1, 0)/m;
    } else {
      ret[i] = Rf_qbinom(V, m, root, 1, 0);
    }
  }
  return ret;
}

//[[Rcpp::export]]
NumericVector binomProbs_(NumericVector x, NumericVector probs, bool naRm, int nIn,
                          int cont) {
  double oldM = 0.0, newM = 0.0;
  int n = 0;
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
      double z2 = z*z;
      double c1, c2, c3, ns, nh, ph;
#define contWilsonContinuityCorrection 0
#define contWilson 1
#define contWald 2
#define contAgrestiCoull 3
      switch (cont) {
      case contWilsonContinuityCorrection:
        c1 = 2*nC*oldM + z2;
        c2 = z*sqrt(z2 - 1.0/nC + 4*nC*var + (4*oldM-2))+1.0;
        c3 = 2*(nC+z2);
        if (p < 0.5) {
          if (p == 0.0) {
            ret[i+4] = 0.0;
          } else {
            ret[i+4] = max2(0, (c1-c2)/c3);
          }
        } else {
          if (p == 1.0) {
            ret[i+4] = 1.0;
          } else {
            ret[i+4] = min2(1, (c1+c2)/c3);
          }
        }
        break;
      case contWilson:
        c1 = 1.0/(1.0+z2/nC)*(oldM+z2/(2.0*nC));
        c2 = z/(1.0+z2/nC)*sqrt(oldM*(1.0-oldM)/nC + z2/(4.0*nC*nC));
        if (p < 0.5) {
          ret[i+4] = c1-c2;
        } else {
          ret[i+4] = c1+c2;
        }
        break;
      case contWald:
        c1 = oldM;
        c2 = z*sqrt(oldM*(1-oldM)/nC);
        if (p < 0.5) {
          ret[i+4] = c1-c2;
        } else {
          ret[i+4] = c1+c2;
        }
        break;
      case contAgrestiCoull:
        // p = ns/n; p*n = ns
        ns = oldM*nC;
        nh = nC+z2;
        ph = 1/nh*(ns+z2/2.0);
        c1 = ph; // ns
        c2 = z*sqrt(ph/nh*(1-ph));
        if (p < 0.5) {
          ret[i+4] = c1-c2;
        } else {
          ret[i+4] = c1+c2;
        }
        break;
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
