#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#include <Rcpp.h>
#include "macros2micros.h"

namespace stan {
  namespace math {

  using std::exp;
  using stan::math::exp;
  using std::sqrt;
  using stan::math::sqrt;
  using std::pow;
  using stan::math::pow;
  using std::acos;
  using stan::math::acos;
  using std::cos;
  using stan::math::cos;

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 2>
    macros2micros(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
                        const int trans,
                        const int ncmt) {
      Eigen::Matrix<T, Eigen::Dynamic, 2> g(ncmt, 3);
      return macros2micros(params, ncmt, trans);
    }
  }
}

extern "C" SEXP _rxode2_macros2micros(SEXP p1, SEXP v1,
                                      SEXP p2, SEXP p3,
                                      SEXP p4, SEXP p5,
                                      SEXP trans, SEXP ncmtS) {
BEGIN_RCPP
  int ncmt = INTEGER(ncmtS)[0];
  Eigen::Matrix<double, Eigen::Dynamic, 1> params(2*ncmt, 1);
  params(0, 0) = REAL(p1)[0];
  params(1, 0) = REAL(v1)[0];
  if (ncmt >=2) {
    params(2,0) = REAL(p2)[0];
    params(3,0) = REAL(p3)[0];
    if (ncmt >= 3) {
      params(4,0) = REAL(p4)[0];
      params(5,0) = REAL(p5)[0];
    }
  }
  Eigen::Matrix<double, Eigen::Dynamic, 2> g = stan::math::macros2micros(params, ncmt, INTEGER(trans)[0]);
  SEXP ret = Rcpp::wrap(g);
  return ret;
END_RCPP
}
