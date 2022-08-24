#include "llik2.h"

////////////////////////////////////////////////////////////////////////////////
// Weibull distribution
// R alpha=shape, sigma=scale

struct weibull_llik {
  const Eigen::VectorXd y_;
  weibull_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T alpha = theta[0];
    T sigma  = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = weibull_log(y_[n], alpha, sigma);
    }
    return lp;
  }
};

stanLl llik_weibull(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  weibull_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikWeibullFull(double* ret, double x, double shape, double scale) {
  if (ret[0] == isWeibull &&
      ret[1] == x   &&
      ret[2] == shape &&
      ret[3] == scale) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x) || !R_finite(shape) || !R_finite(scale)) {
    ret[0] = isWeibull;
    ret[1] = x;
    ret[2] = shape;
    ret[3] = scale;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    return;    
  }
  if (!llikNeedDeriv()) {
    ret[0] = isWeibull;
    ret[1] = x;
    ret[2] = shape;
    ret[3] = scale;
    ret[4] = stan::math::weibull_log(x, shape, scale);
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
  } else {
    Eigen::VectorXd y(1);
    Eigen::VectorXd params(2);
    y(0) = x;
    params(0) = _smallIsNotZero(shape);
    params(1) = _smallIsNotZero(scale);
    stanLl ll = llik_weibull(y, params);
    ret[0] = isWeibull;
    ret[1] = x;
    ret[2] = shape;
    ret[3] = scale;
    ret[4] = ll.fx(0);
    ret[5] = ll.J(0, 0);
    ret[6] = ll.J(0, 1);
  }
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikWeibullInternal(Rcpp::NumericVector x,
                                 Rcpp::NumericVector shape, Rcpp::NumericVector scale) {
  llikNeedDeriv_=1;
  NumericVector fx(x.size());
  NumericVector dShape(x.size());
  NumericVector dScale(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikWeibullFull(cur, x[j], shape[j], scale[j]);
    fx[j]     = cur[4];
    dShape[j] = cur[5];
    dScale[j]  = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dShape"]=dShape,
                                 _["dScale"]=dScale);
}

extern "C" double rxLlikWeibull(double* ret, double x, double shape, double scale) {
  llikWeibullFull(ret, x, shape, scale);
  return ret[4];
}

extern "C" double rxLlikWeibullDshape(double* ret, double x, double shape, double scale) {
  llikWeibullFull(ret, x, shape, scale);
  return ret[5];
}

extern "C" double rxLlikWeibullDscale(double* ret, double x, double shape, double scale) {
  llikWeibullFull(ret, x, shape, scale);
  return ret[6];
}

