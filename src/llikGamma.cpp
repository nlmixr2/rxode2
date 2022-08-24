#include "llik2.h"
////////////////////////////////////////////////////////////////////////////////
// Gamma distribution
// R shape=alpha, scale=1/beta rate=beta
struct gamma_llik {
  const Eigen::VectorXd y_;
  gamma_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T shape = theta[0];
    T rate  = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = gamma_log(y_[n], shape, rate);
    }
    return lp;
  }
};

stanLl llik_gamma(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  gamma_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikGammaFull(double* ret, double x, double shape, double rate) {
  if (ret[0] == isGamma &&
      ret[1] == x   &&
      ret[2] == shape &&
      ret[3] == rate) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x) || !R_finite(shape) || !R_finite(rate)) {
    ret[0] = isGamma;
    ret[1] = x;
    ret[2] = shape;
    ret[3] = rate;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    return;
  }
  if (!llikNeedDeriv()) {
    ret[0] = isGamma;
    ret[1] = x;
    ret[2] = shape;
    ret[3] = rate;
    ret[4] = stan::math::gamma_log(x, _smallIsNotZero(shape), _smallIsNotZero(rate));
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
  } else {
    Eigen::VectorXd y(1);
    Eigen::VectorXd params(2);
    y(0) = x;
    params(0) = _smallIsNotZero(shape);
    params(1) = _smallIsNotZero(rate);
    stanLl ll = llik_gamma(y, params);
    ret[0] = isGamma;
    ret[1] = x;
    ret[2] = shape;
    ret[3] = rate;
    ret[4] = ll.fx(0);
    ret[5] = ll.J(0, 0);
    ret[6] = ll.J(0, 1);
  }
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikGammaInternal(Rcpp::NumericVector x,
                                 Rcpp::NumericVector shape, Rcpp::NumericVector rate) {
  llikNeedDeriv_=1;
  NumericVector fx(x.size());
  NumericVector dShape(x.size());
  NumericVector dRate(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikGammaFull(cur, x[j], shape[j], rate[j]);
    fx[j]     = cur[4];
    dShape[j] = cur[5];
    dRate[j]  = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dShape"]=dShape,
                                 _["dRate"]=dRate);
}

extern "C" double rxLlikGamma(double* ret, double x, double shape, double rate) {
  llikGammaFull(ret, x, shape, rate);
  return ret[4];
}

extern "C" double rxLlikGammaDshape(double* ret, double x, double shape, double rate) {
  llikGammaFull(ret, x, shape, rate);
  return ret[5];
}

extern "C" double rxLlikGammaDrate(double* ret, double x, double shape, double rate) {
  llikGammaFull(ret, x, shape, rate);
  return ret[6];
}
