#include "llik2.h"
////////////////////////////////////////////////////////////////////////////////

struct beta_llik {
  const Eigen::VectorXd y_;
  beta_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T shape1 = theta[0];
    T shape2 = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = beta_log(y_[n], shape1, shape2);
    return lp;
  }
};

stanLl llik_beta(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  beta_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikBetaFull(double* ret, double x, double shape1, double shape2) {
  if (ret[0] == isBeta &&
      ret[1] == x &&
      ret[2] == shape1 &&
      ret[3] == shape2) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x)) {
    ret[0] = isBeta;
    ret[1] = x;
    ret[2] = shape1;
    ret[3] = shape2;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = shape1;
  params(1) = shape2;
  stanLl ll = llik_beta(y, params);
  ret[0] = isBeta;
  ret[1] = x;
  ret[2] = _smallIsNotZero(shape1);
  ret[3] = _smallIsNotZero(shape2);
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}


//[[Rcpp::export]]
Rcpp::DataFrame llikBetaInternal(Rcpp::NumericVector x, Rcpp::NumericVector shape1, Rcpp::NumericVector shape2) {
  NumericVector fx(x.size());
  NumericVector dShape1(x.size());
  NumericVector dShape2(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikBetaFull(cur, x[j], shape1[j], shape2[j]);
    fx[j]      = cur[4];
    dShape1[j] = cur[5];
    dShape2[j] = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dShape1"]=dShape1,
                                 _["dShape2"]=dShape2);
}

extern "C" double rxLlikBeta(double* ret, double x, double shape1, double shape2) {
  llikBetaFull(ret, x, shape1, shape2);
  return ret[4];
}

extern "C" double rxLlikBetaDshape1(double* ret, double x, double shape1, double shape2) {
  llikBetaFull(ret, x, shape1, shape2);
  return ret[5];
}

extern "C" double rxLlikBetaDshape2(double* ret, double x, double shape1, double shape2) {
  llikBetaFull(ret, x, shape1, shape2);
  return ret[6];
}

