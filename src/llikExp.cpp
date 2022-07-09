#include "llik2.h"
////////////////////////////////////////////////////////////////////////////////
// exponential
struct exp_llik {
  const Eigen::VectorXd y_;
  exp_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T beta    = theta[0];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = exponential_log(y_[n], beta);
    return lp;
  }
};

stanLl llik_exp(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  exp_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikExpFull(double* ret, double x, double rate) {
  if (ret[0] == isExp &&
      ret[1] == x &&
      ret[2] == rate) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(1);
  y(0) = x;
  params(0) = rate;
  stanLl ll = llik_exp(y, params);
  ret[0] = isExp;
  ret[1] = x;
  ret[2] = rate;
  ret[3] = ll.fx(0);
  ret[4] = ll.J(0, 0);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikExpInternal(Rcpp::NumericVector x, Rcpp::NumericVector rate) {
  NumericVector fx(x.size());
  NumericVector dRate(x.size());
  double cur[5];
  for (int j = x.size(); j--;) {
    llikExpFull(cur, x[j], rate[j]);
    fx[j]    = cur[3];
    dRate[j]   = cur[4];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dRate"]=dRate);
}

extern "C" double rxLlikExp(double* ret, double x, double rate) {
  llikExpFull(ret, x, rate);
  return ret[3];
}

extern "C" double rxLlikExpDrate(double* ret, double x, double rate) {
  llikExpFull(ret, x, rate);
  return ret[4];
}
