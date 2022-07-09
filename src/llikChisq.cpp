#include "llik2.h"
////////////////////////////////////////////////////////////////////////////////
// chisq

struct chisq_llik {
  const Eigen::VectorXd y_;
  chisq_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T nu    = theta[0];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = chi_square_log(y_[n], nu);
    return lp;
  }
};

stanLl llik_chisq(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  chisq_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikChisqFull(double* ret, double x, double df) {
  if (ret[0] == isChisq &&
      ret[1] == x &&
      ret[2] == df) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(1);
  y(0) = x;
  params(0) = df;
  stanLl ll = llik_chisq(y, params);
  ret[0] = isChisq;
  ret[1] = x;
  ret[2] = df;
  ret[3] = ll.fx(0);
  ret[4] = ll.J(0, 0);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikChisqInternal(Rcpp::NumericVector x, Rcpp::NumericVector df) {
  NumericVector fx(x.size());
  NumericVector dDf(x.size());
  double cur[5];
  for (int j = x.size(); j--;) {
    llikChisqFull(cur, x[j], df[j]);
    fx[j]    = cur[3];
    dDf[j]   = cur[4];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dDf"]=dDf);
}

extern "C" double rxLlikChisq(double* ret, double x, double df) {
  llikChisqFull(ret, x, df);
  return ret[3];
}

extern "C" double rxLlikChisqDdf(double* ret, double x, double df) {
  llikChisqFull(ret, x, df);
  return ret[4];
}
