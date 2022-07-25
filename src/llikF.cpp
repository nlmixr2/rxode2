#include "llik2.h"
////////////////////////////////////////////////////////////////////////////////
// F (not available in stan..., why not, probably not used often)
struct f_llik {
  const Eigen::VectorXd y_;
  f_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T nu1 = theta[0];
    T nu2 = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = lgamma(0.5*nu1+0.5*nu2) - lgamma(0.5*nu1) - lgamma(0.5*nu2) +
        (0.5*nu1)*log(nu1/nu2)  + (0.5*nu1 - 1.0)*log(y_[n])  -
        (0.5*(nu1+nu2))*log(1.0+nu1*y_[n]/nu2);
    }
    return lp;
  }
};

stanLl llik_f(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  f_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikFFull(double* ret, double x, double df1, double df2) {
  if (ret[0] == isF &&
      ret[1] == x   &&
      ret[2] == df1 &&
      ret[3] == df2) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x)) {
    ret[0] = isF;
    ret[1] = x;
    ret[2] = df1;
    ret[3] = df2;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = df1;
  params(1) = df2;
  stanLl ll = llik_f(y, params);
  ret[0] = isF;
  ret[1] = x;
  ret[2] = df1;
  ret[3] = df2;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikFInternal(Rcpp::NumericVector x, Rcpp::NumericVector df1,
                              Rcpp::NumericVector df2) {
  NumericVector fx(x.size());
  NumericVector dDf1(x.size());
  NumericVector dDf2(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikFFull(cur, x[j], df1[j], df2[j]);
    fx[j]    = cur[4];
    dDf1[j]  = cur[5];
    dDf2[j]  = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dDf1"]=dDf1,
                                 _["dDf2"]=dDf2);
}

extern "C" double rxLlikF(double* ret, double x, double df1, double df2) {
  llikFFull(ret, x, df1, df2);
  return ret[4];
}

extern "C" double rxLlikFDdf1(double* ret, double x, double df1, double df2) {
  llikFFull(ret, x, df1, df2);
  return ret[5];
}

extern "C" double rxLlikFDdf2(double* ret, double x, double df1, double df2) {
  llikFFull(ret, x, df1, df2);
  return ret[6];
}
