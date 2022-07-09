#include "llik2.h"

////////////////////////////////////////////////////////////////////////////////
// unif
struct unif_llik {
  const Eigen::VectorXd y_;
  unif_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T alpha = theta[0];
    T beta  = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = uniform_log(y_[n], alpha, beta);
    }
    return lp;
  }
};

stanLl llik_unif(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  unif_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikUnifFull(double* ret, double x, double alpha, double beta) {
  if (ret[0] == isUnif &&
      ret[1] == x   &&
      ret[2] == alpha &&
      ret[3] == beta) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = alpha;
  params(1) = beta;
  stanLl ll = llik_unif(y, params);
  ret[0] = isUnif;
  ret[1] = x;
  ret[2] = alpha;
  ret[3] = beta;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikUnifInternal(Rcpp::NumericVector x,
                                 Rcpp::NumericVector alpha, Rcpp::NumericVector beta) {
  NumericVector fx(x.size());
  NumericVector dAlpha(x.size());
  NumericVector dBeta(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikUnifFull(cur, x[j], alpha[j], beta[j]);
    fx[j]     = cur[4];
    dAlpha[j] = cur[5];
    dBeta[j]  = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dAlpha"]=dAlpha,
                                 _["dBeta"]=dBeta);
}

extern "C" double rxLlikUnif(double* ret, double x, double alpha, double beta) {
  llikUnifFull(ret, x, alpha, beta);
  return ret[4];
}

extern "C" double rxLlikUnifDalpha(double* ret, double x, double alpha, double beta) {
  llikUnifFull(ret, x, alpha, beta);
  return ret[5];
}

extern "C" double rxLlikUnifDbeta(double* ret, double x, double alpha, double beta) {
  llikUnifFull(ret, x, alpha, beta);
  return ret[6];
}
