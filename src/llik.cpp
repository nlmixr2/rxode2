#define STRICT_R_HEADER
#include <stan/math/prim/mat/fun/Eigen.hpp> // must come before #include <RcppEigen.h>
#include <RcppEigen.h>
#include "llik.h"
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;

#include <vector>
#include <stan/math/rev/core.hpp>
#include <stan/math.hpp>

struct normal_llik {
  const Eigen::VectorXd y_;
  normal_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T mu = theta[0];
    T sigma = theta[1];
		
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = normal_log(y_[n], mu, sigma);
    return lp;
  }
};

typedef struct stanLl {
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
} stanLl;

stanLl llik_normal(Eigen::VectorXd y, Eigen::VectorXd params) {
  normal_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J = J;
  return ret;
}

extern "C" void llikNormFull(double* ret, double x, double mu, double sigma) {
  if (ret[0] == x &&
      ret[1] == mu &&
      ret[2] == sigma) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = mu;
  params(1) = sigma;
  stanLl ll = llik_normal(y, params);
  ret[0] = x;
  ret[1] = mu;
  ret[2] = sigma;
  ret[3] = ll.fx(0);
  ret[4] = ll.J(0, 0);
  ret[5] = ll.J(0, 1);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikNormInternal(Rcpp::NumericVector x, Rcpp::NumericVector mu, Rcpp::NumericVector sigma) {
  NumericVector fx(x.size());
  NumericVector dMu(x.size());
  NumericVector dSigma(x.size());
  double cur[6];
  for (int j = x.size(); j--;) {
    llikNormFull(cur, x[j], mu[j], sigma[j]);
    fx[j] = cur[3];
    dMu[j] = cur[4];
    dSigma[j] = cur[5];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dMean"]=dMu,
                                 _["dSd"]=dSigma);
}
