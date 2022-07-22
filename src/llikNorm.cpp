#include "llik2.h"

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

stanLl llik_normal(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  normal_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J = J;
  return ret;
}

static inline void llikNormFull(double* ret, double x, double mu, double sigma) {
  if (ret[0] == isNorm &&
      ret[1] == x &&
      ret[2] == mu &&
      ret[3] == sigma) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x)) {
    ret[0] = isNorm;
    ret[1] = x;
    ret[2] = mu;
    ret[3] = sigma;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = mu;
  params(1) = sigma;
  stanLl ll = llik_normal(y, params);
  ret[0] = isNorm;
  ret[1] = x;
  ret[2] = mu;
  ret[3] = sigma;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikNormInternal(Rcpp::NumericVector x, Rcpp::NumericVector mu, Rcpp::NumericVector sigma) {
  NumericVector fx(x.size());
  NumericVector dMu(x.size());
  NumericVector dSigma(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikNormFull(cur, x[j], mu[j], sigma[j]);
    fx[j] = cur[4];
    dMu[j] = cur[5];
    dSigma[j] = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dMean"]=dMu,
                                 _["dSd"]=dSigma);
}

extern "C" double rxLlikNorm(double* ret, double x, double mu, double sigma) {
  llikNormFull(ret, x, mu, sigma);
  return ret[4];
}

extern "C" double rxLlikNormDmean(double* ret, double x, double mu, double sigma) {
  llikNormFull(ret, x, mu, sigma);
  return ret[5];
}

extern "C" double rxLlikNormDsd(double* ret, double x, double mu, double sigma) {
  llikNormFull(ret, x, mu, sigma);
  return ret[6];
}

#undef isNorm
#undef isPois
#undef isBinom
#undef isBeta
#undef isT
#undef isChisq
#undef isExp
#undef isF
#undef isGeom
#undef isUnif
#undef isWeibull
#undef isGamma
