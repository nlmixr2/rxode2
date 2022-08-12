#include "llik2.h"

////////////////////////////////////////////////////////////////////////////////
// Negative Binomial 2
// R , sigma=scale

struct nbinomMu_llik {
  const Eigen::VectorXi y_;
  const Eigen::VectorXi N_;
  nbinomMu_llik(const Eigen::VectorXi& y, Eigen::VectorXi& N) : y_(y), N_(N) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T mu = theta[0];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int i = 0; i < y_.size(); ++i) {
      lp[i] = stan::math::neg_binomial_2_lpmf(y_[i], mu, N_[i]);
    }
    return lp;
  }
};

stanLl llik_nbinomMu(Eigen::VectorXi& y, Eigen::VectorXi& N, Eigen::VectorXd& params) {
  nbinomMu_llik f(y, N);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}


static inline void llikNbinomMuFull(double* ret, double x, double size, double mu) {
  if (ret[0] == isNbinomMu &&
      ret[1] == x &&
      ret[2] == size &&
      ret[3] == mu) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x) || !R_finite(size) || !R_finite(mu)) {
    ret[0] = isNbinomMu;
    ret[1] = x;
    ret[2] = size;
    ret[3] = mu;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    return;
  }
  Eigen::VectorXi y(1);
  Eigen::VectorXi N(1);
  Eigen::VectorXd params(1);
  y(0) = (int)(x);
  N(0) = (int)(size);
  params(0) = mu;
  stanLl ll = llik_nbinomMu(y, N, params);
  ret[0] = isNbinomMu;
  ret[1] = x;
  ret[2] = size;
  ret[3] = mu;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikNbinomMuInternal(Rcpp::NumericVector x, Rcpp::NumericVector size, Rcpp::NumericVector mu) {
  NumericVector fx(x.size());
  NumericVector dMu(x.size());
  double cur[6];
  for (int j = x.size(); j--;) {
    llikNbinomMuFull(cur, x[j], size[j], mu[j]);
    fx[j]      = cur[4];
    dMu[j]     = cur[5];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dMu"]=dMu);
}

extern "C" double rxLlikNbinomMu(double* ret, double x, double size, double mu) {
  llikNbinomMuFull(ret, x, size, mu);
  return ret[4];
}

extern "C" double rxLlikNbinomMuDmu(double* ret, double x, double size, double mu) {
  llikNbinomMuFull(ret, x, size, mu);
  return ret[5];
}
