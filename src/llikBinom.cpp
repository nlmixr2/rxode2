#include "llik2.h"
////////////////////////////////////////////////////////////////////////////////
struct binom_llik {
  const Eigen::VectorXi y_;
  const Eigen::VectorXi N_;
  binom_llik(const Eigen::VectorXi& y, Eigen::VectorXi& N) : y_(y), N_(N) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T prob = theta[0];
		
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = binomial_log(y_[n], N_[n], prob);
    return lp;
  }
};


stanLl llik_binom(Eigen::VectorXi& y, Eigen::VectorXi& N, Eigen::VectorXd& params) {
  binom_llik f(y, N);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikBinomFull(double* ret, double x, double size, double prob) {
  if (ret[0] == isBinom &&
      ret[1] == x &&
      ret[2] == size &&
      ret[3] == prob) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x)) {
    ret[0] = isBinom;
    ret[1] = x;
    ret[2] = size;
    ret[3] = prob;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    return;
  }
  Eigen::VectorXi y(1);
  Eigen::VectorXi N(1);
  Eigen::VectorXd params(1);
  y(0) = (int)(x);
  N(0) = (int)(size);
  params(0) = prob;
  stanLl ll = llik_binom(y, N, params);
  ret[0] = isBinom;
  ret[1] = x;
  ret[2] = size;
  ret[3] = _parIsProb(prob);
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikBinomInternal(Rcpp::NumericVector x, Rcpp::NumericVector size, Rcpp::NumericVector prob) {
  NumericVector fx(x.size());
  NumericVector dProb(x.size());
  double cur[6];
  for (int j = x.size(); j--;) {
    llikBinomFull(cur, x[j], size[j], prob[j]);
    fx[j]      = cur[4];
    dProb[j] = cur[5];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dProb"]=dProb);
}

extern "C" double rxLlikBinom(double* ret, double x, double size, double prob) {
  llikBinomFull(ret, x, size, prob);
  return ret[4];
}

extern "C" double rxLlikBinomDprob(double* ret, double x, double size, double prob) {
  llikBinomFull(ret, x, size, prob);
  return ret[5];
}

