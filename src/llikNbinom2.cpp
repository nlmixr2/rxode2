#include "llik2.h"

////////////////////////////////////////////////////////////////////////////////
// Negative Binomial 2
// R , sigma=scale

struct nbinom2_llik {
  const Eigen::VectorXi y_;
  const Eigen::VectorXi N_;
  nbinom2_llik(const Eigen::VectorXi& y, Eigen::VectorXi& N) : y_(y), N_(N) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T mu = theta[0];
		
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = stan::math::neg_binomial_2_lpmf(y_[n], N_[n], mu);
    return lp;
  }
};

struct nbinom_llik {
  const Eigen::VectorXi y_;
  const Eigen::VectorXi N_;
  
  nbinom_llik(const Eigen::VectorXi& y, Eigen::VectorXi& N) : y_(y), N_(N) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T p = theta[0]; // prob = size/(size+mu); size/prob-size = mu
		
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = stan::math::neg_binomial_2_lpmf(y_[n], N_[n], N_[n]/p-N_[n]);
    return lp;
  }
};

stanLl llik_nbinom2(Eigen::VectorXi& y, Eigen::VectorXi& N, Eigen::VectorXd& params) {
  nbinom2_llik f(y, N);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

stanLl llik_nbinom(Eigen::VectorXi& y, Eigen::VectorXi& N, Eigen::VectorXd& params) {
  nbinom_llik f(y, N);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}



static inline void llikNbinom2Full(double* ret, double x, double size, double mu) {
  if (ret[0] == isNbinom2 &&
      ret[1] == x &&
      ret[2] == size &&
      ret[3] == mu) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x) || !R_finite(size) || !R_finite(mu)) {
    ret[0] = isNbinom2;
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
  stanLl ll = llik_nbinom2(y, N, params);
  ret[0] = isNbinom2;
  ret[1] = x;
  ret[2] = size;
  ret[3] = mu;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  return;
}

static inline void llikNbinomFull(double* ret, double x, double size, double prob) {
  if (ret[0] == isNbinom &&
      ret[1] == x &&
      ret[2] == size &&
      ret[3] == prob) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x) || !R_finite(size) || !R_finite(prob)) {
    ret[0] = isNbinom;
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
  stanLl ll = llik_nbinom(y, N, params);
  ret[0] = isNbinom2;
  ret[1] = x;
  ret[2] = size;
  ret[3] = prob;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  return;
}


//[[Rcpp::export]]
Rcpp::DataFrame llikNbinom2Internal(Rcpp::NumericVector x, Rcpp::NumericVector size, Rcpp::NumericVector mu) {
  NumericVector fx(x.size());
  NumericVector dMu(x.size());
  double cur[6];
  for (int j = x.size(); j--;) {
    llikNbinom2Full(cur, x[j], size[j], mu[j]);
    fx[j]      = cur[4];
    dMu[j]     = cur[5];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dMu"]=dMu);
}

//[[Rcpp::export]]
Rcpp::DataFrame llikNbinomInternal(Rcpp::NumericVector x, Rcpp::NumericVector size, Rcpp::NumericVector prob) {
  NumericVector fx(x.size());
  NumericVector dProb(x.size());
  double cur[6];
  for (int j = x.size(); j--;) {
    llikNbinomFull(cur, x[j], size[j], prob[j]);
    fx[j]      = cur[4];
    dProb[j]     = cur[5];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dProb"]=dProb);
}


extern "C" double rxLlikNbinom2(double* ret, double x, double size, double mu) {
  llikNbinom2Full(ret, x, size, mu);
  return ret[4];
}

extern "C" double rxLlikNbinom2Dmu(double* ret, double x, double size, double mu) {
  llikNbinom2Full(ret, x, size, mu);
  return ret[5];
}

extern "C" double rxLlikNbinom(double* ret, double x, double size, double prob) {
  llikNbinomFull(ret, x, size, prob);
  return ret[4];
}

extern "C" double rxLlikNbinomDprob(double* ret, double x, double size, double prob) {
  llikNbinomFull(ret, x, size, prob);
  return ret[5];
}


