#define STRICT_R_HEADER
#include <stan/math/prim/mat/fun/Eigen.hpp> // must come before #include <RcppEigen.h>
#include <RcppEigen.h>
#include "llik.h"
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;

#include <vector>
#include <stan/math/rev/core.hpp>
#include <stan/math.hpp>

#define isNorm 0.0
#define isPois 1.0
#define isBinom 2.0

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

////////////////////////////////////////////////////////////////////////////////

struct poisson_llik {
  const Eigen::VectorXi y_;
  poisson_llik(const Eigen::VectorXi& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T l = theta[0];
		
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = poisson_log(y_[n], l);
    return lp;
  }
};


stanLl llik_poisson(Eigen::VectorXi& y, Eigen::VectorXd& params) {
  poisson_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikPoisFull(double* ret, double x, double lambda) {
  if (ret[0] == isPois &&
      ret[1] == x &&
      ret[2] == lambda) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXi y(1);
  Eigen::VectorXd params(1);
  y(0) = (int)(x);
  params(0) = lambda;
  stanLl ll = llik_poisson(y, params);
  ret[0] = isPois;
  ret[1] = x;
  ret[2] = lambda;
  ret[3] = ll.fx(0);
  ret[4] = ll.J(0, 0);
  return;
}


//[[Rcpp::export]]
Rcpp::DataFrame llikPoisInternal(Rcpp::NumericVector x, Rcpp::NumericVector lambda) {
  NumericVector fx(x.size());
  NumericVector dLambda(x.size());
  double cur[5];
  for (int j = x.size(); j--;) {
    llikPoisFull(cur, x[j], lambda[j]);
    fx[j]      = cur[3];
    dLambda[j] = cur[4];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dLambda"]=dLambda);
}

extern "C" double rxLlikPois(double* ret, double x, double lambda) {
  llikPoisFull(ret, x, lambda);
  return ret[3];
}

extern "C" double rxLlikPoisDlambda(double* ret, double x, double lambda) {
  llikPoisFull(ret, x, lambda);
  return ret[4];
}

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
  ret[3] = prob;
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




#undef isNorm
#undef isPois
#undef isBinom
