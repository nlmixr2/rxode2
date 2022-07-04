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
#define isBeta 3.0
#define isT 4.0
#define isChisq 5.0

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


////////////////////////////////////////////////////////////////////////////////

struct beta_llik {
  const Eigen::VectorXd y_;
  beta_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T shape1 = theta[0];
    T shape2 = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = beta_log(y_[n], shape1, shape2);
    return lp;
  }
};

stanLl llik_beta(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  beta_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikBetaFull(double* ret, double x, double shape1, double shape2) {
  if (ret[0] == isBeta &&
      ret[1] == x &&
      ret[2] == shape1 &&
      ret[3] == shape2) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = shape1;
  params(1) = shape2;
  stanLl ll = llik_beta(y, params);
  ret[0] = isBeta;
  ret[1] = x;
  ret[2] = shape1;
  ret[3] = shape2;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}


//[[Rcpp::export]]
Rcpp::DataFrame llikBetaInternal(Rcpp::NumericVector x, Rcpp::NumericVector shape1, Rcpp::NumericVector shape2) {
  NumericVector fx(x.size());
  NumericVector dShape1(x.size());
  NumericVector dShape2(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikBetaFull(cur, x[j], shape1[j], shape2[j]);
    fx[j]      = cur[4];
    dShape1[j] = cur[5];
    dShape2[j] = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dShape1"]=dShape1,
                                 _["dShape2"]=dShape2);
}

extern "C" double rxLlikBeta(double* ret, double x, double shape1, double shape2) {
  llikBetaFull(ret, x, shape1, shape2);
  return ret[4];
}

extern "C" double rxLlikBetaDshape1(double* ret, double x, double shape1, double shape2) {
  llikBetaFull(ret, x, shape1, shape2);
  return ret[5];
}

extern "C" double rxLlikBetaDshape2(double* ret, double x, double shape1, double shape2) {
  llikBetaFull(ret, x, shape1, shape2);
  return ret[6];
}

////////////////////////////////////////////////////////////////////////////////

struct t_llik {
  const Eigen::VectorXd y_;
  t_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T nu    = theta[0];
    T mu    = theta[1];
    T sigma = theta[2];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = student_t_log(y_[n], nu, mu, sigma);
    return lp;
  }
};

stanLl llik_t(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  t_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikTFull(double* ret, double x, double df, double mean, double sd) {
  if (ret[0] == isT &&
      ret[1] == x &&
      ret[2] == df &&
      ret[3] == mean &&
      ret[4] == sd) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(3);
  y(0) = x;
  params(0) = df;
  params(1) = mean;
  params(2) = sd;
  stanLl ll = llik_t(y, params);
  ret[0] = isT;
  ret[1] = x;
  ret[2] = df;
  ret[3] = mean;
  ret[4] = sd;
  ret[5] = ll.fx(0);
  ret[6] = ll.J(0, 0);
  ret[7] = ll.J(0, 1);
  ret[8] = ll.J(0, 2);
  return;
}


//[[Rcpp::export]]
Rcpp::DataFrame llikTInternal(Rcpp::NumericVector x, Rcpp::NumericVector df,
                              Rcpp::NumericVector mean,  Rcpp::NumericVector sd) {
  NumericVector fx(x.size());
  NumericVector dDf(x.size());
  NumericVector dMean(x.size());
  NumericVector dSd(x.size());
  double cur[9];
  for (int j = x.size(); j--;) {
    llikTFull(cur, x[j], df[j], mean[j], sd[j]);
    fx[j]    = cur[5];
    dDf[j]   = cur[6];
    dMean[j] = cur[7];
    dSd[j]   = cur[8];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dDf"]=dDf,
                                 _["dMean"]=dMean,
                                 _["dSd"]=dSd);
}

extern "C" double rxLlikT(double* ret, double x, double df, double mean, double sd) {
  llikTFull(ret, x, df, mean, sd);
  return ret[5];
}

extern "C" double rxLlikTDdf(double* ret, double x, double df, double mean, double sd) {
  llikTFull(ret, x, df, mean, sd);
  return ret[6];
}

extern "C" double rxLlikTDmean(double* ret, double x, double df, double mean, double sd) {
  llikTFull(ret, x, df, mean, sd);
  return ret[7];
}

extern "C" double rxLlikTDsd(double* ret, double x, double df, double mean, double sd) {
  llikTFull(ret, x, df, mean, sd);
  return ret[8];
}

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

#undef isNorm
#undef isPois
#undef isBinom
#undef isBeta
#undef isT
#undef isChisq
