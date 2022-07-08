//#undef NDEBUG
#define NDEBUG // just in case
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#define NDEBUG // just in case
#include <Rcpp.h>
#include <RcppEigen.h>
#include "../inst/include/rxode2.h"
#include "llik.h"
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;

#define isNorm 8.0
#define isPois 1.0
#define isBinom 2.0
#define isBeta 3.0
#define isT 4.0
#define isChisq 5.0
#define isExp 6.0
#define isF 7.0
#define isGeom 9.0
#define isUnif 10.0
#define isWeibull 11.0
#define isGamma 12.0

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

////////////////////////////////////////////////////////////////////////////////
// exponential
struct exp_llik {
  const Eigen::VectorXd y_;
  exp_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T beta    = theta[0];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = exponential_log(y_[n], beta);
    return lp;
  }
};

stanLl llik_exp(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  exp_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikExpFull(double* ret, double x, double rate) {
  if (ret[0] == isExp &&
      ret[1] == x &&
      ret[2] == rate) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(1);
  y(0) = x;
  params(0) = rate;
  stanLl ll = llik_exp(y, params);
  ret[0] = isExp;
  ret[1] = x;
  ret[2] = rate;
  ret[3] = ll.fx(0);
  ret[4] = ll.J(0, 0);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikExpInternal(Rcpp::NumericVector x, Rcpp::NumericVector rate) {
  NumericVector fx(x.size());
  NumericVector dRate(x.size());
  double cur[5];
  for (int j = x.size(); j--;) {
    llikExpFull(cur, x[j], rate[j]);
    fx[j]    = cur[3];
    dRate[j]   = cur[4];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dRate"]=dRate);
}

extern "C" double rxLlikExp(double* ret, double x, double rate) {
  llikExpFull(ret, x, rate);
  return ret[3];
}

extern "C" double rxLlikExpDrate(double* ret, double x, double rate) {
  llikExpFull(ret, x, rate);
  return ret[4];
}

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

////////////////////////////////////////////////////////////////////////////////
// geom ; also not in stan
struct geom_llik {
  const Eigen::VectorXi y_;
  geom_llik(const Eigen::VectorXi& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T p = theta[0];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = log(p)+y_[n]*log(1-p);
    }
    return lp;
  }
};

stanLl llik_geom(Eigen::VectorXi& y, Eigen::VectorXd& params) {
  geom_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikGeomFull(double* ret, double x, double p) {
  if (ret[0] == isGeom &&
      ret[1] == x   &&
      ret[2] == p) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXi y(1);
  Eigen::VectorXd params(1);
  y(0) = (int)(x);
  params(0) = p;
  stanLl ll = llik_geom(y, params);
  ret[0] = isGeom;
  ret[1] = x;
  ret[2] = p;
  ret[3] = ll.fx(0);
  ret[4] = ll.J(0, 0);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikGeomInternal(Rcpp::NumericVector x, Rcpp::NumericVector p) {
  NumericVector fx(x.size());
  NumericVector dP(x.size());
  double cur[5];
  for (int j = x.size(); j--;) {
    llikGeomFull(cur, x[j], p[j]);
    fx[j]    = cur[3];
    dP[j]    = cur[4];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dProb"]=dP);
}

extern "C" double rxLlikGeom(double* ret, double x, double p) {
  llikGeomFull(ret, x, p);
  return ret[3];
}

extern "C" double rxLlikGeomDp(double* ret, double x, double p) {
  llikGeomFull(ret, x, p);
  return ret[4];
}

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

////////////////////////////////////////////////////////////////////////////////
// Weibull distribution
// R alpha=shape, sigma=scale

struct weibull_llik {
  const Eigen::VectorXd y_;
  weibull_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T alpha = theta[0];
    T sigma  = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = weibull_log(y_[n], alpha, sigma);
    }
    return lp;
  }
};

stanLl llik_weibull(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  weibull_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikWeibullFull(double* ret, double x, double shape, double scale) {
  if (ret[0] == isWeibull &&
      ret[1] == x   &&
      ret[2] == shape &&
      ret[3] == scale) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = shape;
  params(1) = scale;
  stanLl ll = llik_weibull(y, params);
  ret[0] = isWeibull;
  ret[1] = x;
  ret[2] = shape;
  ret[3] = scale;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikWeibullInternal(Rcpp::NumericVector x,
                                 Rcpp::NumericVector shape, Rcpp::NumericVector scale) {
  NumericVector fx(x.size());
  NumericVector dShape(x.size());
  NumericVector dScale(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikWeibullFull(cur, x[j], shape[j], scale[j]);
    fx[j]     = cur[4];
    dShape[j] = cur[5];
    dScale[j]  = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dShape"]=dShape,
                                 _["dScale"]=dScale);
}

extern "C" double rxLlikWeibull(double* ret, double x, double shape, double scale) {
  llikWeibullFull(ret, x, shape, scale);
  return ret[4];
}

extern "C" double rxLlikWeibullDshape(double* ret, double x, double shape, double scale) {
  llikWeibullFull(ret, x, shape, scale);
  return ret[5];
}

extern "C" double rxLlikWeibullDscale(double* ret, double x, double shape, double scale) {
  llikWeibullFull(ret, x, shape, scale);
  return ret[6];
}


////////////////////////////////////////////////////////////////////////////////
// Gamma distribution
// R shape=alpha, scale=1/beta rate=beta
struct gamma_llik {
  const Eigen::VectorXd y_;
  gamma_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T shape = theta[0];
    T rate  = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    // manually code log(f) density
    for (int n = 0; n < y_.size(); ++n) {
      lp[n] = gamma_log(y_[n], shape, rate);
    }
    return lp;
  }
};

stanLl llik_gamma(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  gamma_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikGammaFull(double* ret, double x, double shape, double rate) {
  if (ret[0] == isGamma &&
      ret[1] == x   &&
      ret[2] == shape &&
      ret[3] == rate) {
    // Assume this is the same
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = shape;
  params(1) = rate;
  stanLl ll = llik_gamma(y, params);
  ret[0] = isGamma;
  ret[1] = x;
  ret[2] = shape;
  ret[3] = rate;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}

//[[Rcpp::export]]
Rcpp::DataFrame llikGammaInternal(Rcpp::NumericVector x,
                                 Rcpp::NumericVector shape, Rcpp::NumericVector rate) {
  NumericVector fx(x.size());
  NumericVector dShape(x.size());
  NumericVector dRate(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikGammaFull(cur, x[j], shape[j], rate[j]);
    fx[j]     = cur[4];
    dShape[j] = cur[5];
    dRate[j]  = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dShape"]=dShape,
                                 _["dRate"]=dRate);
}

extern "C" double rxLlikGamma(double* ret, double x, double shape, double rate) {
  llikGammaFull(ret, x, shape, rate);
  return ret[4];
}

extern "C" double rxLlikGammaDshape(double* ret, double x, double shape, double rate) {
  llikGammaFull(ret, x, shape, rate);
  return ret[5];
}

extern "C" double rxLlikGammaDrate(double* ret, double x, double shape, double rate) {
  llikGammaFull(ret, x, shape, rate);
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
