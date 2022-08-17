#include "llik2.h"
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
  if (!R_finite(x)    || !R_finite(df) ||
      !R_finite(mean) || !R_finite(sd)) {
    ret[0] = isT;
    ret[1] = x;
    ret[2] = df;
    ret[3] = mean;
    ret[4] = sd;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    ret[7] = NA_REAL;
    ret[8] = NA_REAL;
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(3);
  y(0) = x;
  params(0) = _smallIsNotZero(df);
  params(1) = mean;
  params(2) = _smallIsOne(sd);
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
