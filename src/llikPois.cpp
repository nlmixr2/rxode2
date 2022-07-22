#include "llik2.h"
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
  if (!R_finite(x)) {
    ret[0] = isPois;
    ret[1] = x;
    ret[2] = lambda;
    ret[3] = NA_REAL;
    ret[4] = NA_REAL;
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
