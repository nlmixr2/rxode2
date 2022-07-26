#include "llik2.h"

////////////////////////////////////////////////////////////////////////////////
// Cauchy distribution
// x, location, scale

struct cauchy_llik {
  const Eigen::VectorXd y_;
  cauchy_llik(const Eigen::VectorXd& y) : y_(y) { }

  template <typename T>
  Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
    T location    = theta[0];
    T scale = theta[1];
    Eigen::Matrix<T, -1, 1> lp(y_.size());
    for (int n = 0; n < y_.size(); ++n)
      lp[n] = student_t_log(y_[n], 1.0, location, scale);
    return lp;
  }
};

stanLl llik_cauchy(Eigen::VectorXd& y, Eigen::VectorXd& params) {
  cauchy_llik f(y);
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
  stan::math::jacobian(f, params, fx, J);
  stanLl ret;
  ret.fx = fx;
  ret.J  = J;
  return ret;
}

static inline void llikCauchyFull(double* ret, double x, double location, double scale) {
  if (ret[0] == isCauchy &&
      ret[1] == x &&
      ret[2] == location &&
      ret[3] == scale) {
    // Assume this is the same
    return;
  }
  if (!R_finite(x) || !R_finite(location) || !R_finite(scale)) {
    ret[0] = isCauchy;
    ret[1] = x;
    ret[2] = location;
    ret[3] = scale;
    ret[4] = NA_REAL;
    ret[5] = NA_REAL;
    ret[6] = NA_REAL;
    return;
  }
  Eigen::VectorXd y(1);
  Eigen::VectorXd params(2);
  y(0) = x;
  params(0) = location;
  params(1) = _smallIsOne(scale);
  stanLl ll = llik_cauchy(y, params);
  ret[0] = isCauchy;
  ret[1] = x;
  ret[2] = location;
  ret[3] = scale;
  ret[4] = ll.fx(0);
  ret[5] = ll.J(0, 0);
  ret[6] = ll.J(0, 1);
  return;
}


//[[Rcpp::export]]
Rcpp::DataFrame llikCauchyInternal(Rcpp::NumericVector x, 
                                   Rcpp::NumericVector location,  Rcpp::NumericVector scale) {
  NumericVector fx(x.size());
  NumericVector dLocation(x.size());
  NumericVector dScale(x.size());
  double cur[7];
  for (int j = x.size(); j--;) {
    llikCauchyFull(cur, x[j], location[j], scale[j]);
    fx[j]    = cur[4];
    dLocation[j]   = cur[5];
    dScale[j] = cur[6];
  }
  return Rcpp::DataFrame::create(_["fx"]=fx,
                                 _["dLocation"]=dLocation,
                                 _["dScale"]=dScale);
}

extern "C" double rxLlikCauchy(double* ret, double x, double location, double scale) {
  llikCauchyFull(ret, x, location, scale);
  return ret[4];
}

extern "C" double rxLlikCauchyDlocation(double* ret, double x, double location, double scale) {
  llikCauchyFull(ret, x, location, scale);
  return ret[5];
}

extern "C" double rxLlikCauchyDscale(double* ret, double x, double location, double scale) {
  llikCauchyFull(ret, x, location, scale);
  return ret[6];
}
