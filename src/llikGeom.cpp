#include "llik2.h"
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
