#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "linCmt.h"

// Create linear compartment models for testing

using namespace Rcpp;

// [[Rcpp::export]]
RObject linCmtModelDouble(double dt,
                          double p1, double v1, double p2,
                          double p3, double p4, double p5,
                          double ka,
                          NumericVector alastNV, NumericVector rateNV,
                          const int ncmt, const int oral0, const int trans) {

  stan::math::linCmtStan lc(ncmt, oral0, trans);
  Eigen::Matrix<double, -1, 1> theta;
  Eigen::Matrix<double, -1, 1> alast0 = as<Eigen::Matrix<double, -1, 1> >(alastNV);
  Eigen::Matrix<double, -1, 1> rate = as<Eigen::Matrix<double, -1, 1> >(rateNV);

  switch (ncmt) {
  case 1:
    if (oral0 == 1) {
      theta.resize(3);
      theta << p1, v1, ka;
      if (alast0.size() != 2) {
        stop("Alast0 size needs to be 2");
      }
    } else {
      theta.resize(2);
      theta << p1, v1;
      if (alast0.size() != 1) {
        stop("Alast0 size needs to be 1");
      }
    }
    break;
  case 2:
    if (oral0 == 1) {
      theta.resize(5);
      theta << p1, v1, p2, p3, ka;
      if (alast0.size() != 3) {
        stop("Alast0 size needs to be 3");
      }
    } else {
      theta.resize(4);
      theta << p1, v1, p2, p3;
      if (alast0.size() != 2) {
        stop("Alast0 size needs to be 2");
      }
    }
    break;
  case 3:
    if (oral0 == 1) {
      theta.resize(7);
      theta << p1, v1, p2, p3, p4, p5, ka;
      if (alast0.size() != 4) {
        stop("Alast0 size needs to be 4");
      }
    } else {
      theta.resize(6);
      theta << p1, v1, p2, p3, p4, p5;
      if (alast0.size() != 3) {
        stop("Alast0 size needs to be 3");
      }
    }
    break;
  default:
    stop("Invalid number of compartments");
  }
  double *a = new double[ncmt+oral0];
  double *r = new double[1+oral0];
  double *asave = new double[ncmt+oral0];
  lc.setPtr(a, r, asave);
  lc.setAlast(alast0);
  lc.setRate(rate.data());
  lc.setDt(dt);
  Eigen::Matrix<double, 1, 1> ret = lc(theta);
  NumericVector Alast(ncmt+oral0);
  for (int i = 0; i < ncmt+oral0; i++) {
    Alast[i] = asave[i];
  }
  List retList = List::create(_["val"] = wrap(ret),
                              _["Alast"] = Alast);
  delete[] a;
  delete[] r;
  delete[] asave;
  return retList;
}
