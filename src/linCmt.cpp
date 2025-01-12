#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "linCmt.h"

// Create linear compartment models for testing

using namespace Rcpp;

// [[Rcpp::export]]
RObject linCmtModelDouble(double p1, double v1, double p2,
                          double p3, double p4, double p5,
                          double ka,
                          const int ncmt, const int oral0, const int trans) {
  stan::math::linCmtStan lc(ncmt, oral0, trans);
  Eigen::Matrix<double, -1, 1> theta;
  if (ncmt == 1) {
    if (oral0 == 1) {
      theta.resize(3);
      theta << p1, v1, ka;
    } else {
      theta.resize(2);
      theta << p1, v1;
    }
  } else if (ncmt == 2) {
    if (oral0 == 1) {
      theta.resize(5);
      theta << p1, v1, p2, p3, ka;
    } else {
      theta.resize(4);
      theta << p1, v1, p2, p3;
    }
  } else if (ncmt == 3) {
    if (oral0 == 1) {
      theta.resize(7);
      theta << p1, v1, p2, p3, p4, p5, ka;
    } else {
      theta.resize(6);
      theta << p1, v1, p2, p3, p4, p5;
    }
  } else {
    stop("Invalid number of compartments");
  }
}
