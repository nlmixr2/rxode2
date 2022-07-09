//#undef NDEBUG
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
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

typedef struct stanLl {
  Eigen::VectorXd fx;
  Eigen::Matrix<double, -1, -1> J;
} stanLl;
