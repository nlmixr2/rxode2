#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#define _(String) (String)
#include <stan/math.hpp>
#include "solComp.h"

#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern "C" SEXP _rxode2_solComp2(SEXP k10S, SEXP k12S, SEXP k21S) {
BEGIN_RCPP
  double k10 = REAL(k10S)[0];
  double k12 = REAL(k12S)[0];
  double k21 = REAL(k21S)[0];

  stan::math::solComp2struct<double> ret = stan::math::computeSolComp2(k10, k12, k21, 0.0);
  Rcpp::List retL(3);
  retL[0] = Rcpp::wrap(ret.L);
  retL[1] = Rcpp::wrap(ret.C1);
  retL[2] = Rcpp::wrap(ret.C2);
  retL.attr("names") = Rcpp::CharacterVector::create("L", "C1", "C2");
  return retL;
END_RCPP
}


extern "C" SEXP _rxode2_solComp3(SEXP k10S, SEXP k12S, SEXP k21S,
                                 SEXP k13S, SEXP k31S) {
  BEGIN_RCPP
  double k10 = REAL(k10S)[0];
  double k12 = REAL(k12S)[0];
  double k21 = REAL(k21S)[0];
  double k13 = REAL(k13S)[0];
  double k31 = REAL(k31S)[0];

  stan::math::solComp3struct<double> ret = stan::math::computeSolComp3(k10, k12, k21, k13, k31);
  Rcpp::List retL(4);
  retL[0] = Rcpp::wrap(ret.L);
  retL[1] = Rcpp::wrap(ret.C1);
  retL[2] = Rcpp::wrap(ret.C2);
  retL[3] = Rcpp::wrap(ret.C3);
  retL.attr("names") = Rcpp::CharacterVector::create("L", "C1", "C2", "C3");
  return retL;
  END_RCPP
}
